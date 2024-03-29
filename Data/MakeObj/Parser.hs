module Data.MakeObj.Parser where

import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Text.Reggie (Regex)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import qualified Text.Reggie as Rx
import qualified Data.HashMap.Strict as HM
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as M
import qualified Data.Text as T

import Data.MakeObj.Parser.Time (timeLiteral)
import Data.MakeObj.AST
import Data.MakeObj.Parser.Shared

parseGenerateTree :: String -> Either Error GenerateTree
parseGenerateTree = parse (tree <* eof) ""

parseDefs :: String -> Either Error Defs
parseDefs = parse (defs <* eof) ""

defs :: Parser Defs
defs = Defs <$> label "Defs Config" (many def)
  where def = (,)
              <$> sc typeLabel <* sc (char '=')
              <*> sc tree

tree :: Parser GenerateTree
tree = label "GenerateTree" $
  choice [ try $ GObj <$> object
         , try $ GList <$> list
         , try $ GRange <$> range
         , GRx <$> sc rx
         , GType <$> typeLabel
         , GLiteral <$> value
         ]

interleavedParser :: Parser b -> Parser a -> Parser [a]
interleavedParser interleaver parser = choice
  [ (:) <$> sc parser <*> many (sc interleaver *> sc parser)
  , return []
  ]

value :: Parser Literal
value = label "Value Parser"
  $ choice [ try $ LTime <$> timeLiteral
           , try $ LNumber <$> L.signed spaceParser L.scientific
           , LBool <$> sc bool
           , LNull <$ sc (chars "null")
           , LString <$> sc limitedString
           ]
  where bool = choice
              [ chars "true" $> True
              , chars "false" $> False
              ]
        limitedString = T.pack <$>
          sc (char '"' *> many (M.satisfy (/= '"')) <* char '"')

rxChar :: Parser Char
rxChar = label "Char" . choice $ letterChar
         : map char "+, ?:.*{}[]|()-1234567890-^"

-- | Parses a range (an inclusive Int to Int generator)
range :: Parser Range
range = choice
  [ try . label "Date Range" $ DateRange
    <$> timeLiteral <* sc (chars "to")
    <*> timeLiteral
  , try . label "Int Range" $ IntRange
    <$> int <* sc (chars "to")
    <*> int
  , try . label "Float Range" $ FloatRange
    <$> num <* sc (chars "to")
    <*> num
  ]

rx :: Parser Regex
rx = label "Regex Parser" . try
  $ either error return =<< Rx.parse
  <$> (char '/' *> many rxChar <* char '/')

typeLabel :: Parser TypeLabel
typeLabel = label "TypeLabel" $ TypeLabel . T.pack <$> capitalizedString
  where capitalizedString = (:) <$> upperChar <*> many letterChar

list :: Parser GenerateList
list = label "List" $ choice
          [ try $ Unbounded
            <$> (sc (chars "list") *> _of *> sc tree)
          , try $ ListOf <$> (sc int <* _of) <*> tree'
          , RangedList
            <$> (sc int <* sc (chars "to"))
            <*> (sc int <* _of)
            <*> tree'
          , LiteralList <$> spaceWrapped ('[', ']') (interleavedParser (char ',') tree)
          ]
          where tree' = sc tree
                _of = sc (chars "of")

object :: Parser (HashMap Text GenerateTree)
object = label "Object" $ HM.fromList
         <$> spaceWrapped ('{', '}') (sc mkMapList)
  where
    mkMapList = (:)
      <$> sc mkMapItem
      <*> many (sc (char ',') *> sc mkMapItem)
    mkMapItem = (,)
      <$> (T.pack <$> mkKey)
      <*> (sc (char ':') *> tree)
    mkKey = choice 
      [ char '"' *> many (M.satisfy (/= '"')) <* char '"'
      , many (letterChar <|> digitChar) 
      ]
