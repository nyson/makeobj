{-# LANGUAGE TypeApplications #-}
module Data.MakeObj.Parser where

import Data.Void (Void)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Text.Reggie (Regex)
import Data.Functor (($>))
import Data.Scientific (Scientific, scientific)
import Data.Aeson (Value(..))
import qualified Text.Reggie as Rx hiding (Null)
import qualified Data.HashMap.Strict as HM
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.MakeObj.AST

type Parser = Parsec Void String
type Error = ParseErrorBundle String Void

sc :: Parser t -> Parser t
sc p = spaceParser *> p <* spaceParser

spaceParser :: Parser ()
spaceParser = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

parseGenerateTree :: String -> Either Error GenerateTree
parseGenerateTree = parse tree ""

parseDefs :: String -> Either Error Defs
parseDefs = parse defs ""

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

spaceWrapped :: (Char, Char) -> Parser b -> Parser b
spaceWrapped (c1, c2) parser = schar c1 *> parser <* schar c2
    where schar = sc . char

value :: Parser Value
value = label "Value Parser"
  $ choice [ try $ Number <$> L.signed spaceParser L.scientific
               , Bool <$> sc bool
               , Null <$ sc (chars "null")
               , String <$> sc limitedString
               ]
  where bool = choice
              [ chars "true" $> True 
              , chars "false" $> False
              ]
        limitedString = T.pack <$> 
          sc (char '"' *> many letterChar <* char '"')

chars :: String -> Parser ()
chars = foldr ((>>) . char) mempty

rxChar :: Parser Char
rxChar = label "Char" $ choice $ letterChar
         : map char "+, ?:.*{}[]|()-1234567890-^"

-- | Parses a range (an inclusive Int to Int generator)
range :: Parser (Range Int)
range = label "Int Range" $ Range
  <$> num <* sc (chars "to")
  <*> num

num :: (Integral n, Num n) => Parser n
num = L.signed spaceParser L.decimal

rx :: Parser Regex
rx = label "Regex Parser" $ try $ do
  res <- Rx.parse <$> (char '/' *> many rxChar <* char '/')
  case res of
    Left err -> error "wtf"
    Right rx -> return rx

typeLabel :: Parser TypeLabel
typeLabel = label "TypeLabel" $ TypeLabel . T.pack <$> capitalizedString
  where capitalizedString = (:) <$> upperChar <*> many letterChar

list :: Parser GenerateList
list = label "List" $ choice
          [ try $ Unbounded 
            <$> (sc (chars "list") *> _of *> sc tree)
          , try $ ListOf <$> (sc num <* _of) <*> tree'
          , RangedList
            <$> (sc num <* sc (chars "to"))
            <*> (sc num <* _of)
            <*> tree'
          , LiteralList <$> spaceWrapped ('[', ']') (interleavedParser (char ',') tree)
          
          ]
          where tree' = sc tree
                _of = sc (chars "of")

object :: Parser (HashMap Text GenerateTree)
object = label "Object" $ HM.fromList
         <$> (sc (char '{') *> sc mkMapList <* sc ( char '}'))
  where
    mkMapList = (:) <$> sc mkMapItem <*> many (sc (char ',') *> sc mkMapItem)
    mkMapItem = (,)
      <$> (T.pack <$> many letterChar)
      <*> (sc (char ':') *> tree)
