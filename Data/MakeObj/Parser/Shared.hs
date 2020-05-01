module Data.MakeObj.Parser.Shared 
  ( Parser, Error, try, char, many, some, choice
  , sc, spaceParser, int, float, chars, spaceWrapped
  , digitChar, letterChar, label, upperChar
  , parse, nOf
  ) where

import Prelude hiding (fail)
import Data.Void (Void)
import Text.Megaparsec (Parsec, ParseErrorBundle, try, choice, label, parse)
import Control.Applicative (many, some)
import Control.Monad (replicateM)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type Error = ParseErrorBundle String Void

sc :: Parser t -> Parser t
sc p = spaceParser *> p <* spaceParser

spaceParser :: Parser ()
spaceParser = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

int :: Parser Int
int = L.signed spaceParser L.decimal

float :: Parser Double
float  = L.signed spaceParser L.float

chars :: String -> Parser ()
chars = foldr ((>>) . char) mempty

spaceWrapped :: (Char, Char) -> Parser b -> Parser b
spaceWrapped (c1, c2) parser = schar c1 *> parser <* schar c2
    where schar = sc . char

nOf :: Parser a -> Int -> Parser [a]
nOf = flip replicateM
