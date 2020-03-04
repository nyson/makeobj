module Data.MakeObj.AST where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.MakeObj.PP
import Data.Text (Text)
import qualified Data.Text as T
import Text.Reggie (Regex)
import Data.List (intercalate)

newtype TypeLabel = TypeLabel String
  deriving (Show, Eq, Ord)

instance PP TypeLabel where
  pp (TypeLabel s) = s

data GenerateTree
  = GRx Regex
  | GType TypeLabel
  | GList GenerateTree
  | GObj (HashMap Text GenerateTree)
  deriving Show

pretty :: Int -> GenerateTree -> String
pretty _ (GRx rx) = show rx
pretty _ (GType tl) = pp tl
pretty _ (GList (GType ls)) = concat ["[", pp ls, "]"]
pretty i (GList ls) = concat ["[\n", pretty (i+1) ls ,"\n]"]
pretty i (GObj mp)= concat
  [ "{\n"
  , intercalate ",\n" $ map
    (\(k,v) -> concat [ tabs (i+1)
                      , T.unpack k
                      , ": "
                      , pretty (i+1) v
                      ])
    (HashMap.toList mp)
  , "\n"
  , tabs i
  , "}"
  ]
  where tabs :: Int -> String
        tabs n = replicate (n*4) ' '

instance PP GenerateTree where
  pp = pretty 0
