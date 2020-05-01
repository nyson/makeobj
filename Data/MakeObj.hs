module Data.MakeObj (
  Defs(..), Error, TypeLabel(..),
  GenerateTree(..),
  GenerateList(..),
  parseGenerateTree, parseDefs,
  pp, pprint, jsonStructure, jsonStructureEquality,
  generateObj, generateList, mkTypeLabel, genTree,
  tryParser
  ) where

import Data.MakeObj.AST (
  GenerateTree(..),
  GenerateList(..),
  Defs(..),
  TypeLabel(..),
  mkTypeLabel,
  genTree)

import qualified Text.Megaparsec.Error as Error
import Text.Megaparsec (parse)

import Data.MakeObj.GenerateObj (generateObj, generateList)
import Data.MakeObj.PP (PP(..), pprint)
import Data.MakeObj.Parser hiding (rx)
import Data.MakeObj.Parser.Shared (Error, Parser)
import Data.MakeObj.TreeEquality (jsonStructure, jsonStructureEquality)

tryParser :: PP t => Parser t -> String -> IO ()
tryParser p input = case parse p "stdin" input of
  Left err -> do
    putStrLn "--==## ERROR ##==--"
    putStrLn $ Error.errorBundlePretty err
  Right v -> do
    putStrLn "--==## SUCCESS ##==--"
    putStrLn $ pp v
