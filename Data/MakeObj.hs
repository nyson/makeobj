module Data.MakeObj
  ( Defs(..), Error, TypeLabel(..)
  , GenerateTree(..)
  , GenerateList(..)
  , parseGenerateTree, parseDefs, Error.errorBundlePretty
  , pp, pprint, jsonStructure, jsonStructureEquality
  , generateObj, generateList, mkTypeLabel, genTree
  , tryParser
  , generateJson
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

import Data.Aeson (Value)
import Test.QuickCheck (generate)
import Text.Megaparsec.Error(errorBundlePretty)


generateJson :: String -> String -> IO (Either String Value)
generateJson typeInput defsInput = case parseGenerateTree typeInput of
  Left err -> return $ Left ("Bad structure parse:\n" ++ errorBundlePretty err)
  Right parsedJsonType -> case parseDefs defsInput of
    Left err -> return $ Left ("Bad definitions parse:\n" ++ errorBundlePretty err)
    Right definitions -> Right <$> generate (generateObj definitions parsedJsonType)

tryParser :: (PP t, Show t) => Parser t -> String -> IO ()
tryParser p input = case parse p "stdin" input of
  Left err -> do
    putStrLn "--==## ERROR ##==--"
    putStrLn $ Error.errorBundlePretty err
  Right v -> do
    putStrLn "--==## SUCCESS ##==--"
    putStrLn $ pp v
    print v
