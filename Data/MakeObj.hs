{-# LANGUAGE LambdaCase #-}
module Data.MakeObj
  ( Defs(..), Error, TypeLabel(..)
  , GenerateTree(..)
  , GenerateList(..)
  , parseGenerateTree, parseDefs, Error.errorBundlePretty
  , pp, pprint, jsonStructure, jsonStructureEquality
  , generateObj, generateList, mkTypeLabel, genTree
  , tryParser
  , generateBalancedJson, generateBalancedJsonWithMeta
  , ObjectPackage(..)
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
import Data.MakeObj.CountNodes (Meta, meta, balanceUnbounded, inlineDefinitions)
import Data.Bifunctor (first)
import Data.Aeson (Value)
import Test.QuickCheck (generate)
import Text.Megaparsec.Error(errorBundlePretty)



data ObjectPackage = ObjectPackage
  { generatedCode :: Value
  , metadata :: Meta
  }


generateBalancedJson :: Int -> String -> String -> IO (Either String Value)
generateBalancedJson maxSize typeInput defsInput = fmap generatedCode
  <$> generateBalancedJsonWithMeta maxSize typeInput defsInput

generateBalancedJsonWithMeta  :: Int -> String -> String -> IO (Either String ObjectPackage)
generateBalancedJsonWithMeta maxSize typeInput defsInput
  = case generateAndBalance maxSize typeInput defsInput of
      Right (t, m) -> Right . flip ObjectPackage m
        <$> generate (generateObj (Defs []) t)
      Left err -> return $ Left err
  where
    generateAndBalance maxNodes mainDefIn defsIn = do
      p <- first errorBundlePretty $ parseGenerateTree mainDefIn
      d <- first errorBundlePretty $ parseDefs defsIn
      t' <- inlineDefinitions d p
      let m = meta t'
      t'' <- balanceUnbounded maxNodes m t'
      return (t'', m)



tryParser :: (PP t, Show t) => Parser t -> String -> IO ()
tryParser p input = case parse p "stdin" input of
  Left err -> do
    putStrLn "--==## ERROR ##==--"
    putStrLn $ Error.errorBundlePretty err
  Right v -> do
    putStrLn "--==## SUCCESS ##==--"
    putStrLn $ pp v
    print v
