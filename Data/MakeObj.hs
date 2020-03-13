{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveGeneric, GeneralizedNewtypeDeriving, LambdaCase #-}
{-# LANGUAGE FlexibleInstances, GADTs #-}


module Data.MakeObj (
  Defs(..), Error(..), TypeLabel(..),
  GenerateList(..),
  parseGenerateTree, parseDefs,
  pp, pprint, toStructure, jsonTreeEquality,
  generateObj, generateList, mkTypeLabel, genTree
  ) where

import Control.Monad (replicateM)
import Data.MakeObj.AST
  (GenerateList(..), Defs(..), TypeLabel(..), mkTypeLabel, genTree)
import Data.MakeObj.GenerateObj (generateObj, generateList)
import Data.MakeObj.PP (pp, pprint)
import Data.MakeObj.Parser hiding (rx)
import Data.MakeObj.TreeEquality (jsonTreeEquality, toStructure)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
import Text.Reggie (rx, rxGen)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T
