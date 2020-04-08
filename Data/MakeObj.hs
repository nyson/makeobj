module Data.MakeObj (
  Defs(..), Error(..), TypeLabel(..),
  GenerateTree(..),
  GenerateList(..),
  parseGenerateTree, parseDefs,
  pp, pprint, jsonStructure, jsonStructureEquality,
  generateObj, generateList, mkTypeLabel, genTree
  ) where

import Control.Monad (replicateM)
import Data.MakeObj.AST (
  GenerateTree(..),
  GenerateList(..),
  Defs(..),
  TypeLabel(..),
  mkTypeLabel,
  genTree)
import Data.MakeObj.GenerateObj (generateObj, generateList)
import Data.MakeObj.PP (pp, pprint)
import Data.MakeObj.Parser hiding (rx)
import Data.MakeObj.Parser.Shared (Error(..))
import Data.MakeObj.TreeEquality (jsonStructure, jsonStructureEquality)
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
