{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module TimeTest where

import Test.Tasty.HUnit (assertEqual, Assertion, assertBool)
import Data.MakeObj.AST
import Data.MakeObj.PP
import Data.MakeObj.Parser.Time (timeLiteral)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Aeson (Value(..))
-- import Data.MakeObj( jsonStructure, generateObj, parseGenerateTree
--                    , parseDefs, jsonStructureEquality, pp, pprint, Defs(..)
--                    , GenerateList(..), generateList)
import Data.MakeObj.AST.Time
import Text.RawString.QQ (r)
import Test.QuickCheck

prop_timeliteral :: TimeLiteral -> Property
prop_timeliteral tl@(TimeLiteral t dg tg) 
  = counterexample timestring $ (pp <$> Right tl) === (pp <$> parse timeLiteral "" (pp tl))
  where timestring = concat 
            [ "Time: ", show t
            , "\nDayGranularity: ", show dg
            , "\nTimeGranularity: ", show tg
            , "\npp: ", pp (TimeLiteral t dg tg)
            , "\nparsed: ", either errorBundlePretty pp (parse timeLiteral "" (pp tl))
            ]


-- unit_literalBool = assertEqual "can consume literal bools"
--   (Right (GLiteral (Bool True))) 
--   (parseGenerateTree [r|true|])

