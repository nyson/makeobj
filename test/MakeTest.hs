{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module MakeTest where

import Test.Tasty.HUnit (assertEqual, Assertion, assertBool)
import Data.MakeObj.AST
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Aeson (Value(..))
import Data.MakeObj( jsonStructure, generateObj, parseGenerateTree
                   , parseDefs, jsonStructureEquality, pp, pprint, Defs(..)
                   , GenerateList(..), generateList)
import Text.RawString.QQ (r)
import Test.QuickCheck


prop_circle :: Defs -> Property
prop_circle defs
  = let ppdefs = pp defs
        parsed = parseDefs (pp defs)
    in counterexample
       ( "START Defs: \n" ++ ppdefs
         ++ "\nEND Defs\n\nSTART Parsed: \n" ++ either show pp parsed
         ++ "\nEND Parsed")
       $ parsed === Right defs

unit_literalBool = assertEqual "can consume literal bools"
  (Right (GLiteral (LBool True))) 
  (parseGenerateTree [r|true|])

unit_literalNull = assertEqual "can consume literal null"
  (Right (GLiteral LNull)) 
  (parseGenerateTree [r|null|])

unit_literalNumber = assertEqual "can consume literal numbers"
  (Right (GLiteral (LNumber 10.20))) 
  (parseGenerateTree "10.20")

unit_literalString = assertEqual "can consume literal strings"
  (Right (GLiteral (LString "Hej"))) 
  (parseGenerateTree [r|"Hej"|])

unit_literalArray = assertEqual "can comsume literal arrays"
  (Right (GList (LiteralList [GLiteral $ LString "Hej"]))) 
  (parseGenerateTree [r|["Hej"]|])

unit_literalObject = assertEqual "can comsume literal objects"
  expected
  (parseGenerateTree [r|{hej: "Hej"}|])
  where
    expected = Right . GObj . HM.fromList $ [("hej", GLiteral (LString "Hej"))]

prop_listLength :: GenerateList -> Defs -> Property
prop_listLength gl defs = counterexample (pp gl) $case gl of
  Unbounded _ -> label "List with any length"
    $ forAll mkList
    $ const True
  ListOf n  _ -> label "List with exact length"
    $ forAll mkList
    $ \l -> length l === n
  RangedList min max _ -> label "List with ranged length"
    $ forAll mkList
    $ \l -> let len = length l
            in min <= len && len <= max
  LiteralList ls -> label "Literal list"
    $ forAll mkList 
    $ \l -> length l == length ls
  where mkList = generateList defs gl

isRight :: Either a b -> Bool
isRight = \case Right _ -> True; _ -> False

treeDef :: String
treeDef = "{color: /black/,"
          ++ "marketId: /[a-z]{2}/,"
          ++ "year: 2015 to 2025, "
          ++ "model: /PS[12]/, "
          ++ "package: /performance/, "
          ++ "localIdentifier: /[A-Z]{3} [1-9][0-9]{2}/ ,"
          ++ "estimatedArrivalDate: /date{3}/, "
          ++ "vin: /[a-z12345]{5}-[a-z12345]{5}-[a-z12345]{5}-[a-z12345]{5}/"
          ++ "}"

assertRight :: String -> Either a b -> Assertion
assertRight lbl = assertBool lbl . \case
  Right _ -> True
  Left _ -> False

unit_canParseTree :: Assertion
unit_canParseTree = assertRight "can parse tree" $ parseGenerateTree treeDef
