{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}
module MakeTest where

import qualified Data.List as List

import Test.Tasty.HUnit (assertEqual, Assertion, assertBool)
import Data.MakeObj.AST
-- import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Text.Megaparsec.Error (errorBundlePretty, ParseErrorBundle)
-- mport Data.Aeson (Value(..))
import Data.MakeObj( jsonStructure, generateObj, parseGenerateTree
                   , parseDefs, jsonStructureEquality, pp, pprint, Defs(..)
                   , GenerateList(..), generateList)
import Text.RawString.QQ (r)
import Test.QuickCheck

import Data.Void


indentLines :: Int -> String -> String
indentLines i = unlines . map (++ replicate i ' ') . lines

prettyCounterexample :: Defs -> Either (ParseErrorBundle String Void) Defs -> String
prettyCounterexample actual generated = concat
    [ "START Defs: \n" , indentLines 2 $ pp actual
    , "\nEND Defs\n\nSTART Parsed: \n" , either errorBundlePretty pp generated
    , "\nEND Parsed"
    , "\nSTART Not equal definitions: "
    , indentLines 20 $ case generated of
        Right g -> concatMap ("\n" ++) $ flip map (notEqualSets (unDefs actual) (unDefs g)) $ \case
          (typeLabel, Nothing, Nothing) -> pp typeLabel ++ ": doesn't have any values wtf"
          (typeLabel, Just a, Nothing) -> pp typeLabel ++ ": has lost the value " ++ pp a ++ " in parsing"
          (typeLabel, Nothing, Just b) -> pp typeLabel ++ ": has gained the value " ++ pp b ++ " in parsing"
          (typeLabel, Just a, Just b) -> pp typeLabel ++ ":"
            ++ indentLines 4 ("\nBefore parse:  \n" ++ pp a ++ "\nAfter parse \n" ++ pp b)
        Left _e -> "Failed parse, ignoring..."
    ]

notEqualSets :: forall key value. (Eq key, Eq value)
          => [(key, value)] -> [(key, value)] -> [(key, Maybe value, Maybe value)]
notEqualSets as bs = foldl foldf [] as ++ onlyInA ++ onlyInB
  where onlyInA = map (\(key, value) -> (key, Just value, Nothing))
          $ List.deleteFirstsBy (\a b -> fst a == fst b) as bs
        onlyInB = map (\(key, value) -> (key, Nothing, Just value))
          $ List.deleteFirstsBy (\a b -> fst a == fst b) bs as
        foldf out (key, value) = case lookup key bs of
          Just bval | bval /= value -> (key, Just value, Just bval):out
          _ -> out


prop_circle :: Defs -> Property
prop_circle defs = counterexample (prettyCounterexample defs parsed)
                   $ parsed === Right defs
  where parsed = parseDefs (pp defs)

unit_parseFloatRange :: Assertion
unit_parseFloatRange = assertEqual "can parse float ranges"
  (Right (GRange (FloatRange 1.10 1.12)))
  (parseGenerateTree [r|1.10 to 1.12|])

unit_parseIntRange :: Assertion
unit_parseIntRange = assertEqual "can parse int ranges"
  (Right (GRange (IntRange 1 2)))
  (parseGenerateTree [r|1 to 2|])

unit_literalBool :: Assertion
unit_literalBool = assertEqual "can consume literal bools"
  (Right (GLiteral (LBool True)))
  (parseGenerateTree [r|true|])

unit_literalNull :: Assertion
unit_literalNull = assertEqual "can consume literal null"
  (Right (GLiteral LNull))
  (parseGenerateTree [r|null|])

unit_literalNumber :: Assertion
unit_literalNumber = assertEqual "can consume literal numbers"
  (Right (GLiteral (LNumber 10.20)))
  (parseGenerateTree "10.20")

unit_literalString :: Assertion
unit_literalString = assertEqual "can consume literal strings"
  (Right (GLiteral (LString "Hej")))
  (parseGenerateTree [r|"Hej"|])

unit_literalArray :: Assertion
unit_literalArray = assertEqual "can comsume literal arrays"
  (Right (GList (LiteralList [GLiteral $ LString "Hej"])))
  (parseGenerateTree [r|["Hej"]|])

unit_literalObject :: Assertion
unit_literalObject = assertEqual "can comsume literal objects"
  expected
  (parseGenerateTree [r|{hej: "Hej"}|])
  where
    expected = Right . GObj . HM.fromList $ [("hej", GLiteral (LString "Hej"))]

prop_listLength :: GenerateList -> Defs -> Property
prop_listLength gl defs = counterexample (pp gl) $ case gl of
  Unbounded _ -> label "List with any length"
    $ forAll mkList
    $ const True
  ListOf n  _ -> label "List with exact length"
    $ forAll mkList
    $ \l -> length l === n
  RangedList minLen maxLen _ -> label "List with ranged length"
    $ forAll mkList
    $ \l -> let len = length l
            in minLen <= len && len <= maxLen
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
