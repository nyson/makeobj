{-# LANGUAGE LambdaCase #-}
module MakeTest where

import Test.Tasty.HUnit (assertEqual, Assertion, assertBool)
import Data.MakeObj(randomCar, toStructure, generateObj, parseGenerateTree, parseDefs, jsonTreeEquality, pp, pprint, Defs(..))
import Test.QuickCheck


prop_circle :: Defs -> Property
prop_circle defs
  = let ppdefs = pp defs
        parsed = parseDefs (pp defs)
    in counterexample
       ( "START Defs: \n" ++ ppdefs
         ++ "\nEND Defs\nSTART Parsed: " ++ either show pp parsed
         ++ "\nEND Parsed")
       $ parsed === Right defs

unit_trivial :: Assertion
unit_trivial = assertEqual "Sanity check" 1 1

isRight :: Either a b -> Bool
isRight = \case
  Right _ -> True
  _ -> False

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

unit_canParseTree :: Assertion
unit_canParseTree = assertBool "can parse tree"
  $ isRight (parseGenerateTree treeDef)


unit_canCreateCar :: Assertion
unit_canCreateCar = do
  c1 <- randomCar
  c2 <- do

    tree <- case parseGenerateTree treeDef of
      Left e -> error $ "bad tree: ("++ show e ++ ")\n" ++ pp treeDef
      Right t -> return t

    generate $ generateObj [] tree

  assertBool "structural equality" $ jsonTreeEquality c1 c2

