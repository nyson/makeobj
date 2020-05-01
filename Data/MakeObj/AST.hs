{-# LANGUAGE LambdaCase, GADTs, FlexibleInstances, TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications #-}

module Data.MakeObj.AST where

import Data.Char (toUpper)
import Data.HashMap.Strict (HashMap)
import Data.List (intercalate)
import Data.Scientific (Scientific, fromFloatDigits)
import Data.MakeObj.PP (PP(..))
import Data.Set (Set)
import Data.Text (Text)
import Test.QuickCheck
  ( Arbitrary(..), Gen, listOf1, scale
  , elements, frequency, oneof )
import Text.Reggie (Regex, genRegex, GenRegexOpts(..))

import Data.MakeObj.AST.Time (TimeLiteral(..), arbitraryDiffTime, addDiff)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Reggie as Reggie


simpleString :: Gen String
simpleString = listOf1 (elements ['a'..'z'])

simpleText :: Gen Text
simpleText = T.pack <$> simpleString

tMapHead :: (Char -> Char) -> Text -> Text
tMapHead f = maybe T.empty (\(c, rest) -> f c `T.cons` rest) . T.uncons

upperCase :: Gen Text
upperCase =  tMapHead toUpper <$> simpleText

newtype TypeLabel = TypeLabel Text
  deriving (Show, Eq, Ord)

instance Arbitrary TypeLabel where
  arbitrary = TypeLabel <$> upperCase

mkTypeLabel :: String -> TypeLabel
mkTypeLabel = TypeLabel . T.pack

instance PP TypeLabel where
  pp (TypeLabel s) = T.unpack s

data Range where
  IntRange   :: Int -> Int -> Range
  FloatRange :: Double -> Double -> Range
  DateRange  :: TimeLiteral -> TimeLiteral -> Range
  deriving (Eq, Show)

instance  PP Range where
  pp = \case
    IntRange a b -> pp a ++ " to " ++ pp b
    FloatRange a b -> pp a ++ " to " ++ pp b
    DateRange a b -> pp a ++ " to " ++ pp b

instance Arbitrary Range where
  arbitrary = oneof
    [ arbRange IntRange
    , arbRange FloatRange
    , do time <- arbitrary
         DateRange time <$> fmap (time `addDiff`) arbitraryDiffTime
    ]
    where arbRange :: (Num t, Arbitrary t) => (t -> t -> Range) -> Gen Range
          arbRange mkRange = do
            smaller <- arbitrary
            mkRange smaller . (smaller +) . abs <$> arbitrary

data Literal
  = LNumber Scientific
  | LBool Bool
  | LNull
  | LString Text
  | LTime TimeLiteral
  deriving (Show, Eq)

instance PP Literal where
  pp (LNumber s) = show s
  pp (LBool b) | b = "true"
               | otherwise = "false"
  pp LNull = "null"
  pp (LString t) = concat ["\"", T.unpack t, "\""]
  pp (LTime time) = pp time


instance Arbitrary Literal where
  arbitrary = oneof
    [ LNumber . fromFloatDigits <$> arbitrary @Double
    , LBool <$> arbitrary
    , pure LNull
    , LString . T.pack <$> listOf1 (elements ['A'..'Z'])
    , LTime <$> arbitrary
    ]

data GenerateTree
  = GRx Regex
  | GRange Range
  | GLiteral Literal
  | GType TypeLabel
  | GList GenerateList
  | GObj (HashMap Text GenerateTree)
  deriving (Show, Eq)

data GenerateList
  = Unbounded GenerateTree
  | ListOf Int GenerateTree
  | RangedList Int Int GenerateTree
  | LiteralList [GenerateTree]
  deriving (Show, Eq)

instance Arbitrary GenerateTree where
  arbitrary = (Set.fromList <$> listOf1 arbitrary) >>= genTree

genTree :: Set TypeLabel -> Gen GenerateTree
genTree defs = frequency $ if defs == Set.empty
                           then otherGens
                           else tlGen:otherGens
    where tlGen = (10, GType <$> elements defList)
          otherGens =
            [ (10, GRx <$> genRegex GROpts {grAllowEmpty= False})
            , (5,  GRange <$> arbitrary)
            , (2,  GLiteral <$> arbitrary)
            , (2,  GList <$> genList defs)
            , (2,  GObj . HashMap.fromList
                <$> scale (`div` 2) (listOf1 objectProducer))
            ]
          objectProducer = (,)
            <$> simpleText
            <*> scale (`div` 2) (genTree defs)
          defList = Set.toList defs

instance Arbitrary GenerateList where
  arbitrary = (Set.fromList <$> listOf1 arbitrary) >>= genList

genList :: Set TypeLabel -> Gen GenerateList
genList defs | defs == Set.empty = error "Empty definition set given!"
genList defs = scale (`div` 2) $ oneof
  [ Unbounded <$> genTree defs
  , ListOf
    <$> fmap abs arbitrary
    <*> genTree defs
  , do baseLength <- fmap abs arbitrary
       RangedList baseLength
         <$> ((baseLength +) <$> fmap abs arbitrary)
         <*> genTree defs
  , LiteralList <$> arbitrary
  ]

newtype Defs = Defs { unDefs :: [(TypeLabel, GenerateTree)] }
  deriving (Show, Eq, Semigroup, Monoid)

instance PP Defs where
  pp = concatMap (\(tl, gt) -> pp tl ++ " = " ++ pp gt ++ "\n") . unDefs

instance Arbitrary Defs where
  arbitrary = do
    lbls <- (:) <$> arbitrary @TypeLabel <*> listOf1 (arbitrary @TypeLabel)
    let lblSet = Set.fromList lbls
    Defs <$> mapM (\x -> (x,) <$> genTree (Set.delete x lblSet)) lbls

instance PP GenerateList where
  pp = prettyList 0

prettyList :: Int -> GenerateList -> String
prettyList i = \case
  Unbounded tree -> "list of " ++ pretty i tree
  ListOf len tree -> concat [show len, " of ", pretty i tree]
  RangedList minLen maxLen tree -> concat
    [ show minLen, " to ", show maxLen
    , " of " , pretty i tree
    ]
  LiteralList ls
    | length ls <= 1 -> concat ["[ ", concatMap (pretty (i+1)) ls, " ]"]
    | otherwise -> concat ["[ ", intercalate "," $ map (( ("\n" ++ tabs i) ++) . pretty (i+1)) ls, "\n", tabs i, "]"]

pretty :: Int -> GenerateTree -> String
pretty _ (GRx rx) = "/" ++ Reggie.pp rx ++ "/"
pretty _ (GLiteral v) = pp v
pretty _ (GType tl) = pp tl
pretty _ (GRange r) = pp r
pretty i (GList ls) = prettyList (i+1) ls
pretty i (GObj mp)= concat
  [ "{\n"
  , intercalate ",\n" $ map displayKeyValue (HashMap.toList mp)
  , "\n"
  , tabs i, "}"
  ]
  where displayKeyValue (key, value) = concat
          [ tabs (i+1)
          , T.unpack key
          , ": "
          , pretty (i+1) value
          ]


tabs :: Int -> String
tabs n = replicate (n*4) ' '

instance PP GenerateTree where
  pp = pretty 0
