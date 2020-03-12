{-# LANGUAGE LambdaCase, GADTs, FlexibleInstances, TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.MakeObj.AST where

import Data.Char (toUpper)
import Data.HashMap.Strict (HashMap)
import Data.List (intercalate)
import Data.MakeObj.PP (PP(..))
import Data.Set (Set)
import Data.Text (Text)
import Test.QuickCheck (Arbitrary(..), Gen, listOf1, scale, elements, frequency)
import Text.Reggie (Regex, genRegex, GenRegexOpts(..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Reggie as Reggie


simpleString :: Gen String
simpleString = listOf1 (elements ['a'..'z'])

simpleText :: Gen Text
simpleText = T.pack <$> simpleString

upperCase :: Gen Text
upperCase = transform . T.uncons <$> simpleText
  where transform :: Maybe (Char, Text) -> Text
        transform = \case
          Just (c, rest) -> toUpper c `T.cons` rest
          Nothing -> T.empty

newtype TypeLabel = TypeLabel Text
  deriving (Show, Eq, Ord)

instance Arbitrary TypeLabel where
  arbitrary = TypeLabel <$> upperCase

mkTypeLabel :: String -> TypeLabel
mkTypeLabel = TypeLabel . T.pack

instance PP TypeLabel where
  pp (TypeLabel s) = T.unpack s

data Range t where
  Range :: (Show t, Eq t, Num t) => t -> t -> Range t

deriving instance Show t => Show (Range t)
deriving instance Eq t => Eq (Range t)

instance Arbitrary (Range Int) where
  arbitrary = do
    smaller <- arbitrary
    Range smaller . (smaller +) . abs <$> arbitrary

data GenerateTree
  = GRx Regex
  | GRange (Range Int)
  | GType TypeLabel
  | GList GenerateList
  | GObj (HashMap Text GenerateTree)
  deriving (Show, Eq)

data GenerateList
  = Unbounded GenerateTree
  | ListOf Int GenerateTree
  | RangedList Int Int GenerateTree
  deriving (Show, Eq)

instance Arbitrary GenerateTree where
  arbitrary = (Set.fromList <$> listOf1 arbitrary) >>= genTree

genTree :: Set TypeLabel -> Gen GenerateTree
genTree defs = frequency
    [ (10, GRx <$> genRegex GROpts {grAllowEmpty= False})
    , (5,  GRange <$> arbitrary)
    , (10, GType <$> elements defList)
    , (2, GList <$> genList defs)
    , (2, GObj . HashMap.fromList
          <$> scale (`div` 2) (listOf1 (objectProducer defs)))
    ]
    where objectProducer = (,)
            <$> simpleText
            <*> scale (`div` 2) (genTree defs)
          defList = Set.toList defs

genList :: Set TypeLabel -> Gen GenerateList
genList defs = oneof
  [ Unbounded <$> genTree defs
  , ListOf <$> fmap abs arbitrary <*> genTree defs
  , do Range min max <- arbitrary @(Range Int)
       RangedList min max <$> genTree defs
  ]

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

newtype Defs = Defs { unDefs :: [(TypeLabel, GenerateTree)] }
  deriving (Show, Eq, Semigroup, Monoid)

instance PP Defs where
  pp = concatMap (\(tl, gt) -> pp tl ++ " = " ++ pp gt ++ "\n") . unDefs

instance Arbitrary Defs where
  arbitrary = do
    lbls <- (:) <$> arbitrary @TypeLabel <*> listOf1 (arbitrary @TypeLabel)
    let lblSet = Set.fromList lbls
    Defs <$> mapM (\x -> (x,) <$> genTree (Set.delete x lblSet)) lbls

instance PP GenerateTree where
  pp = pretty 0
    where pretty :: Int -> GenerateTree -> String
          pretty _ (GRx rx) = "/" ++ Reggie.pp rx ++ "/"
          pretty _ (GType tl) = pp tl
          pretty _ (GRange (Range a b)) = pp a ++ " to " ++ pp b
          pretty i (GList ls) = concat ["[ ", prettyList (i+1) ls ," ]"]
          pretty i (GObj mp)= concat
            [ "{\n"
            , intercalate ",\n" $ map
              (\(k,v) -> concat [ tabs (i+1)
                                , T.unpack k
                                , ": "
                                , pretty (i+1) v
                                ])
              (HashMap.toList mp)
            , "\n"
            , tabs i
            , "}"
            ]

          prettyList :: Int -> GenerateList -> String
          prettyList i = (\s -> '[':s ++ "]") . \case
            Unbounded tree -> pretty i tree
            ListOf len tree ->  show len ++ " of " ++ pretty i tree
            RangedList min max tree -> show min ++ " to " ++ show max
                                       ++ " of " ++ pretty i tree
          tabs :: Int -> String
          tabs n = replicate (n*4) ' '
