{-# LANGUAGE LambdaCase, GADTs, FlexibleInstances, TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications #-}
module Data.MakeObj.AST where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.MakeObj.PP
import Data.Text (Text)
import qualified Data.Text as T
import Text.Reggie (Regex, genRegex, GenRegexOpts(..))
import qualified Text.Reggie as Reggie
import Data.List (intercalate)
import Test.QuickCheck
import Data.Char (toUpper)
import qualified Data.Set as Set
import Data.Set (Set)


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
  Range :: Num t => t -> t -> Range t

instance Arbitrary (Range Int) where
  arbitrary = do
    smaller <- arbitrary
    Range smaller . (smaller +) . abs <$> arbitrary

instance Show t => Show (Range t) where
  show (Range a b) = concat ["Range " ++ show a ++ show b]

instance Eq t => Eq (Range t) where
  (Range a b) == (Range a' b') = a == a' && b == b'

data GenerateTree
  = GRx Regex
  | GRange (Range Int)
  | GType TypeLabel
  | GList GenerateTree
  | GObj (HashMap Text GenerateTree)
  deriving (Show, Eq)

instance Arbitrary GenerateTree where
  arbitrary = frequency
    [ (10, GRx <$> genRegex GROpts {grAllowEmpty= False})
    , (5,  GRange <$> arbitrary)
    , (2, GList <$> arbitrary)
    , (1, GObj . HashMap.fromList
          <$> scale (`div` 2) (listOf1 objectProducer))
    ]
    where objectProducer :: Arbitrary a => Gen (Text, a)
          objectProducer = (,) <$> simpleText <*> arbitrary

genTree :: Set TypeLabel -> Gen GenerateTree
genTree defs = frequency
    [ (10, GRx <$> genRegex GROpts {grAllowEmpty= False})
    , (5,  GRange <$> arbitrary)
    , (20, GType <$> elements defList)
    , (2, GList <$> genTree defs)
    , (1, GObj . HashMap.fromList
          <$> scale (`div` 2) (listOf1 (objectProducer defs)))
    ]
    where objectProducer :: Set TypeLabel -> Gen (Text, GenerateTree)
          objectProducer defs = (,) <$> simpleText <*> genTree defs
          defList = Set.toList defs

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
          pretty _ (GList (GType ls)) = concat ["[ ", pp ls, " ]"]
          pretty i (GList inner@(GList _)) = concat
            [ "\n", tabs i, "["
            , pretty (i+1) inner
            , "\n", tabs i, "]"]
          pretty i (GList ls) = concat ["[ ", pretty (i+1) ls ," ]"]
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
          tabs :: Int -> String
          tabs n = replicate (n*4) ' '
