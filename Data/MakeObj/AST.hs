{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
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

instance Show t => Show (Range t)
instance Eq t => Eq (Range t)

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
    , (20, GType <$> arbitrary)
    , (2, GList <$> arbitrary)
    , (1, GObj . HashMap.fromList
          <$> scale (`div` 2) (listOf1 objectProducer))
    ]
    where objectProducer :: Arbitrary a => Gen (Text, a)
          objectProducer = (,)
            <$> simpleText
            <*> arbitrary

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

newtype Defs = Defs { unDefs :: [(TypeLabel, GenerateTree)] }
  deriving (Show, Eq, Semigroup, Monoid)

instance PP Defs where
  pp = concatMap (\(tl, gt) -> pp tl ++ " = " ++ pp gt ++ "\n") . unDefs

instance Arbitrary Defs where
  arbitrary = Defs <$> arbitrary

instance PP GenerateTree where
  pp = pretty 0
    where pretty :: Int -> GenerateTree -> String
          pretty _ (GRx rx) = "/" ++ Reggie.pp rx ++ "/"
          pretty _ (GType tl) = pp tl
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
