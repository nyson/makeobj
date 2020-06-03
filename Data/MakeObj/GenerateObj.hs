{-# LANGUAGE LambdaCase, TupleSections #-}
module Data.MakeObj.GenerateObj where

import Data.Aeson (Value(..))
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.MakeObj.AST
import Data.MakeObj.AST.Time (genRangeBetween)
import Data.MakeObj.PP (pp)
import Test.QuickCheck (listOf, Gen, choose)
import Control.Monad (replicateM)
import Data.Scientific (fromFloatDigits)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Text.Reggie as R

generateObj :: Defs -> GenerateTree -> Gen Value
generateObj defs = \case
  GRx rx -> String . T.pack <$> R.rxGen rx
  GType tl -> case Prelude.lookup tl (unDefs defs) of
      Nothing -> fail $ "Missing typedef: " ++ pp tl
      Just gt -> generateObj defs gt
  GObj obj -> Object . HM.fromList <$> mapM f (HM.toList obj)
    where f (a, t) = (a,) <$> generateObj defs t
  GList t -> Array . V.fromList <$> generateList defs t
  GRange (IntRange a b) -> Number . fromIntegral <$> choose (a , b)
  GRange (FloatRange a b) -> Number . fromFloatDigits <$> choose (a, b)
  GRange (DateRange a b) -> String . T.pack . pp <$> genRangeBetween a b
  GLiteral v -> genLiteral v

generateList :: Defs -> GenerateList -> Gen [Value]
generateList defs = \case
  LiteralList ls -> mapM (generateObj defs) ls
  Unbounded t -> listOf (generateObj defs t)
  ListOf i t -> replicateM i (generateObj defs t)
  RangedList minLen maxLen t -> do
    len <- choose (minLen, maxLen)
    replicateM len (generateObj defs t)

genLiteral :: Literal -> Gen Value
genLiteral = pure . \case
  LNumber n -> Number n
  LBool b -> Bool b
  LNull -> Null
  LString s -> String s
  LTime t -> (String . T.pack . pp) t

newtype LinkOptions = LinkOptions
  { allowCyclicDependencies :: Bool
  } deriving (Eq, Show)

data LinkedGenerateTree = LinkedGenerateTree
  { mainNode :: GenerateTree
  , avaiableDefs :: HashMap TypeLabel GenerateTree
  }

hasCycles :: Defs -> GenerateTree -> Bool
hasCycles = go Set.empty
  where go :: Set TypeLabel -> Defs -> GenerateTree -> Bool
        go touched defs = \case
          GType lbl
            | lbl `Set.member` touched -> True
            | otherwise -> case lookup lbl (unDefs defs) of
                Just tree -> go (Set.insert lbl touched) defs tree
                Nothing -> False
          GObj hm -> any (go touched defs) . map snd $ HM.toList hm
          GList (RangedList _ _ t) -> go touched defs t
          GList (ListOf _ t) -> go touched defs t
          GList (Unbounded t) -> go touched defs t
          GList (LiteralList ts) -> any (go touched defs) ts
          _ -> False
