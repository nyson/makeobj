{-# LANGUAGE LambdaCase, TupleSections #-}
module Data.MakeObj.GenerateObj where

import Data.Aeson
import Data.HashMap.Strict as HM
import Data.MakeObj.AST
import Data.MakeObj.PP
import Test.QuickCheck
import Control.Monad (replicateM)

import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Text.Reggie as R

generateObj :: Defs -> GenerateTree -> Gen Value
generateObj defs = \case
  GRx rx -> String . T.pack <$> R.rxGen rx
  GType tl -> case Prelude.lookup tl (unDefs defs) of
      Nothing -> error $ "Missing typedef: " ++ pp tl
      Just gt -> generateObj defs gt
  GObj obj -> Object . HM.fromList <$> mapM f (HM.toList obj)
    where f (a, t) = (a,) <$> generateObj defs t

  GList t -> Array . V.fromList <$> generateList defs t
  GRange (Range a b) -> Number . fromIntegral <$> elements [a .. b]

generateList :: Defs -> GenerateList -> Gen [Value]
generateList defs = \case
  Unbounded t -> listOf (generateObj defs t)
  ListOf i t -> replicateM i (generateObj defs t)
  RangedList min max t -> do
    len <- choose (min, max)
    replicateM len (generateObj defs t)
