{-# LANGUAGE LambdaCase, TupleSections #-}
module Data.MakeObj.GenerateObj where

import Test.QuickCheck
import Text.Reggie as R
import qualified Data.Map.Lazy as Map
import Data.Aeson
import Data.HashMap.Strict as HM
import Data.MakeObj.AST
import qualified Data.Text as T
import Data.MakeObj.PP
import qualified Data.Vector as V

generateObj :: [(TypeLabel, GenerateTree)] -> GenerateTree -> Gen Value
generateObj defs = \case
  GRx rx -> String . T.pack <$> rxGen rx
  GType tl -> case Prelude.lookup tl defs of
      Nothing -> error $ "Missing typedef: " ++ pp tl
      Just gt -> generateObj defs gt
  GObj mp -> Object . HM.fromList <$> mapM (f defs) (HM.toList mp)
    where f :: [(TypeLabel, GenerateTree)]
            -> (a, GenerateTree)
            -> Gen (a, Value)
          f ds (a, t) = (a,) <$> generateObj ds t
  GList t -> Array . V.fromList <$> listOf (generateObj defs t)
