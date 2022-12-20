{-# LANGUAGE LambdaCase, RankNTypes, TypeApplications #-}
module Data.MakeObj.TreeEquality where

import Data.Aeson (ToJSON, FromJSON(parseJSON), Value(..), decode, encode)
import Data.Aeson.KeyMap (KeyMap)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.MakeObj.PP

import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key

keyMapToTextHashMap :: KeyMap a -> HashMap Text a
keyMapToTextHashMap = HM.mapKeys (Key.toText) . KM.toHashMap

data ValType = TString | TNumber | TBool | TNull
  deriving (Eq, Show)

instance PP ValType where pp = show

data JSONStructure = TNode ValType
              | TList [JSONStructure]
              | TObject (HashMap Text JSONStructure)
  deriving (Eq, Show)

instance PP JSONStructure where
  ppt i = \case
    TNode t -> pp t
    TList xs ->
      let iLvl = i+1
          elems = concatMap ((++ ", \n") . ppt iLvl) xs
      in " [\n" ++ elems ++ replicate (i * 2) ' ' ++ "] "
    TObject hm ->
      let il = i+1
          ppKV key val = replicate (il * 2) ' '
                         ++ unpack key ++ ": "
                         ++ ppt il val ++ "\n"
          elems = concatMap snd . HM.toList $ HM.mapWithKey ppKV hm
      in "{\n" ++ elems ++ replicate (i * 2) ' ' ++ "} "

instance FromJSON JSONStructure where
  parseJSON = \case
    String _ -> return $ TNode TString
    Number _ -> return $ TNode TNumber
    Bool   _ -> return $ TNode TBool
    Null     -> return $ TNode TNull
    Array arr -> TList <$> mapM parseJSON (V.toList arr)
    Object o -> TObject <$> sequence (HM.map parseJSON $ keyMapToTextHashMap o)

jsonStructure :: ToJSON a => a -> Maybe JSONStructure
jsonStructure = decode . encode

jsonStructureEquality :: (ToJSON a, ToJSON b) => a -> b -> Bool
jsonStructureEquality a b = fromMaybe False $ do
  let ecdc :: forall t. ToJSON t => t -> Maybe JSONStructure
      ecdc = decode @JSONStructure . encode
  a' <- ecdc a
  b' <- ecdc b
  return $ a' == b'

