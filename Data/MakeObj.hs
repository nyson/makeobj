{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveGeneric, GeneralizedNewtypeDeriving, LambdaCase #-}
{-# LANGUAGE FlexibleInstances, GADTs #-}


module Data.MakeObj (
  Car(..), Defs(..), Error(..), TypeLabel(..),
  parseGenerateTree, parseDefs,
  pp, pprint, randomCar, toStructure, jsonTreeEquality,
  generateObj, mkTypeLabel, genTree
  ) where

import Data.MakeObj.GenerateObj (generateObj)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Generics
import Test.QuickCheck
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.MakeObj.AST
import Data.Text (Text)
import Data.MakeObj.PP (pp, pprint)
import Data.MakeObj.Parser hiding (rx)
import Data.MakeObj.TreeEquality (jsonTreeEquality, toStructure)
import Text.Reggie (rx, rxGen)
import qualified Data.Text as T
import Data.Time.Clock

newtype SimpleString = Simple String
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey)

instance Arbitrary SimpleString where
  arbitrary = Simple <$> rxGen "[a-z0-9]+"

newtype Vin = Vin Text
  deriving (Show, Eq, Generic, ToJSON)

data Model = PS1 | PS2
  deriving (Show, Eq, Generic)

instance ToJSON Model where
  toJSON PS1 = "PS1"
  toJSON PS2 = "PS2"

newtype Package = Package Text
  deriving (Eq, Show, Generic, ToJSON)

newtype Color = Color Text
  deriving (Eq, Show, Generic, ToJSON)

newtype Market = Market Text
  deriving (Eq, Show, Generic, ToJSON)

newtype LocalCarIdentifier = LocalCarIdentifier Text
  deriving (Eq, Show, Generic, ToJSON)

newtype Date = Date UTCTime
  deriving (Eq, Show, Generic, ToJSON)

data CarStatus = AtHub | IncomingToMarket | InTransition | MarketParkingLot
  deriving (Eq, Show, Generic)

instance ToJSON CarStatus where
  toJSON = \case
    AtHub -> "AT_HUB"
    IncomingToMarket -> "INCOMING_TO_MARKET"
    InTransition -> "IN_TRANSITION"
    MarketParkingLot -> "MARKET_PARKING_LOT"

data DynamoTyped t
  = DString t
  | DNum t
    deriving (Show)

instance Eq t => Eq (DynamoTyped t) where
  (DString a) == (DString b) = a == b
  (DNum a) == (DNum b) = a == b
  _ == _ = False

instance ToJSON t => ToJSON (DynamoTyped t) where
  toJSON t = Object $ HM.fromList [(k, toJSON v)]
    where (k,v) = case t of
            DString t -> ("S", t)
            DNum t -> ("N", t)

newtype YearModel = YearModel Int
  deriving (Eq, Show, Generic, ToJSON)

data Car = Car
  { vin :: Vin
  , estimatedArrivalDate :: Date
  , model :: Model
  , package :: Package
  , marketId :: Market
  , color :: Color
  , localIdentifier :: LocalCarIdentifier
  , year :: YearModel
  -- , status :: CarStatus
  -- , carMetaData :: Map SimpleString SimpleString
  } deriving (Eq, Show, Generic)


timeWithOffset :: Int -> UTCTime
timeWithOffset i = addUTCTime (fromIntegral i * nominalDay) currentTime
  where currentTime = unsafePerformIO getCurrentTime

instance Arbitrary (Car) where
  arbitrary = Car
    <$> (Vin . T.pack <$> rxGen [rx|[0-9A-Z]{17}|])
    <*> (Date . timeWithOffset <$> arbitrary)
    <*> (elements [PS1, PS2])
    <*> ( Package <$> elements ["performance", "regular"])
    <*> ( Market . T.pack <$> elements ["se"])
    <*> (Color <$> elements ["black"])
    <*> (LocalCarIdentifier . T.pack <$> rxGen [rx|[A-Z]{3} [0-9]{3}|])
    <*> (YearModel . (+ 2020) <$> choose (-10, 10))

instance ToJSON Car

randomCars :: IO [Car]
randomCars = mapM generate (replicate 20 arbitrary)

randomCar :: IO Car
randomCar = generate arbitrary
