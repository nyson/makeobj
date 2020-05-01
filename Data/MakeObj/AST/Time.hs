{-# LANGUAGE TupleSections, LambdaCase, NamedFieldPuns #-}
module Data.MakeObj.AST.Time where

import Data.Time
  ( Day(..), DiffTime, UTCTime(..)
  , toGregorian, fromGregorian
  , secondsToDiffTime, picosecondsToDiffTime
  , diffTimeToPicoseconds
  , addUTCTime)
import Data.Time.Calendar (isLeapYear)
import Data.Maybe (fromMaybe)
import Data.MakeObj.PP (PP(..))

import Test.QuickCheck


data TimeLiteral = TimeLiteral
  { tTime :: UTCTime
  , tDayGranularity :: DayGranularity
  , tTimeGranularity :: TimeGranularity
  } deriving Show

fullSecond :: Num a => a
fullSecond = 10^(12 :: Integer)

addDiff :: TimeLiteral -> DiffTime -> TimeLiteral
addDiff tl@TimeLiteral{tTime} diff = tl{tTime = addUTCTime (nom diff) tTime}
  where nom = fromInteger . div fullSecond. diffTimeToPicoseconds

arbitraryDiffTime :: Gen DiffTime
arbitraryDiffTime = secondsToDiffTime . round  <$> choose (0 :: Double, 100*60*60*24*365)

instance PP TimeLiteral where
  pp (TimeLiteral t dg NoTime) = showGranularDay t dg
  pp (TimeLiteral t dg tg) = showGranularDay t dg ++ "T" ++ showGranularTime t tg

instance Eq TimeLiteral where
  a == b | tTimeGranularity a /= tTimeGranularity b = False
  a == b | tDayGranularity a  /= tDayGranularity b  = False
  (TimeLiteral t1 dg tg) == (TimeLiteral t2 _ _)
    = dayEqual dg && diffTimeEqual tg (utctDayTime t1) (utctDayTime t2)
    where ((y1, m1, _), (y2, m2, _)) = (toGregorian $ utctDay t1, toGregorian $ utctDay t2)
          dayEqual = \case
            Year -> y1 == y2
            Month -> y1 == y2 && m1 == m2
            Day -> utctDay t1 == utctDay t2

diffTimeEqual :: TimeGranularity -> DiffTime -> DiffTime -> Bool
diffTimeEqual tg d1 d2 = case tg of
  NoTime
    -> True
  HourMinute
    -> diff1 `div` (fullSecond * 60) == diff2 `div` (fullSecond * 60)
  HourMinuteTimezone
    -> diff1 `div` (fullSecond * 60) == diff2 `div` (fullSecond * 60)
  HourMinuteSecondsTimezone
    -> diffTimeEqual HourMinuteSeconds d1 d2
  HourMinuteSeconds
    -> diff1 `div` fullSecond == diff2 `div` fullSecond
  HourMinuteSecondsFractionsTimezone
    -> diffTimeEqual HourMinuteSecondsFractions d1 d2
  HourMinuteSecondsFractions
    -> diff1 == diff2
  where (diff1, diff2) = (diffTimeToPicoseconds d1, diffTimeToPicoseconds d2)


showGranularDay :: UTCTime -> DayGranularity -> String
showGranularDay t = \case
  Year ->  ymd $ \y _ _ -> y
  Month -> ymd $ \y m _ -> concat [y, "-", m]
  Day ->   ymd $ \y m d -> concat [y, "-", m, "-", d]
  where ymd f = (\(y,m,d) -> f (show y) (show2 m) (show2 d))
            . toGregorian $ utctDay t

showGranularTime :: UTCTime -> TimeGranularity -> String
showGranularTime t = \case
  NoTime -> ""
  HourMinute -> hours ++ ":" ++ minutes
  HourMinuteTimezone -> concat [ hours, ":", minutes, "Z"]
  HourMinuteSeconds  -> concat [ hours, ":", minutes, ":", seconds]
  HourMinuteSecondsTimezone -> concat [ hours, ":", minutes, ":", seconds, "Z"]
  HourMinuteSecondsFractions -> concat
    [ hours, ":", minutes, ":", seconds, ".", fractions]
  HourMinuteSecondsFractionsTimezone -> concat
    [ hours, ":", minutes, ":", seconds, ".", fractions, "Z"]
  where (hours, minutes, seconds, fractions)
          = let ps = diffTimeToPicoseconds $ utctDayTime t
                allseconds = ps `div` fullSecond
            in ( show2 $ (allseconds `div` (60*60)) `mod` 24
               , show2 $ (allseconds `div` 60) `mod` 60
               , show2 $ (allseconds `div` 60) `mod` 60
               , show2 $ ps `mod` fullSecond )

show2 :: (Show i, Integral i) => i -> String
show2 i | abs i <= 9 = '0':show i
        | otherwise  = show i

timeGen :: Gen UTCTime
timeGen = do
  year <- choose (1970, 2200)
  month <- choose (1, 12)
  day <- choose . (1,) $ case month of
    2 | isLeapYear (toInteger month) -> 29
      | otherwise -> 28
    m | m `elem` [1,3,5,7,8,10,12] -> 30
      | otherwise -> 31
  return $ UTCTime (fromGregorian year month day) 0


instance Arbitrary TimeLiteral where
  arbitrary = do
    time <- timeGen
    dayG <- arbitrary
    TimeLiteral time dayG <$> genTimeGranularity dayG

genTimeGranularity :: DayGranularity -> Gen TimeGranularity
genTimeGranularity = \case
  Day -> arbitrary
  _ -> return NoTime

data DayGranularity
  = Year
  | Month
  | Day
  deriving (Eq, Ord, Show)

data TimeGranularity
  = NoTime
  | HourMinute
  | HourMinuteTimezone
  | HourMinuteSeconds
  | HourMinuteSecondsTimezone
  | HourMinuteSecondsFractions
  | HourMinuteSecondsFractionsTimezone
  deriving (Eq, Ord, Show)

mkDayLiteral :: Integer -> Maybe Int -> Maybe Int -> (Day, DayGranularity)
mkDayLiteral year month day
  = ( fromGregorian year (fromMaybe 1 month) (fromMaybe 1 day)
    , maybe Year (const $ maybe Month (const Day) day) month
    )

mkDateTimeLiteral :: Maybe (Integer, Integer)
              -- ^ Hours and Minutes
              -> Maybe (Either Integer (Integer, Double))
              -- ^ Seconds or Seconds with fractions
              -> Maybe (Integer, Integer)
              -- ^ Timezone
              -> (DiffTime, TimeGranularity)
mkDateTimeLiteral Nothing _ _ = (0, NoTime)
mkDateTimeLiteral (Just hourMinutes) maybeSeconds timezone
  = case maybeSeconds of
    Nothing
      -> (secondsToDiffTime $ hmTime hourMinutes + tzValue
         , maybe HourMinute (const HourMinuteTimezone) timezone
         )
    Just (Left seconds)
      -> ( secondsToDiffTime $ hmTime hourMinutes + seconds + tzValue
         , maybe HourMinuteSeconds (const HourMinuteSecondsTimezone)
            timezone
         )
    Just (Right (seconds, fractions))
      -> ( picosecondsToDiffTime $ addFrac (seconds + tzValue) fractions
         , maybe HourMinuteSeconds (const HourMinuteSecondsTimezone)
            timezone
         )

  where hmTime :: (Integer, Integer) -> Integer
        hmTime (hours, minutes) = hours * 24 * 60 + minutes * 60
        addFrac :: Integer -> Double -> Integer
        addFrac i = round . (* fullSecond) . (+ fromInteger i)
        tzValue = maybe 0 hmTime timezone

granularityGen :: Gen (DayGranularity, TimeGranularity)
granularityGen = do
  dayG <- arbitrary
  (dayG, ) <$> case dayG of
    Day -> elements
      [ HourMinute, HourMinuteSeconds, HourMinuteSecondsTimezone
      , HourMinuteSecondsFractions, HourMinuteSecondsFractionsTimezone
      ]
    _ -> pure NoTime


instance Arbitrary TimeGranularity where
  arbitrary = elements
    [ NoTime
    , HourMinute, HourMinuteSeconds, HourMinuteSecondsTimezone
    , HourMinuteSecondsFractions, HourMinuteSecondsFractionsTimezone
    ]

instance Arbitrary DayGranularity where
  arbitrary = elements [Month, Day]
