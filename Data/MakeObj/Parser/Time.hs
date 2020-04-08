{-# LANGUAGE TupleSections  #-}
module Data.MakeObj.Parser.Time where

import Prelude hiding (fail)
import Text.Read (readMaybe)
import Data.Time (secondsToDiffTime, picosecondsToDiffTime, UTCTime(..), DiffTime)
import Data.Time.Calendar (fromGregorian)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Control.Monad.Fail
import Data.MakeObj.Parser.Shared
import Data.MakeObj.AST.Time

timeLiteral :: Parser TimeLiteral
timeLiteral = do 
  (dayGranularity, day) <- choice 
    [ try $ fmap (Day,  ) $ fromGregorian <$> year <*> month  <*> day
    , fmap (Month, ) $ fromGregorian <$> year <*> month  <*> pure 0
    ]
  (timeGranularity, diffTime) <- choice
    [ char 'T' *> choice
      [ try $ (HourMinuteSecondsFractionsTimezone,) <$> ((+)
        <$> hoursMinutesSecondsFractions
        <*> timezone)
      , try $ (HourMinuteSecondsFractions,) <$> hoursMinutesSecondsFractions
      , try $ (HourMinuteSecondsTimezone,) <$> ((+) 
        <$> hoursMinutesSeconds 
        <*> timezone)
      , try $ (HourMinuteSeconds,) <$> hoursMinutesSeconds
      , try $ (HourMinuteTimezone,) <$> ((+) 
        <$> hoursMinutes 
        <*> timezone)
      , (HourMinute,) <$> hoursMinutes
      ]
    , return (NoTime, 0.0)
    ]
  return $ TimeLiteral (UTCTime day diffTime) dayGranularity timeGranularity

timezone :: Parser DiffTime
timezone = label "Timezone"
  $ choice [ try $ label "Timezone Hours and Minutes" $ char '+' *> hoursMinutes
           , label "Z" $ char 'Z' $> 0]

year :: Parser Integer
year = label "Year" $ many digitChar >>= mread

month,day :: Parser Int
month = label "Month" $ char '-' *> many digitChar >>= mread
day = label "Day" month

hoursMinutes :: Parser DiffTime
hoursMinutes = label "Hours and Minutes" 
  $ (\h m -> secondsToDiffTime ((h + m) * 60))
  <$> ((* 24) . read <$> many digitChar)
  <*> (read <$> (char ':' *> many digitChar))

hoursMinutesSeconds :: Parser DiffTime
hoursMinutesSeconds = (+)
  <$> hoursMinutes 
  <*> (secondsToDiffTime . read <$> (char ':' *> nOf digitChar 2))

hoursMinutesSecondsFractions :: Parser DiffTime
hoursMinutesSecondsFractions = (+)
  <$> hoursMinutesSeconds
  <*> (do
    fracs <- char '.' *> many digitChar
    picosecondsToDiffTime <$> mread fracs
    )

mread :: (MonadFail m, Read a) => String -> m a
mread s = maybe (fail $ "Bad read: '"++ s ++"'") return (readMaybe s)
