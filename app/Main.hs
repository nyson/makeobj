{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BL8
import Data.MakeObj


main = BL8.putStrLn =<< encode <$> randomCar

