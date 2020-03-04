{-# LANGUAGE OverloadedStrings, LambdaCase #-}


module Main where

import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import Data.Aeson
import Test.QuickCheck (generate)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.MakeObj

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

configFile :: IO (Either Error Defs)
configFile = do
  fp <- (++ "/.makeobj/Car.defs") <$> getHomeDirectory
  parseDefs <$> readFile fp

main :: IO ()
main = do
  Right (Defs cfg) <- configFile
  safeHead <$> getArgs >>= \case
    Nothing -> BL8.putStrLn =<< encode <$> randomCar
    Just arg -> do
      let Just gen = lookup (TypeLabel arg) cfg
      BL8.putStrLn =<< encode <$> generate (generateObj cfg gen)

