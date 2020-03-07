{-# LANGUAGE OverloadedStrings, LambdaCase, NamedFieldPuns #-}


module Main where

import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import Data.Aeson (encode, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Test.QuickCheck (generate)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.MakeObj
import qualified Data.Set as Set
import qualified Text.Read as Read

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

data Commands = Commands
  { flags :: Set.Set Flag
  , argument :: [String]
  } deriving Show

emptyCommands :: Commands
emptyCommands = Commands Set.empty []

data Flag = PrettyPrint
  deriving (Ord, Eq, Show)

set :: Commands -> String -> Either String Commands
set c@Commands{flags} flag
  = case strToFlag flag of
      Just flag -> return c{flags = Set.insert flag flags }
      Nothing -> Left $ "Flag not recognized: '"++ flag ++"'"

strToFlag = \case
  "p" -> Just PrettyPrint
  "pp" -> Just PrettyPrint
  _ -> Nothing

configFile :: IO (Either Error Defs)
configFile = do
  fp <- (++ "/.makeobj/Car.defs") <$> getHomeDirectory
  parseDefs <$> readFile fp

parseCommands :: [String] -> Either String Commands
parseCommands = pc emptyCommands
  where
    pc :: Commands -> [String] -> Either String Commands
    pc c (('-':'-':flag) : next) = do
      c' <- set c flag
      pc c' next
    pc c (('-':flag:"") : next) = do
      c' <- set c (flag:"")
      pc c' next
    pc c@Commands{argument} (word:next)
      = pc c{argument= word:argument} next
    pc c [] = return c

main :: IO ()
main = do
  Right (Defs cfg) <- configFile
  cmd <- parseCommands <$> getArgs >>= \case
    Left err -> error err
    Right c -> return c

  let encoder :: ToJSON a => a -> IO ()
      encoder | cmd `hasFlag` PrettyPrint = BL8.putStrLn . encodePretty
              | otherwise = BL8.putStrLn . encode

  case argument cmd of
    [] -> randomCar >>= encoder
    (arg:_) -> do
      let Just gen = lookup (mkTypeLabel arg) cfg
      generate (generateObj cfg gen) >>= encoder

hasFlag :: Commands -> Flag -> Bool
hasFlag = flip elem . flags
