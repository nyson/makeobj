{-# LANGUAGE OverloadedStrings, LambdaCase, NamedFieldPuns #-}


module Main where

import System.Environment (getArgs)
import System.Directory (getHomeDirectory, listDirectory)
import Data.Aeson (encode, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Test.QuickCheck (generate)
import Control.Monad (when)
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

data Flag = PrettyPrint | Verbose
  deriving (Ord, Eq, Show)

set :: Commands -> String -> Either String Commands
set c@Commands{flags} flag
  = case strToFlag flag of
      Just flag -> return c{flags = Set.insert flag flags }
      Nothing -> Left $ "Flag not recognized: '"++ flag ++"'"

strToFlag = \case
  "p" -> Just PrettyPrint
  "pp" -> Just PrettyPrint
  "verbose" -> Just Verbose
  "v" -> Just Verbose
  _ -> Nothing

hasSuffix :: String -> String -> Bool
hasSuffix "" "" = True
hasSuffix _  "" = False
hasSuffix suffix@(c2:_) str@(c1:next)
  | c1 == c2 && str == suffix = True
  | otherwise = hasSuffix suffix next

configFile :: IO (Either Error Defs)
configFile = do
  allDefs <- do
    dir <- (++ "/.makeobj/") <$> getHomeDirectory
    map (dir ++) . filter (hasSuffix ".defs") <$> listDirectory dir

  fmap mconcat . sequence . map parseDefs <$> mapM readFile allDefs
  -- fp <- (++ "/.makeobj/Car.defs") <$> getHomeDirectory
  -- parseDefs <$> readFile fp

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
  (Defs cfg ) <- configFile >>= \case
    Left err -> error (show err)
    Right c -> return c

  cmd <- parseCommands <$> getArgs >>= \case
    Left err -> error err
    Right c -> return c

  when (cmd `hasFlag` Verbose) $ do
    putStrLn "Defs loaded: "
    mapM_ (putStrLn . (\(d,_) -> "\t" ++ pp d)) cfg

  let encoder :: ToJSON a => a -> IO ()
      encoder | cmd `hasFlag` PrettyPrint = BL8.putStrLn . encodePretty
              | otherwise = BL8.putStrLn . encode

  case argument cmd of
    [] -> randomCar >>= encoder
    (arg:_) -> case lookup (mkTypeLabel arg) cfg of
      Just gen -> generate (generateObj cfg gen) >>= encoder
      Nothing -> putStrLn "Definition not found!"

hasFlag :: Commands -> Flag -> Bool
hasFlag = flip elem . flags
