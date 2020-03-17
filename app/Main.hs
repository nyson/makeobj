{-# LANGUAGE LambdaCase, NamedFieldPuns #-}
module Main where

import Data.List (isSuffixOf)
import System.Directory (getHomeDirectory, listDirectory, doesFileExist)

import Test.QuickCheck (generate)
import Control.Monad (foldM, when, unless, filterM, forM)
import Data.MakeObj (Defs (..), Error, parseDefs, GenerateTree, parseGenerateTree, pp, generateObj, TypeLabel(..))
import Options (Options(..), loadOptions)
import qualified Data.Text as T

baseDir :: IO FilePath
baseDir = (++ "/.makeobj/") <$> getHomeDirectory

loadDefFile :: String -> IO (Either Error Defs)
loadDefFile defScope = do
  absFilePath <- (++ defScope ++ ".defs") <$> baseDir
  doesFileExist absFilePath >>= flip unless
    (error $ "File not found: '"++ absFilePath ++ "'")
  parseDefs <$> readFile absFilePath

loadDefDir :: IO (Either Error Defs)
loadDefDir = do
  bd <- baseDir
  files <- map (bd ++) . filter (".defs" `isSuffixOf`) <$> listDirectory bd
  fmap mconcat . mapM parseDefs <$> mapM readFile files

main :: IO ()
main = do
  Options
    { verbose, encoder, scope, arg
    } <- loadOptions

  defs <- maybe loadDefDir loadDefFile scope >>= \case
    Left err -> error (show err)
    Right c -> return c

  when verbose $ do
    putStrLn "Defs loaded: "
    mapM_ (putStrLn . (\(d,_) -> "  " ++ pp d)) (unDefs defs)

  case arg of
    [] -> putStrLn "No argument given! What would you like me to generate?"
    (arg:_) -> case parseGenerateTree arg of
      Right tree -> generate (generateObj defs tree) >>= encoder
      Left error -> putStrLn $ "Unsupported syntax: \n\t" ++ show error
