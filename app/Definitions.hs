module Definitions
  ( loadDefFile
  , loadDefDir
  )where

import System.Directory
  ( getHomeDirectory, listDirectory, doesFileExist, createDirectory
  , doesDirectoryExist )
import Control.Monad (unless)
import Data.List (isSuffixOf)
import Data.MakeObj (Error, Defs, parseDefs)

baseDir :: IO FilePath
baseDir = (<> "/.makeobj/") <$> getHomeDirectory

loadDefFile :: String -> IO (Either Error Defs)
loadDefFile defScope = do
  absFilePath <- (<> defScope <> ".defs") <$> baseDir
  doesFileExist absFilePath >>= flip unless
    (error $ "File not found: '" <> absFilePath <> "'")
  parseDefs <$> readFile absFilePath

loadDefDir :: IO (Either Error Defs)
loadDefDir = do
  bd <- baseDir
  directoryExists <- doesDirectoryExist bd
  unless directoryExists $ createDirectory bd
  files <- map (bd <>) . filter (".defs" `isSuffixOf`) <$> listDirectory bd
  fmap mconcat . mapM parseDefs <$> mapM readFile files

