{-# LANGUAGE OverloadedStrings, DeriveGeneric, LambdaCase, NamedFieldPuns #-}
module Main where

import Web.Scotty
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.MakeObj (generateBalancedJsonWithMeta, ObjectPackage(..))
import Data.MakeObj.CountNodes (Meta)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import System.Console.GetOpt
import System.Environment (getArgs)
import Data.List (foldl')
import System.Directory (doesFileExist)
import Control.Monad (unless)

data CodeGenRequest = CodeGenReq
  { input :: String
  , defs :: String
  } deriving (Generic, Show)
instance Aeson.FromJSON CodeGenRequest

data CodeGenResponse = CodeGenResp
  { code :: Value
  , cgError :: Maybe String
  , meta :: Maybe Meta
  } deriving (Generic, Show)
instance Aeson.ToJSON CodeGenResponse

baseUrl :: String
baseUrl = "./web-frontend/build/"

newtype WebAppConfig = WebAppConfig
  { hostDir :: FilePath
  } deriving (Eq, Show)

defaultConfig :: WebAppConfig
defaultConfig = WebAppConfig baseUrl

config :: [OptDescr (WebAppConfig -> WebAppConfig)]
config =
  [ Option ['h'] ["hostDir"]
    (ReqArg (\newHostDir o -> o{hostDir = newHostDir}) "DIR")
    "Base hosting directory of application"
  ]

loadConfig :: IO WebAppConfig
loadConfig = either (ioError . userError) return =<< mkConfig <$> getArgs
  where mkConfig :: [String] -> Either String WebAppConfig
        mkConfig argv = case getOpt Permute config argv of
          (optFs, _, []) -> Right $ foldl' (flip id) defaultConfig optFs
          (_, _, errs) -> Left $ concat errs
            ++ usageInfo "usage: makeobj-web-app [OPTION...]" config

main :: IO ()
main = do
  cfg@WebAppConfig {hostDir} <- loadConfig
  doesFileExist (hostDir <> "index.html")
    >>= flip unless (ioError . userError $ concat
                      [ "Hosting directory '", hostDir
                      , "' was not found on system!"])
  putStrLn $ "config: " <> show cfg
  scotty 1234 $ do
    middleware $ staticPolicy (noDots >-> addBase hostDir)
    middleware logStdoutDev
    post "/api" $ do
      CodeGenReq{input, defs} <- jsonData
      liftIO (generateBalancedJsonWithMeta 250 input defs) >>= \case
        Left err -> json $ CodeGenResp
          { code = ""
          , cgError = Just err
          , meta = Nothing
          }
        Right ObjectPackage
          { generatedCode
          , metadata} -> json $ CodeGenResp
          { code = generatedCode
          , cgError = Nothing
          , meta = Just metadata
          }
    get "/" $ file (hostDir <> "index.html")


