{-# LANGUAGE OverloadedStrings, DeriveGeneric, LambdaCase, NamedFieldPuns #-}
module Main where

import Web.Scotty
import GHC.Generics
import qualified Data.Aeson as Aeson
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.MakeObj  (generateJson)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)


data CodeGenRequest = CodeGenReq
  { input :: String
  , defs :: String
  } deriving (Generic, Show)
instance Aeson.FromJSON CodeGenRequest

data CodeGenResponse = CodeGenResp
  { code :: Value
  , cgError :: Maybe String
  } deriving (Generic, Show)
instance Aeson.ToJSON CodeGenResponse

baseUrl :: String
baseUrl = "./web-frontend/build/"

main :: IO ()
main = scotty 1234 $ do
  middleware $ staticPolicy (noDots >-> addBase baseUrl)
  middleware logStdoutDev
  post "/api" $ do
    CodeGenReq{input, defs} <- jsonData
    liftIO (generateJson input defs) >>= \case
      Left err -> json $ CodeGenResp
        { code = ""
        , cgError = Just err
        }
      Right codeGen -> json $ CodeGenResp
        { code = codeGen
        , cgError = Nothing
        }
  get "/" $ file (baseUrl <> "index.html")
