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

main :: IO ()
main = scotty 1234 $ do
  middleware $ staticPolicy (noDots >-> addBase baseUrl)
  middleware logStdoutDev
  post "/api" $ do
    CodeGenReq{input, defs} <- jsonData
    liftIO (generateBalancedJsonWithMeta 250 input defs) >>= \case
      Left err -> json $ CodeGenResp
        { code = ""
        , cgError = Just err
        , meta = Nothing
        }
      Right (ObjectPackage{generatedCode, metadata}) -> json $ CodeGenResp
        { code = generatedCode
        , cgError = Nothing
        , meta = Just metadata
        }
  get "/" $ file (baseUrl <> "index.html")


