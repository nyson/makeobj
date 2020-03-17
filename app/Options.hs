module Options where

import Data.Set (Set)
import qualified Data.Set as Set
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson (encode, Value)
import Data.Aeson.Encode.Pretty (encodePretty)

defaultOptions :: Options
defaultOptions = Options False bl8Encoder Nothing []

data Options = Options
    { verbose :: Bool
    , encoder :: Value -> IO ()
    , scope :: Maybe String
    , arg :: [String]
    }

bl8Encoder = BL8.putStrLn . encode
bl8PrettyEncoder :: Value -> IO ()
bl8PrettyEncoder = BL8.putStrLn . encodePretty

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbose"]
    (NoArg (\o -> o{verbose= True}))
    "chatty output"

  , Option ['p'] ["pretty", "prettyprint"]
    (NoArg (\o -> o{encoder= BL8.putStrLn . encodePretty}))
    "pretty print resulting JSON"

  , Option ['s'] ["scope"]
    (ReqArg (\scopeName o -> o{scope= Just scopeName }) "FILE")
    "only read specific scope"
  ]


mkOptions :: [String] -> Either String Options
mkOptions argv = case getOpt Permute options argv of
    (optFs, remainingArgs, []) ->
      let opts = foldl (flip id) defaultOptions optFs
      in return opts{arg= remainingArgs }
    (_, _, errs) -> Left (concat errs ++ usageInfo header options)
    where header = "usage: makeobj [OPTION...] Defenitions..."

loadOptions :: IO Options
loadOptions = either (ioError . userError) return =<< mkOptions <$> getArgs
