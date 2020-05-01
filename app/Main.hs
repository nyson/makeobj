{-# LANGUAGE LambdaCase, NamedFieldPuns #-}
module Main where

import Test.QuickCheck (generate)
import Control.Monad (when)
import Data.MakeObj (parseGenerateTree, pp, generateObj, unDefs)
import Options (Options(..), loadOptions)
import Definitions (loadDefDir, loadDefFile)

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
    (args:_) -> case parseGenerateTree args of
      Right tree -> generate (generateObj defs tree) >>= encoder
      Left err -> putStrLn $ "Unsupported syntax: \n\t" ++ show err
