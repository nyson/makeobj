{-# LANGUAGE FlexibleInstances #-}
module Data.MakeObj.PP where

class PP t where
  pp :: t -> String
  pp = ppt 0
  pprint :: t -> IO ()
  pprint = putStrLn . pp
  ppt :: Int -> t -> String
  ppt = const pp

instance PP t => PP (Maybe t) where
  pp (Just t) = "Just " ++ pp t
  pp Nothing = "Nothing"

instance PP Int    where pp = show
instance PP Double where pp = show
instance PP String where pp = id
