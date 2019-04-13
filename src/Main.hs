{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Main where

import Analizer (analize)


main :: IO ()
main = do
  s <- getLine
  case s == "пока" || s == "stop" of
    True  -> pure () 
    False ->  do 
      putStrLn $ analize s
      main


