module Main where

import           Lib

main :: IO ()
main = do
  res <- upgrade Upgrade
  cat res
