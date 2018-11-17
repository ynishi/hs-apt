module Main where

import           Lib

main :: IO ()
main = do
  upgrade Upgrade
  return ()
