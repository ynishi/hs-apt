module Main where

import           Lib

main :: IO ()
main = do
  resApt <- upgrade Upgrade
  cat resApt
  -- upgradeStack UpgradeStack
