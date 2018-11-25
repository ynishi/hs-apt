module Main where

import           Data.Semigroup      ((<>))
import           Lib
import           Options.Applicative

newtype AppOpts = AppOpts
  { target :: String
  }

appOpts :: Parser AppOpts
appOpts =
  AppOpts <$>
  strOption
    (long "target" <> short 't' <> help "Target to upgrade" <> showDefault <>
     value "apt" <>
     metavar "TARGET")

main :: IO ()
main = do
  o <- execParser opts
  case target o of
    "apt"   -> upgrade Upgrade >>= cat
    "stack" -> upgradeStack UpgradeStack
  where
    opts =
      info
        (appOpts <**> helper)
        (fullDesc <> progDesc "Upgrade for TARGET" <>
         header "sysup - a command for upgrade")
