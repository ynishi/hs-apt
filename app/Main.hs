module Main where

import           Data.Semigroup      ((<>))
import           Lib
import qualified Libx                as L
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
  a <- L.newApt
  l <- L.update . L.Upg $ a
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
