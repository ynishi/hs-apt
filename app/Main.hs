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

updateAll t = do
  L.newWithTvar t >>= L.update . L.upg >>= L.list >>= L.upgrade
  return ()

main :: IO ()
main = do
  o <- execParser opts
  putStrLn $ target o
  case target o of
    "apt" -> updateAll L.Apt
    "stack" -> updateAll L.Stack
    "npm" -> updateAll L.NPM
    "yarn" -> updateAll L.Yarn
    "all" -> do
      updateAll L.Apt
      updateAll L.Stack
      updateAll L.NPM
      updateAll L.Yarn
  where
    opts =
      info
        (appOpts <**> helper)
        (fullDesc <> progDesc "Upgrade for TARGET" <>
         header "sysup - a command for upgrade")
