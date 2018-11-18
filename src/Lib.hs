{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
  ( Apt(..)
  , Stack(..)
  , Sub(..)
  , Opt(..)
  , Upgradable
  , update
  , upgrade
  , eval
  , updateStack
  , upgradeStack
  , cat
  ) where

import           Control.Monad
import           Data.List.Split as S
import           Data.Text       as T
import           Data.Text.IO    as TIO
import           Prelude         as P
import           System.Process

type Upgradable = Bool

type Package = String

data Apt a where
  Empty :: Apt ()
  Apt :: Show a => a -> Apt a
  Update :: Apt Upgradable
  UpgradeList :: Apt [Package]
  UpgradeOne :: Package -> Apt Package
  Upgrade :: Apt [Package]

deriving instance Show (Apt a) => Show (Apt a)

data Stack a where
  Stack :: Show a => a -> Stack a
  UpdateStack :: Stack ()
  UpgradeStack :: Stack ()

deriving instance Show (Stack a) => Show (Stack a)

data Sub
  = SubUpdate
  | SubUpgrade

data Opt
  = OptEmpty
  | OptUpdate
  | OptList
  | OptYes
  | OptPkg String

instance Show Opt where
  show OptEmpty     = ""
  show OptUpdate    = "update"
  show OptList      = "list"
  show OptYes       = "-y"
  show (OptPkg str) = str

instance Show Sub where
  show SubUpdate  = "update"
  show SubUpgrade = "upgrade"

eval (Apt a) = a

update :: Apt a -> IO Upgradable
update Update = do
  res <- aptInternal SubUpdate [OptEmpty]
  if count "upgradable" (T.concat res) > 0
    then return True
    else return False

upgrade :: Apt a -> IO [Text]
upgrade UpgradeList = upgradeInternal [OptList]
upgrade (UpgradeOne pkg) = upgradeInternal [OptPkg pkg]
upgrade Upgrade = do
  p <- update Update
  if p
    then upgradeInternal []
    else return []

cat :: [Text] -> IO ()
cat = TIO.putStrLn <$> T.concat

updateStack :: Stack a -> IO ()
updateStack UpdateStack = stackInternal SubUpdate [] >>= cat

upgradeStack :: Stack a -> IO ()
upgradeStack UpgradeStack = do
  updateStack UpdateStack
  res <- stackInternal SubUpgrade []
  cat res

-- internal
aptInternal :: Sub -> [Opt] -> IO [Text]
aptInternal = cmdInternal "apt" (P.map show . (OptYes :))

stackInternal :: Sub -> [Opt] -> IO [Text]
stackInternal = cmdInternal "stack" (P.map show)

cmdInternal :: String -> ([Opt] -> [String]) -> Sub -> [Opt] -> IO [Text]
cmdInternal cmd f sub opts =
  P.map pack . S.splitOn "\n" <$> readProcess cmd (show sub : f opts) ""

upgradeInternal :: [Opt] -> IO [Text]
upgradeInternal opts = upgradeOutFilter <$> aptInternal SubUpgrade opts
  where
    upgradeOutFilter :: [Text] -> [Text]
    upgradeOutFilter =
      P.map pack .
      P.filter (not . P.null) .
      P.filter (\s -> count "..." (pack s) == 0) . P.map unpack
