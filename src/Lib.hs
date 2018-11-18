{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
  ( Apt(..)
  , Sub(..)
  , Opt(..)
  , Upgradable
  , update
  , upgrade
  , eval
  , cat
  ) where

import           Control.Monad
import           Data.List.Split as S
import           Data.Text       as T
import           Data.Text.IO    as TIO
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

-- internal
aptInternal :: Sub -> [Opt] -> IO [Text]
aptInternal sub opts =
  Prelude.map pack . S.splitOn "\n" <$>
  readProcess "apt" (show sub : Prelude.map show (OptYes : opts)) ""

upgradeInternal :: [Opt] -> IO [Text]
upgradeInternal opts = upgradeOutFilter <$> aptInternal SubUpgrade opts
  where
    upgradeOutFilter :: [Text] -> [Text]
    upgradeOutFilter =
      Prelude.map pack .
      Prelude.filter (not . Prelude.null) .
      Prelude.filter (\s -> count "..." (pack s) == 0) . Prelude.map unpack
