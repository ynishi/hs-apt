{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Libx where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.List.Split          as S
import           Data.Text                as T
import           Data.Text.IO             as TIO
import           Prelude                  as P
import           System.Process

type Upgradable = Bool

type Package = String

data EmptyPT =
  EmptyPT
  deriving (Show)

data NonEmptyPT =
  NonEmptyPT
  deriving (Show)

data Upg p a where
  Empty :: Upg EmptyPT a
  List :: Upg NonEmptyPT a -> Upg NonEmptyPT a
  Upg :: a -> Upg NonEmptyPT a
  Update :: Upg NonEmptyPT a -> Upg NonEmptyPT a
  Upgrade :: Bool -> Upg NonEmptyPT a

deriving instance (Show p, Show a) => Show (Upg p a)

instance Functor (Upg p) where
  fmap f (Upg x) = Upg . f $ x
  fmap f Empty   = Empty

empty :: Upg EmptyPT a
empty = Empty

list :: Entity a => Upg NonEmptyPT a -> IO (Upg NonEmptyPT a)
list (Upg u) = do
  x <- listEntity u
  return . Upg $ x

upg :: a -> Upg NonEmptyPT a
upg = Upg

update :: Entity a => Upg NonEmptyPT a -> IO (Upg NonEmptyPT a)
update (Upg u) = do
  x <- updateEntity u
  return . Upg $ x

upgrade :: Entity a => Upg NonEmptyPT a -> IO (Upg NonEmptyPT a)
upgrade (Upg u) = do
  x <- upgradeEntity u
  return . Upg $ x

class Entity a where
  updateEntity :: a -> IO a
  upgradeEntity :: a -> IO a
  listEntity :: a -> IO a

type AptState = (Bool, [Package])

type AptValue = [Package]

newtype Apt =
  Apt (State AptState AptValue)

startAptState = (False, [])

instance Entity Apt where
  updateEntity (Apt s) = do
    print $ runState s $ (False, [])
    readProcess "apt" ["update"] ""
    return $ Apt s
  upgradeEntity a = do
    readProcess "apt" ["upgrade"] ""
    return a
  listEntity a = do
    readProcess "apt" ["upgrade", "list"] ""
    return a
