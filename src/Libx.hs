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

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.STM

type Upgradable = Bool

type Package = Text

data EmptyPT =
  EmptyPT
  deriving (Show)

data NonEmptyPT =
  NonEmptyPT
  deriving (Show)

data Upg p a where
  Empty :: Upg p a -- for Applicative: Upg EmptyPT a
  List :: a -> Upg NonEmptyPT a
  Upg :: a -> Upg p a -- for Applicative: Upg NonEmptyPT a
  Update :: a -> Upg NonEmptyPT a
  Upgrade :: a -> Upg NonEmptyPT a

deriving instance (Show p, Show a) => Show (Upg p a)

instance Functor (Upg p) where
  fmap f (Upg x) = Upg . f $ x
  fmap _ Empty   = Empty

instance Applicative (Upg a) where
  pure = Upg
  (<*>) (Upg f) (Upg a) = Upg (f a)
  (<*>) _ _             = Empty

instance Monad (Upg a) where
  return = Upg
  (>>=) (Upg x) f = f x
  (>>=) _ _       = Empty

empty :: Upg EmptyPT a
empty = Empty

list :: Entity a => Upg NonEmptyPT a -> IO (Upg NonEmptyPT a)
list (List u) = do
  x <- listEntity u
  return . Upg $ x

upg :: a -> Upg NonEmptyPT a
upg = Upg

update :: Entity a => Upg NonEmptyPT a -> IO (Upg NonEmptyPT a)
update (Update u) = Update <$> updateEntity u

upgrade :: Entity a => Upg NonEmptyPT a -> IO (Upg NonEmptyPT a)
upgrade (Upgrade u) = Upgrade <$> upgradeEntity u

class Entity a where
  updateEntity :: a -> IO a
  upgradeEntity :: a -> IO a
  listEntity :: a -> IO a

newtype Apt =
  Apt (TVar [Package])

newApt = do
  tVar <- atomically $ newTVar []
  return (Apt tVar)

instance Entity Apt where
  updateEntity (Apt tVar) = do
    d <- readProcess "apt" ["update"] ""
    atomically $ writeTVar tVar $ parse d
    print $ "tVar:" ++ show d
    return $ Apt tVar
    where
      parse = P.map pack . S.splitOn "\n"
  upgradeEntity (Apt tVar) = do
    d <- readProcess "apt" ["upgrade"] ""
    atomically . writeTVar tVar . upgradeOutFilter . parse $ d
    print $ "tVar:" ++ show d
    return $ Apt tVar
    where
      parse = P.map pack . S.splitOn "\n"
  listEntity (Apt tVar) = do
    d <- readProcess "apt" ["upgrade", "list"] ""
    atomically . writeTVar tVar . upgradeOutFilter . parse $ d
    return $ Apt tVar
    where
      parse d = P.map (pack . P.head . S.splitOn "/") $ S.splitOn "\n" d

upgradeOutFilter :: [Text] -> [Text]
upgradeOutFilter =
  P.map pack .
  P.filter (not . P.null) .
  P.filter (\s -> count "..." (pack s) == 0) . P.map unpack
