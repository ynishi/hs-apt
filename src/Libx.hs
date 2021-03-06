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
import           Data.List                as DL
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
  return . Upgrade $ x

upg :: a -> Upg NonEmptyPT a
upg = Upg

update :: Entity a => Upg NonEmptyPT a -> IO (Upg NonEmptyPT a)
update (Update u) = List <$> updateEntity u
update (Upg u)    = List <$> updateEntity u

upgrade :: Entity a => Upg NonEmptyPT a -> IO (Upg NonEmptyPT a)
upgrade (Upgrade u) = Upg <$> upgradeEntity u

class Entity a where
  updateEntity :: a -> IO a
  upgradeEntity :: a -> IO a
  listEntity :: a -> IO a

newWithTvar t = do
  tVar <- atomically $ newTVar []
  return . t $ tVar

newtype Apt =
  Apt (TVar [Package])

instance Entity Apt where
  updateEntity (Apt tVar) = do
    d <- readProcess "apt" ["update"] ""
    TIO.putStrLn . pack $ "Updated:\n" ++ d
    return $ Apt tVar
  upgradeEntity (Apt tVar) = do
    ps <- readTVarIO tVar
    d <- readProcess "apt" ("upgrade" : "-y" : P.map (sq . show) ps) ""
    let pd = parse d
    atomically . writeTVar tVar . upgradeOutFilter $ pd
    TIO.putStrLn . concatList "Upgraded:" $ pd
    return $ Apt tVar
    where
      parse = P.map pack . DL.lines
      sq :: String -> String
      sq ('\"':s) = P.init s
      sq s        = s
  listEntity (Apt tVar) = do
    d <- readProcess "apt" ["list", "--upgradeable"] ""
    let pd = parse d
    atomically . writeTVar tVar . upgradeOutFilter $ pd
    TIO.putStrLn . concatList "Upgradable List:" $ pd
    return $ Apt tVar
    where
      parse =
        P.map (pack . P.head . S.splitOn "/") .
        P.filter (DL.isInfixOf "/") . DL.lines

newtype NPM =
  NPM (TVar [Text])

instance Entity NPM where
  updateEntity (NPM tVar) = do
    d <- readProcess "npm" ["update"] ""
    TIO.putStrLn . pack $ "Updated:\n" ++ d
    return $ NPM tVar
  upgradeEntity (NPM tVar) = do
    d <- readProcess "npm" ["install", "-g", "npm"] ""
    let pd = parse d
    atomically . writeTVar tVar $ pd
    TIO.putStrLn . concatList "Upgraded:" $ pd
    return $ NPM tVar
    where
      parse =
        P.map (pack . P.head . S.splitOn ",") .
        P.filter (DL.isInfixOf "Version") . DL.lines
  listEntity (NPM tVar) = do
    d <- readProcess "npm" ["list"] ""
    let pd = parse d
    atomically . writeTVar tVar $ pd
    TIO.putStrLn . concatList "Dependencies:" $ pd
    return $ NPM tVar
    where
      parse = P.map (pack . P.head . S.splitOn " ") . DL.lines

newtype Yarn =
  Yarn (TVar [Text])

instance Entity Yarn where
  updateEntity (Yarn tVar) = do
    TIO.putStrLn . pack $ "do nothing:\n"
    return $ Yarn tVar
  upgradeEntity (Yarn tVar) = do
    d <- readProcess "apt" ["upgrade", "-y", "yarn"] ""
    let pd = parse d
    atomically . writeTVar tVar $ pd
    TIO.putStrLn . concatList "Upgraded:" $ pd
    return $ Yarn tVar
    where
      parse =
        P.map (pack . P.head . S.splitOn ",") .
        P.filter (DL.isInfixOf "Version") . DL.lines
  listEntity (Yarn tVar) = do
    TIO.putStrLn . pack $ "do nothing:\n"
    return $ Yarn tVar

newtype Stack =
  Stack (TVar [Text])

instance Entity Stack where
  updateEntity (Stack tVar) = do
    d <- readProcess "stack" ["update"] ""
    TIO.putStrLn . pack $ "Updated:\n" ++ d
    return $ Stack tVar
  upgradeEntity (Stack tVar) = do
    d <- readProcess "stack" ["upgrade"] ""
    let pd = parse d
    atomically . writeTVar tVar $ pd
    TIO.putStrLn . concatList "Upgraded:" $ pd
    return $ Stack tVar
    where
      parse =
        P.map (pack . P.head . S.splitOn ",") .
        P.filter (DL.isInfixOf "Version") . DL.lines
  listEntity (Stack tVar) = do
    d <- readProcess "stack" ["ls", "dependencies"] ""
    let pd = parse d
    atomically . writeTVar tVar $ pd
    TIO.putStrLn . concatList "Dependencies:" $ pd
    return $ Stack tVar
    where
      parse = P.map (pack . P.head . S.splitOn " ") . DL.lines

upgradeOutFilter :: [Text] -> [Text]
upgradeOutFilter =
  P.map pack .
  P.filter (not . P.null) .
  P.filter (\s -> count "..." (pack s) == 0) . P.map unpack

concatList :: Text -> [Text] -> Text
concatList title xs = T.concat $ title : DL.intersperse " " xs
