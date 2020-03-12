{-# LANGUAGE RankNTypes #-}

module Session where

import           Control.Concurrent.MVar
import           Control.Monad.State
import           Control.Lens
import qualified Data.HashMap.Strict           as Map

import           Types

initCacheStore :: IO CacheStore
initCacheStore = newMVar Map.empty

allValues :: CacheStore -> IO [IDPData]
allValues store = do
  m1 <- tryReadMVar store
  return $ maybe [] Map.elems m1

removeKey :: CacheStore -> IDPLabel -> IO ()
removeKey store idpKey = do
  m1 <- takeMVar store
  let m2 = Map.update updateIdpData idpKey m1
  putMVar store m2
  where updateIdpData idpD = Just $ idpD { loginUser = Nothing }

lookupKey :: CacheStore -> IDPLabel -> IO (Maybe IDPData)
lookupKey store idpKey = do
  m1 <- tryReadMVar store
  return (Map.lookup idpKey =<< m1)

insertIDPData :: CacheStore -> IDPData -> IO ()
insertIDPData store val =
  modifyMVar_ store $ execStateT $ at (idpDisplayLabel val) ?= val
