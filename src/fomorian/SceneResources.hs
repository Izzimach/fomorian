{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGe TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Fomorian.SceneResources 
  (--ResourceMap,
   -- get various resources from the resource map, used by a renderer invoke node
   --shaders, v2Buffers, v3Buffers, indexBuffers, textures, texCoordBuffers, objFileBuffers,
   --emptyResourceMap,
   --loadResources,
   syncLoadResource,
   syncUnloadResource,
   syncLoadAll,
   syncUnloadAll,
   GeneralDataSource(..),
   GeometryDataSource(..),
   MaterialDataSource(..),
   Resources(..),
   lookupResource,
   insertResource,
   deleteResource,
   emptyResources
  )
  where

import Control.Monad

import Data.Hashable
import Data.ByteString

import qualified Data.HashMap.Strict as H

import GHC.Generics

import Linear

data GeometryDataSource =
    RawV2 [V2 Float]
  | RawV3 [V3 Float]
  | RawIndexedV3 [V3 Float] [Int]
  | OBJFile String
  deriving (Eq, Show, Ord, Generic)

data MaterialDataSource =
    ShaderPath String
  | TexturePath String
  | TextureBytes ByteString
  deriving (Eq, Show, Ord, Generic)

data GeneralDataSource =
    GeometryData GeometryDataSource
  | MaterialData MaterialDataSource
  deriving (Eq, Show, Generic, Ord)

instance Hashable GeometryDataSource
instance Hashable MaterialDataSource
instance Hashable GeneralDataSource

newtype Resources d r = Resources { unResources :: (H.HashMap d r) }
  deriving (Eq, Show)

emptyResources :: Resources d r
emptyResources = Resources H.empty

lookupResource :: (Eq d,Hashable d) => d -> Resources d r -> Maybe r
lookupResource d (Resources m) = H.lookup d m

insertResource :: (Eq d, Hashable d) => d -> r -> Resources d r -> Resources d r
insertResource d r (Resources m) = Resources $ H.insert d r m

deleteResource :: (Eq d, Hashable d) => d -> Resources d r -> Resources d r
deleteResource d (Resources m) = Resources $ H.delete d m

syncLoadResource :: (Eq d, Hashable d) => (d -> IO r) -> Resources d r -> d -> IO (Resources d r)
syncLoadResource loader rm d =
  case (lookupResource d rm) of
    Just _ -> return rm    -- ^ if already loaded do nothing
    Nothing ->             -- ^ resource not found so we must load it
      do
        r <- loader d
        return (insertResource d r rm)

syncUnloadResource :: (Eq d, Hashable d) => (r -> IO ()) -> Resources d r -> d -> IO (Resources d r)
syncUnloadResource unloader rm d =
  case (lookupResource d rm) of
    Nothing -> return rm
    Just r ->
      do
        unloader r
        return (deleteResource d rm)

syncLoadAll :: (Eq d, Hashable d) => (d -> IO r) -> Resources d r -> [d] -> IO (Resources d r)
syncLoadAll loader rm d = foldM (syncLoadResource loader) rm d

syncUnloadAll :: (r -> IO ()) -> Resources d r -> IO ()
syncUnloadAll unloader (Resources m) =
  do mapM_ unloader m
     return ()
