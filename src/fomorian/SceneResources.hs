{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGe TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Fomorian.SceneResources 
  (
    DataSource(..),
    Resource(..),
    BasicDataSourceTypes,
    BasicResourceTypes,
    GeometryResource(..),
    VertexAttribute(..),
    VertexDataType(..),
    loadBasicData
  )
  where

import Control.Monad

import Data.Hashable
import Foreign.Storable (Storable, sizeOf)

import qualified Data.ByteString.Lazy as B

import Data.Row
import Data.Hashable

import qualified Data.Map.Strict as M

import GHC.Generics
import System.FilePath


import Linear

import Fomorian.GraphicsLoaders.ProcessWavefront (OBJBufferRecord(..), loadWavefrontOBJFile)

newtype DataSource r = DataSource { unDataSource :: (Var r) }
  deriving (Generic)

instance (Forall r Eq) => Eq (DataSource r) where
  (DataSource a) == (DataSource b) =  (a == b)

instance (Forall r Show) => Show (DataSource r) where
  show (DataSource a) = "[DataSource " ++ show a ++ "]"
  
instance (Forall r Eq, Forall r Ord) => Ord (DataSource r) where
  compare (DataSource a) (DataSource b) = compare a b

instance Hashable (Var r) => Hashable (DataSource r) where
  hashWithSalt s (DataSource x) = hashWithSalt s x


newtype Resource r = Resource { unResource :: (Var r) }
  deriving (Generic)

instance (Forall r Eq) => Eq (Resource r) where
  (Resource r) == (Resource s) =  (r == s)
instance (Forall r Show) => Show (Resource r) where
  show (Resource a) = "[Resource " ++ show a ++ "]"
instance (Forall r Eq, Forall r Ord) => Ord (Resource r) where
  compare (Resource a) (Resource b) = compare a b

instance Hashable (Var r) => Hashable (Resource r) where
  hashWithSalt s (Resource x) = hashWithSalt s x


type BasicDataSourceTypes =
     ("coordinates2d"    .== [(Float,Float)])
  .+ ("coordinates3d"    .== [(Float,Float,Float)])
  .+ ("rawVertexAttribs" .== ([V3 Float],[Int]))
  .+ ("wavefrontPath" .== FilePath)
  .+ ("shaderPath"    .== FilePath)
  .+ ("texturePath"   .== FilePath)

type BasicResourceTypes =
     ("vertexPositions"  .== GeometryResource [V3 Float] [Int] VertexAttribute)
  .+ ("vertexData"       .== GeometryResource [Float] [Int] VertexAttribute)
  .+ ("shaderBytes"      .== B.ByteString)
  .+ ("textureBytes"     .== B.ByteString)

-- | backend-agnostic representation of vertex data type
data VertexDataType =
    VertexFloat 
  | VertexInt
  deriving (Eq, Show)

-- | backend-agnostic version of Vertex Attributes: convert to your backend-specific representation
data VertexAttribute =
  VertexAttribute {
    numComponents :: Int,
    dataType :: VertexDataType,
    stride :: Int, -- stride in bytes
    offset :: Int  -- offset in bytes
  }
  deriving (Eq, Show)

-- | Generic representation of geometry. Contains buffer info and a list of vertex attributes.
data GeometryResource b i atr =
  GeometryResource {
    vBuffer :: b,
    indexBuffer :: (Maybe i),
    elementCount :: Int,
    attributeMap ::  (M.Map String atr)
  }
  deriving (Eq, Show)


vertex2ToGeometry :: [(Float,Float)] -> GeometryResource [V3 Float] [Int] VertexAttribute
vertex2ToGeometry ffs = GeometryResource v2s Nothing (length ffs) attribs
  where
    v2s = fmap (\(x,y) -> V3 x y 0) ffs
    attribs = M.fromList $ [("position",VertexAttribute 3 VertexFloat 12 0)]

vertex3ToGeometry :: [(Float,Float,Float)] -> GeometryResource [V3 Float] [Int] VertexAttribute
vertex3ToGeometry f3s = GeometryResource v3s Nothing (length f3s) attribs
  where
    v3s = fmap (\(x,y,z) -> V3 x y z) f3s
    attribs = M.fromList $ [("position",VertexAttribute 3 VertexFloat 12 0)]

v3IndexToGeometry :: [V3 Float] -> [Int] -> GeometryResource [V3 Float] [Int] VertexAttribute
v3IndexToGeometry v3s ixs = GeometryResource v3s (Just ixs) (length v3s) attribs
  where
    attribs = M.fromList $ [("position",VertexAttribute 3 VertexFloat 12 0)]

flattenWavefrontVertex :: OBJBufferRecord -> [Float]
flattenWavefrontVertex (OBJBufferRecord objR) =
  let (V3 x y z) = objR .! #pos3
      (V2 u v)   = objR .! #texCoord
      (V3 nx ny nz) = objR .! #normal
  in [x,y,z,u,v,nx,ny,nz]
  
wavefrontGeometry :: FilePath -> IO (GeometryResource [Float] [Int] VertexAttribute)
wavefrontGeometry fp = do
    r <- loadWavefrontOBJFile ("resources" </> "geometry" </> fp)
    case r of
      Left e -> error e
      Right (vertdata, indexdata) -> do
        let stride = fromIntegral $ sizeOf (undefined :: OBJBufferRecord)
        let floatSize = fromIntegral $ sizeOf (undefined :: Float)
        let attribs = M.fromList [
              ("position",VertexAttribute 3 VertexFloat stride 0),
              ("texCoord",VertexAttribute 2 VertexFloat stride (3*floatSize)),
              ("normal",  VertexAttribute 3 VertexFloat stride (5*floatSize))
              ]
        let rawVerts = concatMap flattenWavefrontVertex vertdata
        return $ GeometryResource rawVerts (Just indexdata) (length indexdata) attribs


loadBasicData ::  DataSource BasicDataSourceTypes -> IO (Resource BasicResourceTypes)
loadBasicData (DataSource bd) = fmap Resource $ switch bd $
     #coordinates2d     .== (\v2s -> return $ IsJust #vertexPositions (vertex2ToGeometry v2s))
  .+ #coordinates3d     .== (\v3s -> return $ IsJust #vertexPositions (vertex3ToGeometry v3s))
  .+ #rawVertexAttribs  .== (\(v3s,ixs) -> return $ IsJust #vertexPositions (v3IndexToGeometry v3s ixs))
  .+ #wavefrontPath     .== (\fp -> do g <- wavefrontGeometry fp; return $ IsJust #vertexData g)
  .+ #shaderPath        .== (\fp -> do b <- B.readFile ("resources" </> "shaders" </> fp); return (IsJust #shaderBytes b))
  .+ #texturePath       .== (\fp -> do b <- B.readFile ("resources "</> "textures" </> fp); return (IsJust #textureBytes b))

