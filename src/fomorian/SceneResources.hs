{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
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
    BasicResource,
    BasicDataSource,
    BasicDataSourceTypes,
    BasicResourceTypes,
    GeometryResource(..),
    loadWithPrebuilts,
    vertex2ToGeometry,
    wavefrontGeometry,
    BufferFill(..)
  )
  where

import Data.Maybe (fromMaybe)
import Data.Hashable
import Data.Word (Word32)
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE

import Data.Row
import Data.Row.Variants

import Foreign.Storable (sizeOf, Storable)
import Foreign.Ptr
import Foreign.Marshal.Array (pokeArray)

import GHC.Generics
import GHC.StableName

import System.FilePath

import Linear

import Fomorian.StorableLayout
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
     ("userSource"     .== Text)
  .+ ("wavefrontPath"  .== FilePath)
  .+ ("shaderPath"     .== FilePath)
  .+ ("texturePath"    .== FilePath)

type BasicDataSource = DataSource BasicDataSourceTypes

type BasicResourceTypes =
     ("vertexPositions"  .== GeometryResource [V3 Float] [Int] DataLayoutMap)
  .+ ("vertexFloats"     .== GeometryResource [Float] [Int] DataLayoutMap)
  .+ ("vertexFunction"   .== GeometryResource BufferFill BufferFill DataLayoutMap)
  .+ ("shaderBytes"      .== B.ByteString)
  .+ ("textureBytes"     .== B.ByteString)

type BasicResource = Resource BasicResourceTypes


-- | Generic representation of geometry. Contains buffer info and a list of vertex attributes.
data GeometryResource b i atr =
  GeometryResource {
    vBuffer :: b,             -- ^ vertex data or a handle to the buffer that holds it.
    indexBuffer :: Maybe i,   -- ^ index data, if this is 'Nothing' then use a non-indexed draw.
    elementCount :: Int,      -- ^ the raw count of vertex or index elements. Still need to divide by 3 or whatever depending on your primitive.
    attributeMap :: atr       -- ^ a 'Map' describing the data format. specifics vary by platform. 'DataLayoutMap' is supposed to be the neutral representation.
  }
  deriving (Eq, Show)

-- | An opaque buffer that holds generic vertex data. Hold the size (in bytes) needed for the data
--   and a function that will fill the specified memory location with vertex data.
data BufferFill = BufferFill Int (Ptr Int -> IO ())

mkBufferFill :: forall x. (Storable x) => NE.NonEmpty x -> BufferFill
mkBufferFill xs = BufferFill bSize fillFunc
  where
    bSize = NE.length xs * sizeOf (NE.head xs)
    fillFunc = \pI -> pokeArray (castPtr pI) (NE.toList xs)

vertex2ToGeometry :: [(Float,Float)] -> GeometryResource [V3 Float] [Int] DataLayoutMap
vertex2ToGeometry ffs = GeometryResource v2s Nothing (length ffs) attribs
  where
    v2s = fmap (\(x,y) -> V3 x y 0) ffs
    attribs = DataLayoutMap 12 $ M.singleton "position" (DataLayoutElement 3 NeutralFloat32 0)

vertex3ToGeometry :: [(Float,Float,Float)] -> GeometryResource [V3 Float] [Int] DataLayoutMap
vertex3ToGeometry f3s = GeometryResource v3s Nothing (length f3s) attribs
  where
    v3s = fmap (\(x,y,z) -> V3 x y z) f3s
    attribs = DataLayoutMap 12 $ M.singleton "position" (DataLayoutElement 3 NeutralFloat32 0)

v3IndexToGeometry :: [V3 Float] -> [Int] -> GeometryResource [V3 Float] [Int] DataLayoutMap
v3IndexToGeometry v3s ixs = GeometryResource v3s (Just ixs) (length v3s) attribs
  where
    attribs = DataLayoutMap 12 $ M.singleton "position" (DataLayoutElement 3 NeutralFloat32 0)

flattenWavefrontVertex :: OBJBufferRecord -> [Float]
flattenWavefrontVertex (OBJBufferRecord objR) =
  let (V3 x y z) = objR .! #pos3
      (V2 u v)   = objR .! #texCoord
      (V3 nx ny nz) = objR .! #normal
  in [x,y,z,u,v,nx,ny,nz]
  
wavefrontGeometry :: FilePath -> IO (GeometryResource [Float] [Int] DataLayoutMap)
wavefrontGeometry fp = do
  r <- loadWavefrontOBJFile ("resources" </> "geometry" </> fp)
  case r of
    Left e -> error e
    Right (vertdata, indexdata) -> do
      let str = fromIntegral $ sizeOf (undefined :: OBJBufferRecord)
      let floatSize = fromIntegral $ sizeOf (undefined :: Float)
      let attribs = DataLayoutMap str $ M.fromList [
            ("position",DataLayoutElement 3 NeutralFloat32 0),
            ("texCoord",DataLayoutElement 2 NeutralFloat32 (3*floatSize)),
            ("normal",  DataLayoutElement 3 NeutralFloat32 (5*floatSize))
            ]
      let rawVerts = concatMap flattenWavefrontVertex vertdata
      return $ GeometryResource rawVerts (Just indexdata) (length indexdata) attribs

-- | Build some generic vertex data using a list of records. data layout is generated automatically with generics, and
--   the underlying vertex data type is hidden under the BufferFill.
autoBuildBufferFill :: forall x. (Storable x, Generic x, RecordLayoutG (Rep x)) => NE.NonEmpty x -> Maybe (NE.NonEmpty Word32) -> GeometryResource BufferFill BufferFill DataLayoutMap
autoBuildBufferFill vs mix = GeometryResource (mkBufferFill vs) (fmap mkBufferFill mix) eCount (autoRecordLayout $ NE.head vs)
  where
    eCount = case mix of
               Nothing -> NE.length vs
               Just ix -> length ix


loadWithPrebuilts :: M.Map Text (Var BasicResourceTypes) -> BasicDataSource -> IO BasicResource
loadWithPrebuilts prebuiltMap (DataSource bd) = fmap Resource $ switch bd $
     #userSource     .== (\l -> return $ fromMaybe undefined $ M.lookup l prebuiltMap)
  .+ #wavefrontPath  .== (\fp -> do g <- wavefrontGeometry fp; return $ IsJust #vertexFloats g)
  .+ #shaderPath     .== (\fp -> do b <- B.readFile ("resources" </> "shaders" </> fp); return (IsJust #shaderBytes b))
  .+ #texturePath    .== (\fp -> do b <- B.readFile ("resources "</> "textures" </> fp); return (IsJust #textureBytes b))

