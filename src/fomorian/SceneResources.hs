{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module Fomorian.SceneResources 
  (--ResourceMap,
   -- get various resources from the resource map, used by a renderer invoke node
   --shaders, v2Buffers, v3Buffers, indexBuffers, textures, texCoordBuffers, objFileBuffers,
   --emptyResourceMap,
   --loadResources,
   syncLoadResource,
   syncUnloadResource,
   loadGLResource,
   unloadGLResource,
   GLDataSource(..),
   GLResourceRecord(..),
   Resources(..),
   lookupResource,
   emptyResources
  )
  where

import Data.Row
import Data.Row.Records

import Data.Word (Word32)
import Data.Hashable

import qualified Data.Set as S
import qualified Data.HashMap.Strict as H

import GHC.Generics
import System.FilePath ((</>))

import Control.Monad (foldM, (>=>))

import Linear

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (GLfloat, ($=))
import qualified Graphics.GLUtil as GLU
import qualified Graphics.GLUtil.Shaders (loadShader)

import Fomorian.ProcessWavefront (OBJBufferFormat, loadWavefrontOBJFile)
import Foreign.Storable (Storable)

data GLDataSource =
    RawV2 [V2 Float]
  | RawV3 [V3 Float]
  | RawIndex [Int]
  | ShaderFiles String String
  | TextureFile String
  deriving (Eq, Show, Generic, Ord)

instance Hashable GLDataSource


data GLResourceType =
    GLBufferV2
  | GLBufferV3
  | GLBufferIndex
  | GLShader
  | GLTexture
  deriving (Eq, Show, Generic)

data GLResourceRecord =
    GLResourceBO GL.BufferObject GL.GLint
  | GLShaderProgram GLU.ShaderProgram
  | GLTextureObject GL.TextureObject

newtype Resources d r = Resources (H.HashMap d r)

emptyResources :: Resources d r
emptyResources = Resources H.empty

lookupResource :: (Eq d,Hashable d) => Resources d r -> d -> Maybe r
lookupResource (Resources m) d = H.lookup d m

syncLoadResource :: (GLDataSource -> IO GLResourceRecord) -> GLDataSource -> Resources GLDataSource GLResourceRecord -> IO (Resources GLDataSource GLResourceRecord)
syncLoadResource loader d rm@(Resources m) =
  case (H.lookup d m) of
    Just _ -> return rm    -- ^ if already loaded do nothing
    Nothing ->             -- ^ resource not found so we must load it
      do
        r <- loader d
        let m' = H.insert d r m
        return (Resources m')

syncUnloadResource :: (GLResourceRecord -> IO ()) -> GLDataSource -> Resources GLDataSource GLResourceRecord -> IO (Resources GLDataSource GLResourceRecord)
syncUnloadResource unloader d rm@(Resources m) =
  case (H.lookup d m) of
    Nothing -> return rm
    Just r ->
      do
        unloader r
        let m' = H.delete d m
        return (Resources m')

syncUnloadAll :: (GLResourceRecord -> IO ()) -> Resources GLDataSource GLResourceRecord -> IO ()
syncUnloadAll unloader (Resources m) =
  do mapM_ unloader m
     return ()

loadBuffer :: (Storable a) => GL.BufferTarget -> [a] -> IO GLResourceRecord
loadBuffer t d = do r <- GLU.makeBuffer t d
                    return (GLResourceBO r (fromIntegral $ length d))

loadGLResource :: GLDataSource -> IO GLResourceRecord
loadGLResource (RawV2 v2data) = loadBuffer GL.ArrayBuffer v2data
loadGLResource (RawV3 v3data) = loadBuffer GL.ArrayBuffer v3data
loadGLResource (RawIndex ixdata) = loadBuffer GL.ElementArrayBuffer ixdata
-- for shaders we need seperate files for vertex and fragment shaders
loadGLResource (ShaderFiles vert frag) = do s <- GLU.simpleShaderProgram ("./resources" </> "shaders" </> vert) ("./resources" </> "shaders" </> frag)
                                            return (GLShaderProgram s)
loadGLResource (TextureFile filePath) = do v <- GLU.readTexture filePath
                                           case v of
                                             Left err -> error ("Error loading texture " ++ filePath ++ ": " ++ err)
                                             Right obj -> do GL.textureFilter GL.Texture2D $= ((GL.Nearest,Nothing), GL.Nearest)
                                                             GLU.texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
                                                             return (GLTextureObject obj)

unloadGLResource :: GLResourceRecord -> IO ()
unloadGLResource (GLResourceBO o l) = undefined
unloadGLResource (GLShaderProgram s) = undefined
unloadGLResource (GLTextureObject o) = undefined


{-
buildOBJBuffers :: String -> IO (
                                    (VGL.BufferedVertices OBJBufferFormat, Word32),
                                    (GL.BufferObject, Word32)
                                  )
buildOBJBuffers filename = do
  r <- loadWavefrontOBJFile ("resources" </> "geometry" </> filename)
  case r of
    Left e -> error e
    Right (vertdata, indexdata) -> do
      let vlength = fromIntegral $ length vertdata
      vbuf <- VGL.bufferVertices vertdata
      let ilength = fromIntegral $ length indexdata
      ibuf <- GLU.bufferIndices $ fmap fromIntegral indexdata
      return ((vbuf, vlength),(ibuf, ilength))

-}
