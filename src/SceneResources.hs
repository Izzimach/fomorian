{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module SceneResources where

import Linear
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (GLfloat, ($=))
import qualified Graphics.GLUtil as GLU

import Data.Vinyl
import qualified Graphics.VinylGL as VGL
import Data.Word (Word32)
import System.FilePath ((</>))
import Control.Monad (foldM, (>=>))

import qualified Data.Set as S
import qualified Data.Map as M

data VertexSourceData = V2Data [V2 GLfloat]
  | V3Data [V3 GLfloat]
  | IndexData  [Int]
  deriving (Ord, Eq)

type Pos2 = '("pos2", V2 GLfloat)
type Pos3 = '("pos3", V3 GLfloat)
type VertIndex = '("index", Int)


  
data ResourceMap = Resources { 
    shaders :: (M.Map String GLU.ShaderProgram),
    v2Buffers :: (M.Map VertexSourceData (VGL.BufferedVertices '[Pos2], Word32)),
    v3Buffers :: (M.Map VertexSourceData (VGL.BufferedVertices '[Pos3], Word32)),
    indexBuffers :: (M.Map VertexSourceData (GL.BufferObject, Word32)),
    textures :: (M.Map String GL.TextureObject)
  }

emptyResourceMap :: ResourceMap
emptyResourceMap = Resources M.empty M.empty M.empty M.empty M.empty

data ResourceList = ResourceList { 
    shaderfiles :: S.Set String,
    vertexfiles :: S.Set VertexSourceData,
    texturefiles :: S.Set String
  }

emptyResourceList :: ResourceList
emptyResourceList = ResourceList S.empty S.empty S.empty

mergeResourceLists :: ResourceList -> ResourceList -> ResourceList
mergeResourceLists r1 r2 = ResourceList
  {
    shaderfiles = S.union (shaderfiles r1) (shaderfiles r2),
    vertexfiles = S.union (vertexfiles r1) (vertexfiles r2),
    texturefiles = S.union (texturefiles r1) (texturefiles r2)
  }

initShader :: String -> IO GLU.ShaderProgram
initShader shadername = GLU.loadShaderFamily $ "resources" </> "shaders" </> shadername

buildVertices2 :: [V2 GLfloat]  -> IO (VGL.BufferedVertices '[Pos2])
buildVertices2 v2s = VGL.bufferVertices $ fmap (\x -> #pos2 =: x :& RNil) v2s

buildVertices3 :: [V3 GLfloat] -> IO (VGL.BufferedVertices '[Pos3])
buildVertices3 v3s = VGL.bufferVertices $ fmap (\x -> #pos3 =: x :& RNil) v3s

buildIndices :: [Int] -> IO (GL.BufferObject)
buildIndices idxs = GLU.bufferIndices $ fmap (fromIntegral) idxs


-- | @loadBuffers vs r@ will load any of the vertex data arrays in @vs@ that are not
--   already contained in @r@
loadBuffers :: [VertexSourceData] -> ResourceMap -> IO ResourceMap
loadBuffers vs r = foldM loadBufferM r vs
  where
    loadBufferM racc vv@(V2Data v2) = case (M.lookup vv (v2Buffers racc)) of
                                        Just _ -> return racc
                                        Nothing -> do vb <- buildVertices2 v2
                                                      let vblen = fromIntegral $ length v2
                                                      return $ racc { v2Buffers = M.insert vv (vb, vblen) (v2Buffers r) }
    loadBufferM racc vv@(V3Data v3) = case (M.lookup vv (v3Buffers racc)) of
                                        Just _ -> return racc
                                        Nothing -> do vb <- buildVertices3 v3
                                                      let vblen = fromIntegral $ length v3
                                                      return $ racc { v3Buffers = M.insert vv (vb, vblen) (v3Buffers r) }
    loadBufferM racc vv@(IndexData i) = case (M.lookup vv (indexBuffers racc)) of
                                          Just _ -> return racc
                                          Nothing -> do ib <- buildIndices i
                                                        let iblen = fromIntegral $ length i
                                                        return $ racc { indexBuffers = M.insert vv (ib, iblen) (indexBuffers r) }

loadTextures :: [String] -> ResourceMap -> IO ResourceMap
loadTextures tfs r = foldM loadTextureM r tfs
  where
    loadTextureM racc tf = do res <- GLU.readTexture ("resources" </> "textures" </> tf)
                              case res of
                                Left x -> error x
                                Right t -> do GL.textureFilter GL.Texture2D $= ((GL.Nearest,Nothing), GL.Nearest)
                                              GLU.texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
                                              return $ racc { textures = M.insert tf t (textures racc) }
                                res <- GLU.readTexture ("resources" </> "textures" </> tf)

loadShaders :: [String] -> ResourceMap -> IO ResourceMap
loadShaders ss r = foldM loadShaderM r ss
  where
    loadShaderM racc s = case (M.lookup s (shaders racc)) of
                           Just _ -> return racc
                           Nothing -> do sh <- initShader s
                                         return $ racc { shaders = M.insert s sh (shaders racc) }

-- | Load resources synchronously. In @loadResources rl rp@
--   the value @rl@ is a ResourceLIst of resources needed and
--   @rp@ holds the resources currently loaded. Any resources
--   specified in @rl@ that are not already available in @rp@
--   are loaded and add to @rp@. The return value is an update
--   @rp@ that now holds all the resources specified in @rl@
--   (assuming no errors)

loadResources :: ResourceList -> ResourceMap -> IO ResourceMap
loadResources neededresources loadedresources = do
  let lb = loadBuffers (S.toList . vertexfiles $ neededresources)
  let lt = loadTextures (S.toList . texturefiles $ neededresources)
  let ls = loadShaders (S.toList . shaderfiles $ neededresources)
  (lb >=> lt >=> ls) loadedresources