{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module Fomorian.SceneResources 
  (VertexSourceData(..),
   ResourceMap,
   -- get various resources from the resource map, used by a renderer invoke node
   shaders, v2Buffers, v3Buffers, indexBuffers, textures, texCoordBuffers, objFileBuffers,
   emptyResourceMap,
   loadResources,
   mergeResourceLists,
   ResourceList(..),
   emptyResourceList
  )
  where

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
import Fomorian.ProcessWavefront (OBJBufferFormat, loadWavefrontOBJFile)

{- |
   Vertex buffer and index buffer resources are specified as a VertexSourceData.
   The constructor determines the buffer format. For example, to specify a
   vertex buffer of 2d values the code would look like @V2Data [V2 0 0, V2 1 0, V2 1 1]@
-}
data VertexSourceData = 
    V2Data [V2 GLfloat]
  | T2Data [V2 GLfloat]
  | V3Data [V3 GLfloat]
  | OBJFile String
  | IndexData  [Int]
  deriving (Ord, Eq)


-- types so that vinyl-gl can check attribute data
type Pos2 = '("pos2", V2 Float)
type Pos3 = '("pos3", V3 Float)
type TexCoord = '("texCoord", V2 Float)
type Normal = '("normal", V3 Float)
type VertIndex = '("index", Int)


{- |
   The resource map holds all loaded resources. Scene nodes specify resources
   in varying ways depending on resource: String/filepaths for shader programs, VertexSourceData
   for vertex, Strings/filepaths for textures
-}
data ResourceMap = Resources { 
    shaders :: (M.Map String GLU.ShaderProgram),
    v2Buffers :: (M.Map VertexSourceData (VGL.BufferedVertices '[Pos2], Word32)),
    v3Buffers :: (M.Map VertexSourceData (VGL.BufferedVertices '[Pos3], Word32)),
    texCoordBuffers :: (M.Map VertexSourceData (VGL.BufferedVertices '[TexCoord], Word32)),
    objFileBuffers :: (M.Map VertexSourceData (
                                               (VGL.BufferedVertices OBJBufferFormat, Word32),
                                               (GL.BufferObject, Word32)
                                              )
                      ),
    indexBuffers :: (M.Map VertexSourceData (GL.BufferObject, Word32)),
    textures :: (M.Map String GL.TextureObject)
  }

emptyResourceMap :: ResourceMap
emptyResourceMap = Resources M.empty M.empty M.empty M.empty M.empty M.empty M.empty

--
-- |Just a list of resources that need to be loaded or created.  The resource
-- |manager looks at this to figure what to load. When loaded, resources are added
-- |to a 'ResourceMap'
--
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

instance Monoid ResourceList where
  mempty = emptyResourceList
  mappend = mergeResourceLists

  
--
-- functions to load/create graphics resources
--

initShader :: String -> IO GLU.ShaderProgram
initShader shadername = GLU.loadShaderFamily $ "resources" </> "shaders" </> shadername

buildVertices2 :: [V2 GLfloat]  -> IO (VGL.BufferedVertices '[Pos2])
buildVertices2 v2s = VGL.bufferVertices $ fmap (\x -> #pos2 =: x :& RNil) v2s

buildVertices3 :: [V3 GLfloat] -> IO (VGL.BufferedVertices '[Pos3])
buildVertices3 v3s = VGL.bufferVertices $ fmap (\x -> #pos3 =: x :& RNil) v3s

buildTexCoords :: [V2 GLfloat] -> IO (VGL.BufferedVertices '[TexCoord])
buildTexCoords t2s = VGL.bufferVertices $ fmap (\x -> #texCoord =: x :& RNil) t2s

buildIndices :: [Int] -> IO (GL.BufferObject)
buildIndices idxs = GLU.bufferIndices $ fmap (fromIntegral) idxs

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



-- | @loadBuffers vs r@ will load any of the vertex data arrays in @vs@ that are not
-- | already contained in @r@
loadBuffers :: [VertexSourceData] -> ResourceMap -> IO ResourceMap
loadBuffers vs r = foldM loadBufferM r vs
  where
    loadBufferM racc vv@(V2Data v2) = case (M.lookup vv (v2Buffers racc)) of
      Just _ -> return racc
      Nothing -> do vb <- buildVertices2 v2
                    let vblen = fromIntegral $ length v2
                    return $ racc { v2Buffers = M.insert vv (vb, vblen) (v2Buffers r) }
    loadBufferM racc vv@(T2Data v2) = case (M.lookup vv (texCoordBuffers racc)) of
      Just _ -> return racc
      Nothing -> do vb <- buildTexCoords v2
                    let vblen = fromIntegral $ length v2
                    return $ racc { texCoordBuffers = M.insert vv (vb, vblen) (texCoordBuffers r) }
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
    loadBufferM racc obj@(OBJFile f) = case (M.lookup obj (objFileBuffers racc)) of
                                         Just _ -> return racc
                                         Nothing -> do objb <- buildOBJBuffers f
                                                       return racc { objFileBuffers = M.insert obj objb (objFileBuffers racc)}
                                                       

loadTextures :: [String] -> ResourceMap -> IO ResourceMap
loadTextures tfs r = foldM loadTextureM r tfs
  where
    loadTextureM racc tf = case (M.lookup tf (textures racc)) of
                              Just _  -> return racc
                              Nothing -> do
                                res <- GLU.readTexture ("resources" </> "textures" </> tf)
                                case res of
                                  Left x -> error x
                                  Right t -> do GL.textureFilter GL.Texture2D $= ((GL.Nearest,Nothing), GL.Nearest)
                                                GLU.texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
                                                return $ racc { textures = M.insert tf t (textures racc) }

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
