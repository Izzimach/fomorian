{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fomorian.OpenGLCommand where

import qualified GHC.Generics as GHC

import Linear

import Control.Monad
import Control.Monad.IO.Class

import Data.Functor
import Data.Functor.Foldable
import Data.Kind (Constraint)

import Data.Row
import Data.Row.Records

import qualified Data.Set as S
import qualified Data.Map as M

import Foreign.Ptr (nullPtr)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (GLfloat, ($=))
import qualified Graphics.GLUtil as GLU
import qualified Graphics.GLUtil.Shaders (loadShader)
import Graphics.GLUtil.ShaderProgram (setUniform)

import Fomorian.SceneNode
import Fomorian.SceneResources

data OpenGLTarget
type instance (InvokeReq OpenGLTarget sreq) = (HasType "shader" GLDataSource sreq,
                                                HasType "vertices" GLDataSource sreq)
type instance (FrameReq OpenGLTarget dreq) = (HasType "modelViewMatrix" (M44 Float) dreq,
                                               HasType "projectionMatrix" (M44 Float) dreq)

drawGLAlgebra :: (MonadIO m) => SceneNode r OpenGLTarget (DrawCmd r m ()) -> DrawCmd r m ()
drawGLAlgebra (Invoke x) = liftIO $ putStrLn $ show (x .! #shader)
drawGLAlgebra (Group children) = foldl (>>) (DC $ \_ -> return ()) children
drawGLAlgebra (Transformer f gr) = DC $ \r ->
    let s = f r
        scmd = cata drawGLAlgebra gr
    in runDC scmd s

drawGL :: SceneGraph r OpenGLTarget -> DrawCmd r IO ()
drawGL = cata drawGLAlgebra

{-
testScene :: (FrameReq OpenGLTarget r) => SceneGraph r OpenGLTarget
testScene = group [invoke (#shader .== (ShaderFiles "ack" "argh") .+
                           #vertices .== (RawV3 [V3 0 0 1])), 
                   invoke (#shader .== (ShaderFiles "ack" "argh") .+
                           #vertices .== (RawV3 [V3 0 0 1]))]
-}


type GLResourceList = S.Set GLDataSource

oglResourcesAlgebra :: SceneNode dreq OpenGLTarget GLResourceList -> GLResourceList
oglResourcesAlgebra (Invoke x) = let sh = x .! #shader
                                     v  = x .! #vertices
                                 in S.fromList [sh, v]
oglResourcesAlgebra (Group cmds) = foldl S.union S.empty cmds
oglResourcesAlgebra (Transformer t gr) = oglResourcesScene gr

oglResourcesScene :: SceneGraph dreq OpenGLTarget -> GLResourceList
oglResourcesScene sg = cata oglResourcesAlgebra sg

-- | Given a scene and a set of already-loaded resources, makes sure all the resources are loaded for this scene. Synchronously loads
--   any resources needed for the scene.
loadResourcesScene :: SceneGraph dreq OpenGLTarget -> Resources GLDataSource GLResourceRecord -> IO (Resources GLDataSource GLResourceRecord)
loadResourcesScene sg oldres =
  let needsResources = oglResourcesScene sg
      loadX = flip (syncLoadResource loadGLResource)
  in foldM loadX oldres needsResources


data OpenGLCommand
type instance (InvokeReq OpenGLCommand sreq) = (HasType "shader" GLU.ShaderProgram sreq,
                                                HasType "vertices" GL.BufferObject sreq,
                                                HasType "vertexCount" GL.GLint sreq,
                                                HasType "indices" (Maybe GL.BufferObject) sreq)
type instance (FrameReq OpenGLCommand dreq) = (HasType "modelViewMatrix" (M44 Float) dreq,
                                               HasType "projectionMatrix" (M44 Float) dreq)

-- | Given an OpenGLTarget scene graph and loaded resources, builds an OpenGlCommand scene graph which can be directly
--   converted into a monad to draw the scene
oglCommandAlgebra :: Resources GLDataSource GLResourceRecord -> SceneNode dreq OpenGLTarget (SceneGraph dreq OpenGLCommand) -> SceneGraph dreq OpenGLCommand
oglCommandAlgebra r (Invoke x) =
  let shaderRecord = lookupResource r (x .! #shader)
      vertexObject = lookupResource r (x .! #vertices)
  in case (shaderRecord, vertexObject) of
       (Just (GLShaderProgram s), Just (GLResourceBO v l)) -> Fix $ Invoke (#shader .== s .+ #vertices .== v .+ #vertexCount .== l .+ #indices .== Nothing)
       (_,_)                                             -> undefined
oglCommandAlgebra _ (Group cmds) = Fix $ Group cmds
oglCommandAlgebra r (Transformer t gr) = Fix $ Transformer t (oglToCommand r gr)

oglToCommand :: Resources GLDataSource GLResourceRecord -> SceneGraph dreq OpenGLTarget -> SceneGraph dreq OpenGLCommand
oglToCommand r sg = cata (oglCommandAlgebra r) sg


--
-- Drawing with OpenGL - shader parameters must be Uniform-valid, which
-- means they are GLfloat, GLint, or vectors (V2,V3,V4) or matrices
--

invokeGL :: (InvokeReq OpenGLCommand r, FrameReq OpenGLCommand dreq) => Rec r -> DrawCmd dreq IO ()
invokeGL r = DC $ \dr ->
  do
    let s = r .! #shader
    let vs = r .! #vertices
    let vc = r .! #vertexCount
    let attrib = GL.vertexAttribArray (GL.AttribLocation 0)
    let attribptr = GL.vertexAttribPointer (GL.AttribLocation 0)
    GL.currentProgram $= Just (GLU.program s)
    attrib $= GL.Enabled
    attribptr $= (GL.ToFloat, GL.VertexArrayDescriptor (2 :: GL.GLint) GL.Float (0 :: GL.GLsizei) nullPtr)
    GL.bindBuffer GL.ArrayBuffer $= Just vs
    setUniform s "modelViewMatrix" ((dr .! #modelViewMatrix) :: M44 GL.GLfloat)
    setUniform s "projectionMatrix" ((dr .! #projectionMatrix) :: M44 GL.GLfloat)
    GL.drawArrays GL.Triangles 0 vc
    checkError
    attrib $= GL.Disabled
    return ()
  where
    checkError = do e <- GL.get GL.errors
                    case e of
                      ((GL.Error c s) : _) -> putStrLn $ "Error " ++ show c ++ " " ++ s
                      _ -> return ()

{-
     let shaderdata = r .! #shader
     let vBufferValues = rvalf #vertexBuffers ir
     let v2Vertices = mapMaybe (\x -> M.lookup x (v2Buffers rm)) vBufferValues
     let texCoords = mapMaybe (\x -> M.lookup x (texCoordBuffers rm))  vBufferValues
     let v3Vertices = mapMaybe (\x -> M.lookup x (v3Buffers rm)) vBufferValues
     let textureObjects = mapMaybe (\x -> M.lookup x (textures rm)) (rvalf #textures ir)
     let indexVertices = mapMaybe (\x -> M.lookup x (indexBuffers rm)) vBufferValues
     let objVertices = mapMaybe (\x -> M.lookup x (objFileBuffers rm)) vBufferValues
     GL.currentProgram $= Just (GLU.program shaderdata)
     GLU.printErrorMsg "currentProgram"
     --VGL.setUniforms shaderdata (rvalf #staticParameters ir)
     DC.withDict dc (VGL.setSomeUniforms shaderdata sp) :: IO ()
     mapM_ (vmap shaderdata) v2Vertices
     GLU.printErrorMsg "v2Vertices"
     mapM_ (vmap shaderdata) texCoords
     GLU.printErrorMsg "texCoords"
     mapM_ (vmap shaderdata) v3Vertices
     GLU.printErrorMsg "v3Vertices"
     -- objVertices are tuples, the first element is the
     -- vertex buffer we want to vmap
     mapM_ ((vmap shaderdata) . fst) objVertices
     let allIndexBuffers =  mappend indexVertices (map snd objVertices)
     mapM_ (\x -> GL.bindBuffer GL.ElementArrayBuffer $= Just (fst x)) allIndexBuffers
     GLU.printErrorMsg "indexVertices"
     --putStrLn $ show textureObjects
     GLU.withTextures2D textureObjects $ do
       --
       -- if an index array exists, use it via drawElements,
       -- otherwise just draw without an index array using drawArrays
       --
       if not (null allIndexBuffers) then do
         -- draw with drawElements
         --
         -- index arrays are Word32 which maps to GL type UnsignedInt
         -- need 'fromIntegral' to convert the count to GL.NumArrayIndices type
         let drawCount = (fromIntegral . snd . head $ allIndexBuffers)
         GL.drawElements GL.Triangles drawCount GL.UnsignedInt GLU.offset0
       else do
         -- draw with drawArrays
         --
         -- we assume 2D drawing if 2d vertices are specified for this node,
         -- otherwise use 3D drawing
         if not (null v2Vertices) then
           GL.drawArrays GL.Triangles 0 (fromIntegral . snd . head $ v2Vertices)
         else
           GL.drawArrays GL.Triangles 0 (fromIntegral . snd . head $ v3Vertices)
       GLU.printErrorMsg "drawArrays"
       return ()
    
     --putStrLn $ "argh " ++ (rvalf #shader ir)
     --let labels = recordToList (getLabels ir)
     --mapM_ putStrLn labels
    getLabels :: (AllFields ff) =>  FieldRec ff -> Rec (Data.Vinyl.Functor.Const String) ff
    getLabels _ = rlabels
    vmap shaderdata v = do
           VGL.bindVertices $ fst v
           VGL.enableVertices shaderdata $ fst v

-}
openGLAlgebra :: SceneNode r OpenGLCommand (DrawCmd r IO ()) -> DrawCmd r IO ()
openGLAlgebra (Invoke x)     = invokeGL x
openGLAlgebra (Group cmds)   = foldl (>>) (DC $ \_ -> return ()) cmds
openGLAlgebra (Transformer t gr) = DC $ \fd ->
         let fd2  = t fd
             scmd = cata openGLAlgebra gr
             in runDC scmd fd2


openGLgo :: SceneGraph r OpenGLCommand -> Rec r -> IO ()
openGLgo sg fd = let sm = cata openGLAlgebra sg
                 in runDC sm fd
