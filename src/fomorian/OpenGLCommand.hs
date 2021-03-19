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

import Linear

import Data.Functor.Foldable

import Data.Row

import Foreign.Ptr (nullPtr)

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as GLUtil
import Graphics.GLUtil.ShaderProgram (setUniform)

import Fomorian.SceneNode
import Fomorian.SceneResources
import Fomorian.OpenGLResources

data OpenGLCommand
type instance (InvokeReq OpenGLCommand sreq) = (HasType "shader" GLUtil.ShaderProgram sreq,
                                                HasType "vao" GL.VertexArrayObject sreq,
                                                HasType "vertexCount" GL.GLint sreq,
                                                HasType "indexBuffer" (Maybe (GL.BufferObject)) sreq,
                                                HasType "textures" [GL.TextureObject] sreq)
type instance (FrameReq OpenGLCommand dreq) = (HasType "modelMatrix" (M44 Float) dreq,
                                               HasType "viewMatrix" (M44 Float) dreq,
                                               HasType "projectionMatrix" (M44 Float) dreq)

-- | Given an OpenGLTarget scene graph and loaded resources, builds an OpenGlCommand scene graph which can be directly
--   converted into a monad to draw the scene
oglCommandAlgebra :: OpenGLResources -> SceneNode dreq OpenGLTarget (SceneGraph dreq OpenGLCommand) -> SceneGraph dreq OpenGLCommand
oglCommandAlgebra (OpenGLResources r r') (Invoke x) =
  -- these are pulled from the InvokeReq for OpenGLTarget
  let shaderGLSource = x .! #shader
      vertexGLSource = x .! #vertices
      textureGLSource = x .! #textures
      shaderRecord = lookupResource shaderGLSource r
      vertexRecord = lookupResource vertexGLSource r
      textureList = traverse (flip lookupResource r) textureGLSource
  in case (shaderGLSource, vertexGLSource) of
    (MaterialData ss, GeometryData vs) -> 
      case (shaderRecord, vertexRecord) of
          (Just (GLShaderProgram sR), Just (GLVertexArray _va ix len)) ->
            case (lookupResource (BoundVertices ss vs) r') of
              (Just (GLBoundVertices vao _ _)) -> Fix $ Invoke (#shader      .== sR .+
                                                                #vao         .== vao .+ 
                                                                #vertexCount .== len .+ 
                                                                #indexBuffer .== ix .+ 
                                                                #textures    .== [])
              _                          -> undefined
          (_,_) -> undefined
    (_,_) -> undefined
oglCommandAlgebra _ (Group cmds) = Fix $ Group cmds
oglCommandAlgebra r (Transformer t gr) = Fix $ Transformer t (oglToCommand r gr)

oglToCommand :: OpenGLResources -> SceneGraph dreq OpenGLTarget -> SceneGraph dreq OpenGLCommand
oglToCommand r sg = cata (oglCommandAlgebra r) sg


--
-- Drawing with OpenGL - shader parameters must be Uniform-valid, which
-- means they are GLfloat, GLint, or vectors (V2,V3,V4) or matrices
--

invokeGL :: (InvokeReq OpenGLCommand r, FrameReq OpenGLCommand dreq) => Rec r -> DrawCmd dreq IO ()
invokeGL r = DC $ \dr ->
  do
    let s = r .! #shader
    let vao = r .! #vao
    let ib = r .! #indexBuffer
    let vc = r .! #vertexCount
    GL.currentProgram $= Just (GLUtil.program s)
    GL.bindVertexArrayObject $= Just vao
    setUniform s "modelMatrix" ((dr .! #modelMatrix) :: M44 GL.GLfloat)
    setUniform s "viewMatrix" ((dr .! #viewMatrix) :: M44 GL.GLfloat)
    setUniform s "projectionMatrix" ((dr .! #projectionMatrix) :: M44 GL.GLfloat)
    GL.bindBuffer GL.ElementArrayBuffer $= ib
    case ib of
      Just _ -> GL.drawElements GL.Triangles vc GL.UnsignedInt nullPtr
      Nothing -> GL.drawArrays GL.Triangles 0 vc
    checkError
    GL.bindVertexArrayObject $= Nothing
    return ()
  where
    checkError = do e <- GL.get GLU.errors
                    case e of
                      ((GLU.Error c s) : _) -> putStrLn $ "Error " ++ show c ++ " " ++ s
                      _ -> return ()


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
