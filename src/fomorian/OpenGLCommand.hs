{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Fomorian.OpenGLCommand where

import Linear

import Data.Functor.Foldable
import Data.Row
import Data.Row.Variants (view)
import qualified Data.Map.Strict as M

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
type instance (DrawReq OpenGLCommand dreq) = (HasType "modelMatrix" (M44 Float) dreq,
                                               HasType "viewMatrix" (M44 Float) dreq,
                                               HasType "projectionMatrix" (M44 Float) dreq)

-- | Given an OpenGLTarget scene graph and loaded resources, builds an OpenGlCommand scene graph which can be directly
--   converted into a monad to draw the scene
oglCommandAlgebra :: OpenGLResources -> SceneGraphF dreq OpenGLTarget (SceneGraph dreq OpenGLCommand) -> SceneGraph dreq OpenGLCommand
oglCommandAlgebra (OpenGLResources h) (InvokeF x) =
  -- these are pulled from the InvokeReq for OpenGLTarget
  let vaoSource = x .! #geometry
      vaoResource = M.lookup (DataSource $ IsJust #boundVertices vaoSource) h
      textureGLSource = x .! #textures
      {-textureList =  do t <- traverse (\r -> H.lookup (bumpVert r) h) textureGLSource
                        t2 <- traverse (view #textureObject . unResource) t
                        return t2-}
      textureList = do t <- traverse (\r -> M.lookup (bumpVert r) h) textureGLSource
                       t2 <- traverse (view #textureObject . unResource) t
                       return t2
      --tl = traverse (view #textureObject . unResource) textureList
      --textureList =  (traverse (\r -> H.lookup (bumpVert r) h) textureGLSource) >>= (traverse (view #textureObject . unResource))
  in
    case (vaoResource, textureList) of
      (Just (Resource (view #boundVertices -> Just (vao,sp,geo))), Just tl) ->
        Invoke (   #shader .== sp
                .+ #vao .== vao
                .+ #vertexCount .== 0
                .+ #indexBuffer .== (indexBuffer geo)
                .+ #textures .== tl
               )
      _ -> Group []
    {-case (shaderGLSource, vertexGLSource) of
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
    (_,_) -> undefined-}
oglCommandAlgebra _ (GroupF cmds) = Group cmds
oglCommandAlgebra r (TransformerF t gr) = Transformer t (oglToCommand r gr)

oglToCommand :: OpenGLResources -> SceneGraph dreq OpenGLTarget -> SceneGraph dreq OpenGLCommand
oglToCommand r sg = cata (oglCommandAlgebra r) sg


bumpVert :: DataSource BasicDataSourceTypes -> DataSource GLDataSourceTypes
bumpVert (DataSource vd) = DataSource (diversify @("boundVertices" .== (FilePath, DataSource BasicDataSourceTypes)) vd)


--
-- Drawing with OpenGL - shader parameters must be Uniform-valid, which
-- means they are GLfloat, GLint, or vectors (V2,V3,V4) or matrices
--

invokeGL :: (InvokeReq OpenGLCommand r, DrawReq OpenGLCommand dreq) => Rec r -> DrawCmd dreq IO ()
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


openGLAlgebra :: SceneGraphF r OpenGLCommand (DrawCmd r IO ()) -> DrawCmd r IO ()
openGLAlgebra (InvokeF x)     = invokeGL x
openGLAlgebra (GroupF cmds)   = foldl (>>) (DC $ \_ -> return ()) cmds
openGLAlgebra (TransformerF t gr) = DC $ \fd ->
         let fd2  = t fd
             scmd = cata openGLAlgebra gr
             in runDC scmd fd2


openGLgo :: SceneGraph r OpenGLCommand -> Rec r -> IO ()
openGLgo sg fd = let sm = cata openGLAlgebra sg
                 in runDC sm fd
