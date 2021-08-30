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

{-
The 'OpenGLCommand' target represents a scene graph that is ready to draw to an OpenGL frame buffer.
You create an 'OpenGLCommand' target by combining the 'OpenGLTarget' with a set of loaded resources.
You can convert this scene graph into a 'DrawCmd' that draws to the OpenGL framebuffer (see 'openGLgo')
-}
module Fomorian.OpenGL.OpenGLCommand where

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
import Fomorian.OpenGL.OpenGLResources

data OpenGLCommand

type instance (InvokeReq OpenGLCommand ir) = (HasType "shader" GLUtil.ShaderProgram ir,
                                                HasType "vao" GL.VertexArrayObject ir,
                                                HasType "vertexCount" GL.GLint ir,
                                                HasType "indexBuffer" (Maybe (GL.BufferObject)) ir,
                                                HasType "textures" [GL.TextureObject] ir)
type instance (DrawReq OpenGLCommand dr) = (HasType "modelMatrix" (M44 Float) dr,
                                               HasType "viewMatrix" (M44 Float) dr,
                                               HasType "projectionMatrix" (M44 Float) dr)

-- | Given an OpenGLTarget scene graph and loaded resources, builds an OpenGlCommand scene graph which can be directly
--   converted into a monad to draw the scene
oglCommandAlgebra :: OpenGLResources -> SceneGraphF OpenGLTarget dr (SceneGraph OpenGLCommand dr) -> SceneGraph OpenGLCommand dr
oglCommandAlgebra (OpenGLResources h) (InvokeF x) =
  -- these are pulled from the InvokeReq for OpenGLTarget
  let vaoSource = x .! #vertexarray
      vaoResource = M.lookup (DataSource $ IsJust #vertexarray vaoSource) h
      textureGLSource = x .! #textures
      textureList = do t <- traverse (\r -> M.lookup (bumpVert r) h) textureGLSource
                       t2 <- traverse (view #textureObject . unResource) t
                       return t2
  in
    case (vaoResource, textureList) of
      (Just (Resource (view #boundVertices -> Just (vao,sp,geo))), Just tl) ->
        Invoke (   #shader .== sp
                .+ #vao .== vao
                .+ #vertexCount .== (fromIntegral $ elementCount geo)
                .+ #indexBuffer .== (indexBuffer geo)
                .+ #textures .== tl
               )
      -- not loaded yet
      _ -> Group []
oglCommandAlgebra _ (GroupF cmds) = Group cmds
oglCommandAlgebra r (TransformerF t gr) = Transformer t (oglToCommand r gr)


oglToCommand :: OpenGLResources -> SceneGraph OpenGLTarget dr -> SceneGraph OpenGLCommand dr
oglToCommand r sg = cata (oglCommandAlgebra r) sg


bumpVert :: DataSource BasicDataSourceTypes -> DataSource GLDataSourceTypes
bumpVert (DataSource vd) = DataSource (diversify @("vertexarray" .== (FilePath, DataSource BasicDataSourceTypes)) vd)



-- | Drawing a single thing with OpenGL - shader parameters must be Uniform-valid, which
--   means they are GLfloat, GLint, or vectors (V2,V3,V4) or matrices.
invokeGL :: (InvokeReq OpenGLCommand ir, DrawReq OpenGLCommand dr) => Rec ir -> DrawCmd dr IO ()
invokeGL r = DC $ \dr ->
  do
    let s = r .! #shader
    let vao = r .! #vao
    let ib = r .! #indexBuffer
    let vc = r .! #vertexCount
    let txs = r .! #textures
    GL.currentProgram $= Just (GLUtil.program s)
    GL.bindVertexArrayObject $= Just vao
    setUniform s "modelMatrix" ((dr .! #modelMatrix) :: M44 GL.GLfloat)
    setUniform s "viewMatrix" ((dr .! #viewMatrix) :: M44 GL.GLfloat)
    setUniform s "projectionMatrix" ((dr .! #projectionMatrix) :: M44 GL.GLfloat)
    GL.bindBuffer GL.ElementArrayBuffer $= ib
    GLUtil.withTextures2D txs $ do
      case ib of
        Just _ -> GL.drawElements GL.Triangles vc GL.UnsignedInt nullPtr
        Nothing -> GL.drawArrays GL.Triangles 0 vc
      checkError
    GL.bindVertexArrayObject $= Nothing
    GL.bindBuffer GL.ElementArrayBuffer $= Nothing
    return ()
  where
    checkError = do e <- GL.get GLU.errors
                    case e of
                      ((GLU.Error c s) : _) -> putStrLn $ "Error " ++ show c ++ " " ++ s
                      _ -> return ()



openGLAlgebra :: SceneGraphF OpenGLCommand dr (DrawCmd dr IO ()) -> DrawCmd dr IO ()
openGLAlgebra (InvokeF x)     = invokeGL x
openGLAlgebra (GroupF cmds)   = foldl (>>) (DC $ \_ -> return ()) cmds
openGLAlgebra (TransformerF t gr) = DC $ \fd ->
         let fd2  = t fd
             scmd = cata openGLAlgebra gr
             in runDC scmd fd2


openGLgo :: SceneGraph OpenGLCommand dr -> Rec dr -> IO ()
openGLgo sg fd = let sm = cata openGLAlgebra sg
                 in runDC sm fd
