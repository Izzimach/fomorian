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

import Fomorian.SceneNode
import Fomorian.SceneResources

data OpenGLCommand
type instance (InvokeReq OpenGLCommand sreq) = (HasType "shader" GLDataSource sreq,
                                                HasType "vertices" GLDataSource sreq)
type instance (FrameReq OpenGLCommand dreq) = (HasType "modelViewMatrix" (M44 Float) dreq,
                                               HasType "projectionMatrix" (M44 Float) dreq)

drawGLAlgebra :: (MonadIO m) => SceneNode r OpenGLCommand (DrawCmd r m ()) -> DrawCmd r m ()
drawGLAlgebra (Invoke x) = liftIO $ putStrLn $ show (x .! #shader)
drawGLAlgebra (Group children) = foldl (>>) (DC $ \_ -> return ()) children
drawGLAlgebra (Transformer f gr) = DC $ \r ->
    let s = f r
        scmd = cata drawGLAlgebra gr
    in runDC scmd s

drawGL :: SceneGraph r OpenGLCommand -> DrawCmd r IO ()
drawGL = cata drawGLAlgebra

testScene :: (HasType "time" (Double) r) => SceneGraph r OpenGLCommand
testScene = group [invoke (#shader .== "ack"), invoke (#shader .== "argh")]



type GLResourceList = S.Set GLDataSource

oglResourcesAlgebra :: SceneNode dreq OpenGLCommand GLResourceList -> GLResourceList
oglResourcesAlgebra (Invoke x) = let sh = x .! #shader
                                     v  = x .! #vertices
                                 in S.fromList [sh, v]
oglResourcesAlgebra (Group cmds) = foldl S.union S.empty cmds
oglResourcesAlgebra (Transformer t gr) = oglResourcesScene gr

oglResourcesScene :: SceneGraph dreq OpenGLCommand -> GLResourceList
oglResourcesScene sg = cata oglResourcesAlgebra sg

{-

--
-- Drawing with OpenGL - shader parameters must be Uniform-valid, which
-- means they are GLfloat, GLint, or vectors (V2,V3,V4) or matrices
--

data DrawGL

instance DrawMethod DrawGL where
  type ShaderReady DrawGL sp = (VGL.UniformFields sp)


invokeGL :: (sp ~ FieldRec sf) =>
  Invocation tp sp ->
  ReaderT ResourceMap (DrawCmd (FrameData sp np DrawGL) IO) ()
invokeGL ir = ReaderT $ \rm -> DC $ \fd -> goInvoke ir rm fd
  where
    goInvoke :: (sp ~ FieldRec sf) =>
      Invocation tp sp ->
      ResourceMap -> FrameData sp np DrawGL -> IO ()
    goInvoke ir rm (FrameData sp np dc) = liftIO $ do
     let vBufferValues = rvalf #vertexBuffers ir
     let v2Vertices = mapMaybe (\x -> M.lookup x (v2Buffers rm)) vBufferValues
     let texCoords = mapMaybe (\x -> M.lookup x (texCoordBuffers rm))  vBufferValues
     let v3Vertices = mapMaybe (\x -> M.lookup x (v3Buffers rm)) vBufferValues
     let textureObjects = mapMaybe (\x -> M.lookup x (textures rm)) (rvalf #textures ir)
     let indexVertices = mapMaybe (\x -> M.lookup x (indexBuffers rm)) vBufferValues
     let objVertices = mapMaybe (\x -> M.lookup x (objFileBuffers rm)) vBufferValues
     let (Just shaderdata) = M.lookup (rvalf #shader ir) (shaders rm)
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


openGLAlgebra :: (sp ~ FieldRec sf) =>
  SceneNode sp np DrawGL (ReaderT ResourceMap (DrawCmd (FrameData sp np DrawGL) IO) ()) ->
  (ReaderT ResourceMap (DrawCmd (FrameData sp np DrawGL) IO) ())
openGLAlgebra (Invoke x)     = invokeGL x
openGLAlgebra (Group cmds)   = foldl (>>) (ReaderT $ \rm -> DC $ \fd -> return ()) cmds
openGLAlgebra (Transformer t gr) = ReaderT $ \rm ->
  DC $ \fd ->
         let fd2  = t fd
             (FrameData sp2 c2 dc2) = fd2
             scmd = DC.withDict dc2 (cata openGLAlgebra gr)
             in runDC (runReaderT scmd rm) fd2
  
openGLgo :: SceneGraph (FieldRec sf) np DrawGL ->
  FrameData (FieldRec sf) np DrawGL ->
  ResourceMap ->
  IO ()
openGLgo sg sp rm = let sm = runReaderT (cata openGLAlgebra sg) rm
                    in runDC sm sp
                  
-}