{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SceneNode where

import Linear

import Data.Kind (Constraint)

-- qualify most of OpenGL stuff except for a few common types
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint)
import qualified Graphics.GLUtil as GLU

import Data.Vinyl
import Data.Vinyl.Lens
import Data.Vinyl.TypeLevel (Nat(Z),Nat(S))

import Data.Word (Word32)
import qualified Graphics.VinylGL as VGL
import Graphics.VinylGL.Uniforms (UniformFields)
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

import SceneResources

data PerFrameData ff = PerFrameData (FieldRec ff)
--data ResourceMap rm = Resources rm

-- |The 'Drawable' class represents a single node or subtree of the
-- scene graph.
class (Drawable s fr) where
  type FrameConstraints s fr :: Constraint
  -- |In @draw s rp rs@:
  -- @s@ is the scenegraph to render
  -- @rp@ is a vinyl record containing
  -- the set of time-varying parameters getting passed to this node for this
  -- specific frame
  -- @rs@ is the ResourceMap containing resources
  draw :: (FrameConstraints s fr, fr ~ FieldRec ff) => s -> PerFrameData ff -> ResourceMap -> IO ()

class (NeedsResources s) where
  resources :: s -> ResourceList
  
--
-- A scene node represented as a vinyl record.
-- The type sp is a vinyl record generated for each node, representing
-- the static shader parameters used by that node.
--
data InvokeNode sp = Invoke (FieldRec '[
      '("shader", String),
      '("shaderParameters", sp),
      '("vertexBuffers", [VertexSourceData]),
      '("textures", [String])
    ])


instance Drawable (InvokeNode sp) fr where
  type FrameConstraints (InvokeNode sp) fr = (UniformFields fr)
  draw s d rm = renderNode s d rm

instance NeedsResources (InvokeNode sp) where
  resources (Invoke s) = ResourceList { 
      shaderfiles =  S.singleton (rvalf #shader s), 
      vertexfiles =  S.fromList (rvalf #vertexBuffers s),
      texturefiles = S.fromList (rvalf #textures s)
    }

renderNode :: forall sp fr . (UniformFields (FieldRec fr))
           => InvokeNode sp -> PerFrameData fr -> ResourceMap -> IO ()
renderNode (Invoke s) (PerFrameData d) resources = do
  let vBufferValues = rvalf #vertexBuffers s
  let v2Vertices = mapMaybe (\x -> M.lookup x (v2Buffers resources)) vBufferValues
  let v3Vertices = mapMaybe (\x -> M.lookup x (v3Buffers resources)) vBufferValues
  let indexVertices = mapMaybe (\x -> M.lookup x (indexBuffers resources)) vBufferValues
  let textureObjects = mapMaybe (\x -> M.lookup x (textures resources)) (rvalf #textures s)
  let (Just shaderdata) = M.lookup (rvalf #shader s) (shaders resources)
  GL.currentProgram $= Just (GLU.program shaderdata)
  GLU.printErrorMsg "currentProgram"
  --VGL.setUniforms shaderdata ((#tex =: (0 :: GLint)) :& RNil)
  VGL.setUniforms shaderdata d
  mapM (VGL.enableVertices' shaderdata . fst) v2Vertices
  mapM (VGL.bindVertices . fst) v2Vertices
  mapM (VGL.enableVertices' shaderdata . fst) v3Vertices
  mapM (VGL.bindVertices . fst) v3Vertices
  mapM (\x -> GL.bindBuffer GL.ElementArrayBuffer $= Just (fst x)) indexVertices
  --putStrLn $ show textureObjects
  GLU.withTextures2D textureObjects $ do
    --
    -- if an index array exists, use it via drawElements,
    -- otherwise just draw without an index array using drawArrays
    --
    if (length indexVertices > 0) then
      -- draw with drawElements
      --
      -- index arrays are Word32 which maps to GL type UnsignedInt
      -- need 'fromIntegral' to convert the count to GL.NumArrayIndices type
      GL.drawElements GL.Triangles (fromIntegral . snd . head $ indexVertices) GL.UnsignedInt GLU.offset0
    else
      -- draw with drawArrays
      --
      -- we assume 2D drawing if 2d vertices are specified for this node,
      -- otherwise use 3D drawing
      if (length v2Vertices > 0) then
        GL.drawArrays GL.Triangles 0 (fromIntegral . snd . head $ v2Vertices)
      else
        GL.drawArrays GL.Triangles 0 (fromIntegral . snd . head $ v3Vertices)
    GLU.printErrorMsg "drawArrays"
    return ()


data SceneGraph dr fr = SG dr fr

renderScene :: (fr ~ FieldRec ff, Drawable dr fr, FrameConstraints dr fr) =>
  SceneGraph dr fr -> PerFrameData ff -> ResourceMap -> IO ()
renderScene (SG s _) d rm = draw s d rm

loadResourcesForScene :: (NeedsResources dr) => SceneGraph dr ff -> IO ResourceMap
loadResourcesForScene sg = syncResourcesForScene sg emptyResourceMap

syncResourcesForScene :: (NeedsResources dr) => SceneGraph dr ff -> ResourceMap -> IO ResourceMap
syncResourcesForScene (SG s _) oldres =
  let
    rl = listResourcesInGraph s
  in
    loadResources rl oldres

listResourcesInGraph :: (NeedsResources s) => s -> ResourceList
listResourcesInGraph s = listResources' s emptyResourceList

listResources' :: (NeedsResources s) => s -> ResourceList -> ResourceList
listResources' s acc =
  let rl = resources s
  in mergeResourceLists acc rl
