{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module SceneNode where
import Linear

-- qualify most of OpenGL stuff except for a few common types
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint)
import qualified Graphics.GLUtil as GLU

import Data.Vinyl
import Data.Word (Word32)
import qualified Graphics.VinylGL as VGL
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

import SceneResources

-- |The 'SceneNode' class represents a single node of the scene graph,
--  although operations apply to the node and all its children.
class SceneNode s where
  -- |'@nodeResources@ returns a ResourceList of the resources this subtree requires
  nodeResources :: s -> ResourceList
  -- |In @render s rp rs@:
  -- @s@ is the scenegraph to render
  -- @rp@ is a vinyl record containing
  -- the set of time-varying parameters getting passed to this node for this
  -- specific frame
  -- @rs@ is the ResourceMap containing resources
  render :: s -> RenderParams -> ResourceMap -> IO ()

  
{-|
  A generic wrapper to represent any SceneNode. We need this since
  a given child scene node could be of any type, so the 'children'
  field is just a list of SceneNodeBox elements instead of a list
  containing a specific type.
-}
data SceneNodeBox = forall s . (SceneNode s) => SceneNodeBox s

instance SceneNode SceneNodeBox where
  nodeResources (SceneNodeBox s) = nodeResources s
  render (SceneNodeBox s) rp rs = render s rp rs


--
-- A scene node represented as a vinyl record.
-- The type sp is a vinyl record generated for each node, representing
-- the staic shader parameters used by that node.
--
type SceneNodeR sp = FieldRec '[
  '("shader", String),
  '("shaderParameters", sp),
  '("vertexBuffers", [VertexSourceData]),
  '("textures", [String]),
  '("children", [SceneNodeBox])
  ]

instance SceneNode (SceneNodeR sp) where
  nodeResources s = ResourceList { 
                  shaderfiles =  S.singleton (rvalf #shader s), 
                  vertexfiles =  S.fromList (rvalf #vertexBuffers s),
                  texturefiles = S.fromList (rvalf #textures s)
                }
  render s rp rs = renderNode s rp rs

type RenderParams = FieldRec '[
   '("transformMatrix", M44 GLfloat), 
   '("tex", GLint)
    ]

renderNode :: SceneNodeR sp -> RenderParams -> ResourceMap -> IO ()
renderNode scene renderparams resources = do
  let vBufferValues = rvalf #vertexBuffers scene
  let v2Vertices = mapMaybe (\x -> M.lookup x (v2Buffers resources)) vBufferValues
  let v3Vertices = mapMaybe (\x -> M.lookup x (v3Buffers resources)) vBufferValues
  let indexVertices = mapMaybe (\x -> M.lookup x (indexBuffers resources)) vBufferValues
  let textureObjects = mapMaybe (\x -> M.lookup x (textures resources)) (rvalf #textures scene)
  let (Just shaderdata) = M.lookup (rvalf #shader scene) (shaders resources)
  GL.currentProgram $= Just (GLU.program shaderdata)
  GLU.printErrorMsg "currentProgram"
  --VGL.setUniforms shaderdata ((#tex =: (0 :: GLint)) :& RNil)
  VGL.setUniforms shaderdata renderparams
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


data SceneGraph = forall s . (SceneNode s) => SG s

renderScene :: SceneGraph -> RenderParams -> ResourceMap -> IO ()
renderScene (SG s) rp rm = render s rp rm

loadResourcesForScene :: SceneGraph -> IO ResourceMap
loadResourcesForScene sg = syncResourcesForScene sg emptyResourceMap

syncResourcesForScene :: SceneGraph -> ResourceMap -> IO ResourceMap
syncResourcesForScene (SG s) oldres =
  let
    rl = listResourcesInGraph s
  in
    loadResources rl oldres

listResourcesInGraph :: (SceneNode s) => s -> ResourceList
listResourcesInGraph s = listResources' s emptyResourceList

listResources' :: (SceneNode s) => s -> ResourceList -> ResourceList
listResources' s acc =
  let rl = nodeResources s
  in mergeResourceLists acc rl
