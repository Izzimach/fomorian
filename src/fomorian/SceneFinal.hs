-- experimental SceneNode implementation using a final embedding

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.SceneFinal where

import Linear
import Control.Lens ((%~),(.~),over,view)
import Data.Kind (Constraint)
import Data.Proxy

import Control.Monad.Trans

-- qualify most of OpenGL stuff except for a few common types
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint)
import qualified Graphics.GLUtil as GLU

import Data.Vinyl
import Data.Vinyl.Lens
import Data.Vinyl.TypeLevel (Nat(Z),Nat(S), AllConstrained)
import Data.Vinyl.Functor
import qualified Data.Constraint as DC

import Data.Word (Word32)
import qualified Graphics.VinylGL as VGL
import Graphics.VinylGL.Uniforms (UniformFields)
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

import Fomorian.SceneResources

-- @PerFrameData type represents not only the shape of the data but
-- include a Data.Constraint Dict that holds any constraints on the per-frame data.
-- This is used to describe any constraints on the per-frame data that is imposed
-- by invocation.
data PerFrameData ff c = PerFrameData (FieldRec ff) (DC.Dict c)

class NoFrameConstraint a where

instance NoFrameConstraint a where

data RenderParamsProxy a = RPProxy

newtype InvokeRecord sp rp = 
  Invocation (
    FieldRec '[
      '("shader", String),
      '("shaderParameters", sp),
      '("vertexBuffers", [VertexSourceData]),
      '("textures", [String]),
      '("rpProxy", RenderParamsProxy rp)
    ]
  )

-- the InvokableFrameData for a given representation describes both the data and
-- the constraints on the data specific to this representation
newtype InvokableFrameData repr f = Invokable (PerFrameData f (InvokeConstraint repr (FieldRec f)))

class SceneSYM repr where
  type InvokeConstraint repr ff :: Constraint
  invoke :: InvokeRecord sp (InvokableFrameData repr f) -> repr (InvokableFrameData repr f)
  group  :: [repr rp] -> repr rp
  transformer :: (r1 -> r2) -> repr r2 -> repr r1

--
-- DrawGL is a kinda-monad that takes in per-frame render parameters and draws the
-- scene. It's basically a ReaderT with the type args shuffled around so that
-- we can manipulate the required render parameters instead of the return type.
--

newtype DrawGL m rp = DrawGL { runDrawGL :: rp -> ResourceMap -> m () }

instance (Monoid (m ())) => Monoid (DrawGL m rp) where
  mempty = DrawGL $ \_ _ -> mempty
  mappend a b = DrawGL $
    \rp re -> mappend ((runDrawGL a) rp re) ((runDrawGL b) rp re)


instance (MonadIO m) => SceneSYM (DrawGL m) where
  type InvokeConstraint (DrawGL m) ff = UniformFields ff
  invoke :: InvokeRecord sp (InvokableFrameData (DrawGL m) f) ->
     DrawGL m (InvokableFrameData (DrawGL m) f)
  invoke (Invocation ir) = DrawGL $ \(Invokable (PerFrameData rp dc)) rr -> liftIO $ do
      let vBufferValues = rvalf #vertexBuffers ir
      let v2Vertices = mapMaybe (\x -> M.lookup x (v2Buffers rr)) vBufferValues
      let texCoords = mapMaybe (\x -> M.lookup x (texCoordBuffers rr))  vBufferValues
      let v3Vertices = mapMaybe (\x -> M.lookup x (v3Buffers rr)) vBufferValues
      let textureObjects = mapMaybe (\x -> M.lookup x (textures rr)) (rvalf #textures ir)
      let indexVertices = mapMaybe (\x -> M.lookup x (indexBuffers rr)) vBufferValues
      let objVertices = mapMaybe (\x -> M.lookup x (objFileBuffers rr)) vBufferValues
      let (Just shaderdata) = M.lookup (rvalf #shader ir) (shaders rr)
      GL.currentProgram $= Just (GLU.program shaderdata)
      GLU.printErrorMsg "currentProgram"
      --VGL.setUniforms shaderdata ((#tex =: (0 :: GLint)) :& RNil)
      DC.withDict dc (VGL.setSomeUniforms shaderdata rp) :: IO ()
      mapM_ (vmap shaderdata) v2Vertices
      mapM_ (vmap shaderdata) texCoords
      mapM_ (vmap shaderdata) v3Vertices
      -- objVertices are tuples, the first element is the vertex buffer we want to vmap
      mapM_ ((vmap shaderdata) . fst) objVertices
      mapM_ (\x -> GL.bindBuffer GL.ElementArrayBuffer $= Just (fst x)) indexVertices
      --putStrLn $ show textureObjects
      let allIndexBuffers =  mappend indexVertices (map snd objVertices)
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
    where getLabels :: (AllFields ff) =>  FieldRec ff -> Rec (Data.Vinyl.Functor.Const String) ff
          getLabels _ = rlabels
          vmap shaderdata v = do
            VGL.bindVertices $ fst v
            VGL.enableVertexFields shaderdata $ fst v

  -- DrawGL isn't a proper monad instance so we have to fake it
  group  :: [DrawGL m rp] -> DrawGL m rp
  group []        = DrawGL $ \_ _ -> return ()
  group (ig : igs) = DrawGL $ \rp re -> do
                                  runDrawGL ig rp re
                                  runDrawGL (group igs) rp re

  -- basically contramap
  transformer :: (rp1 -> rp2) -> DrawGL m rp2 -> DrawGL m rp1
  transformer f ib = DrawGL $ \rp re -> let ob = runDrawGL ib
                                        in ob (f rp) re

newtype OGLResources a = OGLResources { needsGLResources :: ResourceList }

instance SceneSYM OGLResources where
  type InvokeConstraint OGLResources f  = NoFrameConstraint f
  invoke :: InvokeRecord sp rp -> OGLResources rp
  invoke (Invocation ir) = OGLResources $ ResourceList { 
        shaderfiles =  S.singleton (rvalf #shader ir), 
        vertexfiles =  S.fromList (rvalf #vertexBuffers ir),
        texturefiles = S.fromList (rvalf #textures ir)
      }
  
  group :: [OGLResources rp] -> OGLResources rp
  group xs = OGLResources $ foldMap needsGLResources xs

  -- when listing resources we don't care about per-frame draw parameters
  transformer :: (rp1 -> rp2) -> OGLResources rp2 -> OGLResources rp1
  transformer _ (OGLResources rl) = OGLResources rl

--
-- common scene nodes and transforms
--

buildPixelOrthoMatrix :: (Integral a, RealFrac b) => a -> a -> M44 b
buildPixelOrthoMatrix w h =
  let 
    w1 = 1.0 / fromIntegral w
    h1 = 1.0 / fromIntegral h
    scaleMatrix x y z = V4  (V4 x 0 0 0)
                            (V4 0 y 0 0)
                            (V4 0 0 z 0)
                            (V4 0 0 0 1)
    translationMatrix x y z = V4 (V4 1 0 0 x)
                                  (V4 0 1 0 y)
                                  (V4 0 0 1 z)
                                  (V4 0 0 0 1)
    m1 = translationMatrix (-1) (-1) 0
    m2 = scaleMatrix (w1*2) (h1*2) 1
  in
    m1 !*! m2


type PixelOrthoFrameFields = '[ '("windowX", Integer), '("windowY",Integer), '("curTime",Float) ]

orthize :: (fr ~ FieldRec ff, PixelOrthoFrameFields <: ff) => fr -> M44 GLfloat
orthize d = let rf = rcast d :: FieldRec PixelOrthoFrameFields
                w = rvalf #windowX rf
                h = rvalf #windowY rf
            in
              buildPixelOrthoMatrix w h

type OrthoParamFields = '[
    '("cameraProjection", M44 GLfloat),
    '("worldTransform", M44 GLfloat),
    '("time", Float)
  ]
type OrthoParams = FieldRec OrthoParamFields
  
orthoConvert :: (fr ~ FieldRec ff, 
                 PixelOrthoFrameFields ~ ff,
                 InvokeConstraint repr OrthoParams) => fr -> InvokableFrameData repr OrthoParamFields
orthoConvert fr = 
        let orthoM = orthize fr
            t = rvalf #curTime fr
            frameData =      (#cameraProjection =: orthoM)
                          :& (#worldTransform =: (identity :: M44 GLfloat) )
                          :& (#time =: t)
                          :& RNil
        in
          Invokable (PerFrameData frameData DC.Dict)

--
-- | Generate an orthographic projection where OpenGL coordinates
-- | are mapped onto window pixels. For a window of width $w$ and
-- | height $h$ the coordinates (0,0) are the lower-left corner of the
-- | window and (w,h) refer to the upper-right corner of the window.
-- | Resizing the window will change these values; if you want some sort
-- | of auto-scaling with the window use $ortho2DView$ or $fitOrtho2DView$
--
pixelOrtho2DView :: (SceneSYM repr, fr ~ FieldRec ff, PixelOrthoFrameFields ~ ff, InvokeConstraint repr OrthoParams) => 
    repr (InvokableFrameData repr OrthoParamFields) -> repr fr
pixelOrtho2DView = transformer orthoConvert




perspectiveProject :: (fr ~ FieldRec ff, PixelOrthoFrameFields  ~ ff,
                       InvokeConstraint repr OrthoParams) => 
                       GLfloat -> GLfloat -> GLfloat -> GLfloat -> fr -> InvokableFrameData repr OrthoParamFields
perspectiveProject fov aspect near far fr =
          let t = rvalf #curTime fr
              frameData =    (#cameraProjection =: perspective fov aspect near far)
                          :& (#worldTransform   =: (identity :: M44 GLfloat) )
                          :& (#time =: t)
                          :& RNil
          in
            Invokable (PerFrameData frameData DC.Dict) 

perspective3DView :: (SceneSYM repr, fr ~ FieldRec ff, PixelOrthoFrameFields ~ ff, InvokeConstraint repr OrthoParams) => 
    (Float, Float) -> repr (InvokableFrameData repr OrthoParamFields) -> repr fr
perspective3DView (near, far)= transformer (perspectiveProject 1 1 near far)

translateWorld :: (InvokeConstraint repr OrthoParams) => 
                  V3 GLfloat ->
                  InvokableFrameData repr OrthoParamFields -> InvokableFrameData repr OrthoParamFields
translateWorld (V3 tx ty tz) (Invokable (PerFrameData rp dc)) =
  let xform = rvalf #worldTransform rp
      t = rvalf #time rp
      translationMatrix x y z = V4 (V4 1 0 0 x)
                                   (V4 0 1 0 y)
                                   (V4 0 0 1 z)
                                   (V4 0 0 0 1)
      xform' = xform !*! translationMatrix tx ty tz
      cproj = rvalf #cameraProjection rp
      frameData =    (#cameraProjection =: cproj)
                  :& (#worldTransform =: xform')
                  :& (#time =: t)
                  :& RNil
  in
    Invokable (PerFrameData frameData DC.Dict)

translate2d :: (SceneSYM repr,
                InvokeConstraint repr OrthoParams) =>
    V2 GLfloat -> repr (InvokableFrameData repr OrthoParamFields) -> 
                          repr (InvokableFrameData repr OrthoParamFields)
translate2d (V2 tx ty) = transformer (translateWorld (V3 tx ty 0))

translate3d :: (SceneSYM repr,
              InvokeConstraint repr OrthoParams) =>
      V3 GLfloat -> repr (InvokableFrameData repr OrthoParamFields) -> 
                                     repr (InvokableFrameData repr OrthoParamFields)
translate3d tr = transformer (translateWorld tr)

rotateAxisAngle :: (SceneSYM repr,
                    InvokeConstraint repr OrthoParams) =>
                    V3 GLfloat -> Float -> (InvokableFrameData repr OrthoParamFields) -> 
                      (InvokableFrameData repr OrthoParamFields)
rotateAxisAngle axis angle (Invokable (PerFrameData rp dc)) =
  let xform = rvalf #worldTransform rp
      cproj = rvalf #cameraProjection rp
      t = rvalf #time rp
      -- first build a quaterion, then convert to a matrix
      quat = axisAngle axis angle
      rotationMatrix = mkTransformation quat (V3 0 0 0)
      xform' = xform !*! rotationMatrix
      frameData =    (#cameraProjection =: cproj)
                  :& (#worldTransform =: xform')
                  :& (#time =: t)
                  :& RNil
  in
    Invokable (PerFrameData frameData DC.Dict)

rotate3d axis angle = transformer (rotateAxisAngle axis angle)

rotate3dDynamic axis spinspeed = transformer (spinAxis axis spinspeed)
  where
    spinAxis axis spin x@(Invokable (PerFrameData rp dc)) =
      let angle = spin * realToFrac (rvalf #time rp)
      in rotateAxisAngle axis angle x

