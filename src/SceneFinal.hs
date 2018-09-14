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

module SceneFinal where

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

import SceneResources

data PerFrameData ff c = PerFrameData (FieldRec ff) (DC.Dict c)

class NoFrameConstraint a where

instance NoFrameConstraint a where

data RenderParamsProxy a = RPProxy

newtype InvokeRecord sp rp = Invocation (FieldRec '[
      '("shader", String),
      '("shaderParameters", sp),
      '("vertexBuffers", [VertexSourceData]),
      '("textures", [String]),
      '("rpProxy", RenderParamsProxy rp)
    ])

newtype InvokableFrameData repr f = Invokable (PerFrameData f (InvokeConstraint repr (FieldRec f)))

class SceneSYM repr where
  type InvokeConstraint repr ff :: Constraint
  invoke :: InvokeRecord sp (InvokableFrameData repr f) -> repr (InvokableFrameData repr f)
--  invokable :: FieldRec f -> repr (InvokableFrameData repr f)
  group  :: [repr rp] -> repr rp
  transformer :: (r1 -> r2) -> repr r2 -> repr r1

--
-- DrawGL is a monad that takes in per-frame render parameters and draws the
-- scene. It's basically a ReaderT with the type args shuffled around so that
-- we can manipulate the required render parameters instead of the return type.
--

newtype DrawGL m rp = DrawGL { runDrawGL :: rp -> ResourceMap -> m () }



instance (MonadIO m) => SceneSYM (DrawGL m) where
  type InvokeConstraint (DrawGL m) ff = UniformFields ff
  invoke :: InvokeRecord sp (InvokableFrameData (DrawGL m) f) ->
     DrawGL m (InvokableFrameData (DrawGL m) f)
  invoke (Invocation ir) = DrawGL $ \(Invokable (PerFrameData rp dc)) rr -> liftIO $ do
      let vBufferValues = rvalf #vertexBuffers ir
      let v2Vertices = mapMaybe (\x -> M.lookup x (v2Buffers rr)) vBufferValues
      let texCoords = mapMaybe (\x -> M.lookup x (texCoordBuffers rr))  vBufferValues
      let v3Vertices = mapMaybe (\x -> M.lookup x (v3Buffers rr)) vBufferValues
      let indexVertices = mapMaybe (\x -> M.lookup x (indexBuffers rr)) vBufferValues
      let textureObjects = mapMaybe (\x -> M.lookup x (textures rr)) (rvalf #textures ir)
      let (Just shaderdata) = M.lookup (rvalf #shader ir) (shaders rr)
      GL.currentProgram $= Just (GLU.program shaderdata)
      GLU.printErrorMsg "currentProgram"
      --VGL.setUniforms shaderdata ((#tex =: (0 :: GLint)) :& RNil)
      DC.withDict dc (VGL.setUniforms shaderdata rp) :: IO ()
      mapM_ (vmap shaderdata) v2Vertices
      mapM_ (vmap shaderdata) texCoords
      mapM_ (vmap shaderdata) v3Vertices
      mapM_ (\x -> GL.bindBuffer GL.ElementArrayBuffer $= Just (fst x)) indexVertices
      --putStrLn $ show textureObjects
      GLU.withTextures2D textureObjects $ do
        --
        -- if an index array exists, use it via drawElements,
        -- otherwise just draw without an index array using drawArrays
        --
        if not (null indexVertices) then
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

  -- should make a proper monad instance for DrawGL
  group  :: [DrawGL m rp] -> DrawGL m rp
  group []        = DrawGL $ \_ _ -> return ()
  group (ig : igs) = DrawGL $ \a r -> do
                                  runDrawGL ig a r
                                  runDrawGL (group igs) a r

  transformer :: (rp1 -> rp2) -> DrawGL m rp2 -> DrawGL m rp1
  transformer f ib = DrawGL $ \a r -> let ob = runDrawGL ib
                                      in ob (f a) r

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

type PixelOrthoFrameFields = '[ '("windowX", Integer), '("windowY",Integer) ]

orthize :: (fr ~ FieldRec ff, PixelOrthoFrameFields <: ff) => fr -> M44 GLfloat
orthize d = let rf = rcast d :: FieldRec PixelOrthoFrameFields
                w = rvalf #windowX rf
                h = rvalf #windowY rf
            in
              buildPixelOrthoMatrix w h

type OrthoParamFields = '[
    '("cameraProjection", M44 GLfloat),
    '("worldTransform", M44 GLfloat)
  ]
type OrthoParams = FieldRec OrthoParamFields
  
orthoConvert :: (fr ~ FieldRec ff, PixelOrthoFrameFields <: ff,
                 InvokeConstraint repr OrthoParams) => fr -> InvokableFrameData repr OrthoParamFields
orthoConvert fr = 
        let orthoM = orthize fr
            frameData =      (#cameraProjection =: orthoM)
                          :& (#worldTransform =: (identity :: M44 GLfloat) )
                          :& RNil
        in
          Invokable (PerFrameData frameData DC.Dict)

ortho2DView :: (SceneSYM repr, fr ~ FieldRec ff, PixelOrthoFrameFields <: ff,
                InvokeConstraint repr OrthoParams) => 
    repr (InvokableFrameData repr OrthoParamFields) -> repr fr
ortho2DView = transformer orthoConvert

translateWorld :: (InvokeConstraint repr OrthoParams) => 
                  GLfloat -> GLfloat ->
                  InvokableFrameData repr OrthoParamFields -> InvokableFrameData repr OrthoParamFields
translateWorld tx ty (Invokable (PerFrameData rp dc)) =
  let xform = rvalf #worldTransform rp
      translationMatrix x y z = V4 (V4 1 0 0 x)
                                   (V4 0 1 0 y)
                                   (V4 0 0 1 z)
                                   (V4 0 0 0 1)
      xform' = translationMatrix tx ty 0 !*! xform
      cproj = rvalf #cameraProjection rp
      frameData =    (#cameraProjection =: cproj)
                  :& (#worldTransform =: xform')
                  :& RNil
  in
    Invokable (PerFrameData frameData DC.Dict)

translate2d :: (SceneSYM repr,
              InvokeConstraint repr OrthoParams) =>
    (GLfloat, GLfloat) -> repr (InvokableFrameData repr OrthoParamFields) -> 
                          repr (InvokableFrameData repr OrthoParamFields)
translate2d (tx,ty) = transformer (translateWorld tx ty)
