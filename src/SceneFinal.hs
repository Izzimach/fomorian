-- experimental SceneNode implementation using a final embedding

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
{-# LANGUAGE UndecidableInstances #-}

module SceneFinal where

import Linear
import Control.Lens ((%~),(.~),over,view)
import Data.Kind (Constraint)

import Control.Monad.Trans

-- qualify most of OpenGL stuff except for a few common types
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint)
import qualified Graphics.GLUtil as GLU

import Data.Vinyl
import Data.Vinyl.Lens
import Data.Vinyl.TypeLevel (Nat(Z),Nat(S), AllConstrained)
import Data.Vinyl.Functor

import Data.Word (Word32)
import qualified Graphics.VinylGL as VGL
import Graphics.VinylGL.Uniforms (UniformFields)
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

import SceneResources

data PerFrameData ff = PerFrameData (FieldRec ff)

data RenderParamsProxy a = RPProxy

data InvokeRecord sp rp = Invocation (FieldRec '[
      '("shader", String),
      '("shaderParameters", sp),
      '("vertexBuffers", [VertexSourceData]),
      '("textures", [String]),
      '("rpProxy", RenderParamsProxy rp)
    ])

class SceneSYM repr where
  invoke :: InvokeRecord sp rp -> repr rp
  group  :: [repr rp] -> repr rp
  transformer :: (r1 -> r2) -> repr r2 -> repr r1

--
-- A monad that generates OpenGL commands. I guess
-- this is really just the ReaderT monad, where what we're
-- "reading" are the render parameters for the current frame
--

data OpenGLM r m a = OpenGLM { runOpenGL :: r -> m a }

instance (Monad m) => Functor (OpenGLM r m) where
  fmap f m = OpenGLM $ \r -> do
    a <- runOpenGL m r
    return (f a)

instance (Applicative m, Monad m) => Applicative (OpenGLM r m) where
  pure a = OpenGLM (\_ -> pure a)
  m1 <*> m2 = OpenGLM $ \r -> do
      fa <- runOpenGL m1 r
      a  <- runOpenGL m2 r
      return (fa a)

instance (Monad m) => Monad (OpenGLM r m) where
  m >>= f = OpenGLM $ \r -> do
    a <- runOpenGL m r
    runOpenGL (f a) r

instance MonadTrans (OpenGLM r) where
  lift m = OpenGLM $ \_ -> m

instance (MonadIO m) => MonadIO (OpenGLM r m) where
  liftIO = lift . liftIO


--
-- DrawGL is a monad that takes in per-frame render parameters and draws the
-- scene. It's basically a ReaderT with the type args shuffled around so that
-- we can manipulate the required render parameters instead of the return type.
--

data DrawGL m rp = DrawGL { runDrawGL :: rp -> m () }

instance (MonadIO m) => SceneSYM (DrawGL m) where
  invoke :: InvokeRecord sp rp -> DrawGL m rp
  invoke (Invocation ir) = DrawGL (\rp -> liftIO $ do
                                        putStrLn $ "argh " ++ (rvalf #shader ir)
                                        let labels = recordToList (getLabels ir)
                                        mapM_ putStrLn labels
                                        return ()
                           )
    where getLabels :: (AllFields ff) =>  FieldRec ff -> Rec (Data.Vinyl.Functor.Const String) ff
          getLabels _ = rlabels

  group  :: [DrawGL m rp] -> DrawGL m rp
  group []         = DrawGL (\_ -> return ())
  group (ig : igs) = ig

  transformer :: (rp1 -> rp2) -> DrawGL m rp2 -> DrawGL m rp1
  transformer f ib = DrawGL $ \a -> let ob = runDrawGL ib
                                      in ob (f a)

newtype OGLResources a = OGLResources ResourceMap

instance SceneSYM OGLResources where
  invoke :: InvokeRecord sp rp -> OGLResources rp
  invoke = undefined

  group :: [OGLResources rp] -> OGLResources rp
  group = undefined

  transformer :: (rp1 -> rp2) -> OGLResources rp2 -> OGLResources rp1
  transformer = undefined

--
-- common scene nodes and transforms
--

buildPixelOrthoMatrix :: (Integral a) => a -> a -> M44 GLfloat
buildPixelOrthoMatrix w h =
  let 
    w1 = 1.0 / (fromIntegral w)
    h1 = 1.0 / (fromIntegral h)
    scaleMatrix x y z = V4 (V4 x 0 0 0)
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
orthize d = let rf = (rcast d) :: FieldRec PixelOrthoFrameFields
                w = rvalf #windowX rf
                h = rvalf #windowY rf
            in
              buildPixelOrthoMatrix w h

type OrthoParams = FieldRec '[ 
    '("cameraProjection", M44 GLfloat),
    '("worldTransform", M44 GLfloat)
  ]
  
orthoConvert :: (fr ~ FieldRec ff, PixelOrthoFrameFields <: ff) => fr -> OrthoParams
orthoConvert fr = 
        let orthoM = orthize fr
        in   (#cameraProjection =: orthoM)
          :& (#worldTransform =: (identity :: M44 GLfloat) )
          :& RNil

ortho2DView :: (SceneSYM repr, fr ~ FieldRec ff, PixelOrthoFrameFields <: ff) => repr OrthoParams -> repr fr
ortho2DView  = transformer orthoConvert
