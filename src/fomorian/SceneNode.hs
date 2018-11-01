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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

--
-- Scene node description as an F-algebra, which may open up some
-- useful functionality over Initial or Final encodings.
--

module Fomorian.SceneNode where

import Linear
import Control.Lens ( (%~), (.~), over, view)
import Data.Kind (Constraint)
import Data.Proxy
import Data.Functor.Foldable
import Data.Functor.Contravariant
import Data.Semigroup
import Data.Foldable
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad
import Control.Monad.Reader
import Control.Applicative


-- qualify most of OpenGL stuff except for a few common types
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat, GLint)
import qualified Graphics.GLUtil as GLU

-- vinyl
import Data.Vinyl
import Data.Vinyl.Lens
import Data.Vinyl.TypeLevel (Nat(Z),Nat(S), AllConstrained)
import Data.Vinyl.Functor
import qualified Data.Constraint as DC

-- vinyl-gl
import Data.Word (Word32)
import qualified Graphics.VinylGL as VGL
import Graphics.VinylGL.Uniforms (UniformFields)

import Fomorian.SceneResources

type Invocation tp sp =
  FieldRec '[
  '("shader", String),
  '("staticParameters", tp),
  '("frameParameters", sp),
  '("vertexBuffers", [VertexSourceData]),
  '("textures", [String])
  ]

--
-- @FrameData@ has two row types of parameters:
-- @sp@ is a set of "shader-ready" parameters. The types of these
-- values are those that can be passed into the shader for the given
-- renderer. Typically these are floats, ints, or vectors/matrices.
-- @np@ are not shader-ready, so they can be anything such as strings
-- or GADTs or f-algebras or whatever
--
data FrameData sp np cmd = FrameData sp np (DC.Dict (ShaderReady cmd sp))

class DrawMethod cmd where
  type ShaderReady cmd sp :: Constraint

data SceneNode sp np cmd x =
    forall tp . (Show tp, ShaderReady cmd tp) => Invoke (Invocation tp sp)
  | Group [x]
  | forall sp2 np2 c2 sf2 nf2. (sp2 ~ FieldRec sf2, np2 ~ FieldRec nf2) => Transformer (FrameData sp np cmd -> FrameData sp2 np2 cmd) (SceneGraph sp2 np2 cmd)

instance (Show x, Show sp) => Show (SceneNode sp np cmd x) where
  show (Invoke iv) = "[Invoke:" ++ show iv ++ "]"
  show (Group cmds) = "[Group:" ++ show cmds ++ "]"
  show (Transformer t gr) = "[Transformer]"

instance Functor (SceneNode sp np cmd) where
  fmap f (Invoke x) = Invoke x
  fmap f (Group cmds) = Group (fmap f cmds)
  fmap f (Transformer t gr) = Transformer t gr
  


type SceneGraph sp np cmd = Fix (SceneNode sp np cmd)



foldAlgebra :: (Monoid m) => SceneNode sp np cmd m -> m
foldAlgebra (Invoke x)         = mempty
foldAlgebra (Group cmds)       = foldr mappend mempty cmds
foldAlgebra (Transformer t gr) = cata foldAlgebra gr


foldSceneGraph :: (Monoid m) => SceneGraph sp np cmd -> m
foldSceneGraph g = cata foldAlgebra g


--
-- DrawCmd is basically a ReaderT monad, where the input @r@
-- is the set of render parameters for the scene graph.
--


newtype DrawCmd r m a = DC { runDC :: r -> m a }

noopDraw :: (Monad m) => a -> DrawCmd r m a
noopDraw x = return x

instance (Functor m) => Functor (DrawCmd r m) where
  fmap f dc = DC $ \r -> fmap f (runDC dc r)

instance (Applicative m) => Applicative (DrawCmd r m) where
  pure x             = DC $ \_ -> pure x
  (DC fa) <*> (DC b) = DC $ \r -> (fa r) <*> (b r)

instance (Monad m) => Monad (DrawCmd r m) where
  return     = pure
  a >>= b    = DC $ \r -> do a' <- runDC a r
                             runDC (b a') r

instance (MonadIO m) => MonadIO (DrawCmd r m) where
  liftIO x = DC $ \r -> liftIO x
  
instance (Alternative m) => Alternative (DrawCmd r m) where
  empty             = DC $ \r -> empty
  (DC a) <|> (DC b) = DC $ \r -> (a r) <|> (b r)

instance (MonadPlus m) => MonadPlus (DrawCmd r m) where
  mzero     = DC $ \r -> mzero
  mplus m n = DC $ \r -> mplus (runDC m r) (runDC n r)




--
-- The Transformer switches from a (SceneNode r) to a (SceneNode s) so we're
-- going to need to switch the (Invoker r) to a (Invoker s). This
-- is why 'iv' needs to a higher rank type.
--

data DumpScene

instance DrawMethod DumpScene where
  type ShaderReady DumpScene sp = (Show sp)


dumpAlgebra :: (MonadIO m) => SceneNode sp np DumpScene (DrawCmd (FrameData sp np DumpScene) m ()) -> DrawCmd (FrameData sp np DumpScene) m ()
dumpAlgebra (Invoke x)     = liftIO $ do putStrLn $ show (rvalf #shader x)
                                         return ()
dumpAlgebra (Group cmds)   = foldl (>>) (DC $ \_ -> return ()) cmds
dumpAlgebra (Transformer t gr) = DC $ \r ->
  let s = t r
      scmd = cata dumpAlgebra gr
  in runDC scmd s

dumpScene :: (MonadIO m) => SceneGraph sp np DumpScene-> DrawCmd (FrameData sp np DumpScene) m ()
dumpScene sg = cata dumpAlgebra sg

--
-- Find resources for a scene
--


newtype OGLResources a = OGLResources { needsGLResources :: ResourceList }

oglResourcesAlgebra :: SceneNode sp np cmd ResourceList -> ResourceList
oglResourcesAlgebra (Invoke x) = ResourceList {
        shaderfiles =  S.singleton (rvalf #shader x), 
        vertexfiles =  S.fromList (rvalf #vertexBuffers x),
        texturefiles = S.fromList (rvalf #textures x)
      }
oglResourcesAlgebra (Group cmds) = foldl mergeResourceLists emptyResourceList cmds
oglResourcesAlgebra (Transformer t gr) = oglResourcesScene gr

oglResourcesScene :: SceneGraph sp np cmd -> ResourceList
oglResourcesScene sg = cata oglResourcesAlgebra sg


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
                  

transformer :: forall sp np sf nf sp2 np2 sf2 nf2 cmd. (sp ~ FieldRec sf, np ~ FieldRec nf, sp2 ~ FieldRec sf2, np2 ~ FieldRec nf2) =>
  (FrameData sp np cmd -> FrameData sp2 np2 cmd) ->
  (SceneGraph sp2 np2 cmd) ->
  Fix (SceneNode sp np cmd)
transformer t sg = Fix $ Transformer t sg

group :: [Fix (SceneNode sp np cmd)] -> Fix (SceneNode sp np cmd)
group xs = Fix $ Group xs

