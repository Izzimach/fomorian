{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
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

{-|
Description: Scene node description as a recursive datatype.
-}
module Fomorian.SceneNode where

import qualified GHC.Generics as GHC

import Linear

import Data.Kind (Constraint)
import Data.Functor.Foldable
import Data.Fix (Fix(..))

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Applicative

-- row-types
import Data.Row
import Data.Row.Records


-- | The type 'a' is a draw command type. The kind 'r' is a type-level list
--   of the values needed to draw things in the draw command 'a'.
--   Specifically, to create an 'Invvoke' node with the draw commmand 'cmd'
--   you need to provide data parameters of type @InvokeReq cmd@.

--   Some examples:
--   - 'NoDraw' has an empty list of requrements 'Empty'
--   - 'DebugDump' just prints out the labels on invoke nodes, so it requires a string label on invoke nodes.
type family InvokeReq a (sreq :: Row s) :: Constraint

type family FrameReq a (dreq :: Row r) :: Constraint


-- | NoDraw is a draw command that does nothing, used for testing/empty folds
data NoDraw
type instance (InvokeReq NoDraw sreq) = ()
type instance (FrameReq NoDraw dreq) = ()

-- | DebugDumpDraw is a draw command for dumping out text to view the tree structure. See 'dumpScene'. Each node has a label.
data DebugDump
type instance (InvokeReq DebugDump sreq) = (HasType "label" String sreq)
type instance (FrameReq DebugDump dreq) = ()




-- | A node of a scene graph. Parameters are sadly not intuitive:
--   - 'dreq' is basically a type signature of the inputs needed by this node/graph. 'dreq' is a 'Row' that
--     lists the parameters needed by this node. So when you "call" this graph using
--     some sort of draw command you will need to provide a record holding all the parameters listed in 'dreq'
--   - 'cmd' is the draw command which indirectly represents the thing you are rendering to. Different render
--     backends would use different 'cmd' types. There are also commands like 'NoDraw' or 'DebugDump' that "render"
--     nothing or produce a string as output.
--   - The final parameter 'x' holds node children. You make this a recursive type using 'Fix'
--
data SceneNode dreq cmd x where
  -- | Here is where the actual drawing happens. Static parameters in the scene graph are combined with
  --   dynamic parameters provided by the caller to produce output. Typically this is a monad that runs draw commands.
  Invoke :: (InvokeReq cmd sreq, FrameReq cmd dreq) => Rec sreq -> SceneNode dreq cmd x
  -- | Translates the dynamic data, adding or removing elements from the dynamic record that is
  --   per-frame data. Use this to compute per-frame values or override defaults that were passed in at the top.
  Transformer :: (Rec dreq -> Rec dreq2) -> SceneGraph dreq2 cmd -> SceneNode dreq cmd x
  -- | Holds a bunch of children nodes. All children need to be of the same type.
  Group :: [x] -> SceneNode dreq cmd x

instance Functor (SceneNode dreq cmd) where
  fmap _ (Invoke r) = Invoke r
  fmap _ (Transformer t s) = Transformer t s
  fmap f (Group xs) = Group (fmap f xs)

type SceneGraph r cmd = Fix (SceneNode r cmd)

modifyFields :: (Rec rowin -> Rec rowadd) -> Rec rowin -> Rec (rowadd .// rowin)
modifyFields f x = (f x) .// x

setFields :: (Rec rowin -> Rec rowmodify) -> SceneGraph (rowmodify .// rowin) cmd -> SceneNode (rowin) cmd x
setFields f m = Transformer (modifyFields f) m

setViewMatrix :: (HasType "x" (V3 Float) r) => Rec r -> Rec ("viewMatrix" .== Float)
setViewMatrix r =
  let (V3 x _ _) = r .! #x
  in (#viewMatrix .== x)

viewMatrixNode :: (HasType "x" (V3 Float) r) => SceneGraph (("viewMatrix" .== Float) .// r) cmd -> SceneNode r cmd x
viewMatrixNode p = setFields setViewMatrix p


--
-- DrawCmd is basically a ReaderT monad, where the input @r@
-- is the set of render parameters for the scene graph.
--
newtype DrawCmd r m a = DC { runDC :: Rec r -> m a }

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
  liftIO x = DC $ \_ -> liftIO x
  
instance (Alternative m) => Alternative (DrawCmd r m) where
  empty             = DC $ \_ -> Control.Applicative.empty
  (DC a) <|> (DC b) = DC $ \r -> (a r) <|> (b r)

instance (MonadPlus m) => MonadPlus (DrawCmd r m) where
  mzero     = DC $ \_ -> mzero
  mplus m n = DC $ \r -> mplus (runDC m r) (runDC n r)

type DrawCmdAlgebra   r cmd m = SceneNode r cmd (DrawCmd r m ()) -> DrawCmd r m ()
type DrawCmdAlgebraIO r cmd m = (MonadIO m) => SceneNode r cmd (DrawCmd r m ()) -> DrawCmd r m ()

cataDrawCmd :: Fix (SceneNode r cmd) -> DrawCmdAlgebra r cmd m -> DrawCmd r m ()
cataDrawCmd sg alg = cata alg sg

cataDrawCmdIO :: (MonadIO m) => Fix (SceneNode r cmd) -> DrawCmdAlgebraIO r cmd m -> DrawCmd r m ()
cataDrawCmdIO sg alg = cata alg sg


dumpAlgebra :: (MonadIO m) => SceneNode r DebugDump (DrawCmd r m ()) -> DrawCmd r m ()
dumpAlgebra (Invoke x)     = liftIO $ do putStrLn $ show (x .! #label)
                                         return ()
dumpAlgebra (Group cmds)   = foldl (>>) (DC $ \_ -> return ()) cmds
dumpAlgebra (Transformer f gr) = DC $ \r ->
  let s = f r
      scmd = cata dumpAlgebra gr
  in runDC scmd s

-- | Just dumps out labels of the node in order to stdout
dumpScene :: (MonadIO m) => Fix (SceneNode r DebugDump) -> DrawCmd r m ()
dumpScene sg = cata dumpAlgebra sg

sceneLabels :: (MonadIO m) => Fix (SceneNode Empty DebugDump) -> m ()
sceneLabels sg = let cmd = dumpScene sg
                 in (runDC cmd) Data.Row.Records.empty
                 


-- | Dot/graphviz data for a particular node. The first field is the lable for that node as a string.
--   The second field is a list of edgestrings, or strings where each element is an edge (in dot format) of the subtree.
data NodeVizData = NodeVizData { vizLabel ::String, edgeStrings :: [String] }
  deriving (Eq, Show, GHC.Generic)

-- | Given a label for the current node and 'NodeVizData' of all children, combine together to
--   form a 'NodeVizData' for this node
combineNodeViz :: String -> [NodeVizData] -> NodeVizData
combineNodeViz nodelabel childrenvizdata =
  let child_labels = fmap vizLabel childrenvizdata
      my_edgestrings = fmap (\x -> nodelabel ++ " -> " ++ x ++ "\n") child_labels
      child_edgestrings = concat (fmap edgeStrings childrenvizdata)
  in NodeVizData nodelabel ([nodelabel ++ "\n"] ++ my_edgestrings ++ child_edgestrings)

dumpDotAlgebra :: SceneNode r DebugDump (State Integer NodeVizData) -> State Integer NodeVizData
dumpDotAlgebra (Invoke x) = let label = x .! #label
                             in return $ NodeVizData label [label ++ "\n"]
dumpDotAlgebra (Group cmds) = do i <- get
                                 let foldedcmds = sequenceA cmds
                                 let (x,i') = runState foldedcmds (i+1) 
                                 put i'
                                 let my_label = "node" ++ show i
                                 let my_vizdata = combineNodeViz my_label x
                                 return my_vizdata
dumpDotAlgebra (Transformer _ gr) = do i <- get
                                       let (d, i') = runState (dumpDotScene (gr)) i
                                       put i'
                                       return d

dumpDotScene :: Fix (SceneNode r DebugDump) -> State Integer NodeVizData
dumpDotScene sg = cata dumpDotAlgebra sg
                      
dotStringScene :: Fix (SceneNode r DebugDump) -> String
dotStringScene sg =
  let ((NodeVizData _ edges),_) = runState (dumpDotScene sg) 0
  in "digraph {\n" ++ (concat edges) ++ "}"



-- | Shortcut you can use instead of calling constructors and 'Fix' directly
transformer :: (Rec r1 -> Rec r2) -> (SceneGraph r2 cmd) -> Fix (SceneNode r1 cmd)
transformer t sg = Fix $ Transformer t sg

group :: [Fix (SceneNode r cmd)] -> SceneGraph r cmd
group xs = Fix $ Group xs

invoke :: (InvokeReq cmd s, FrameReq cmd r) => Rec s -> SceneGraph r cmd
invoke r = Fix $ Invoke r

