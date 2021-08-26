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
Description: Scene node description as a recursive datatype. It can be a graph (DAG) but is usually simply a tree.
To "draw" the scene graph is to fold the tree; usually this is @cata alg@ where the type of @alg@ dictates the result
of the draw/fold.
-}
module Fomorian.SceneNode where

import qualified GHC.Generics as GHC

import Data.Constraint
import Data.Kind (Type)
import Data.Functor.Foldable

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Applicative

-- row-types
import Data.Row
import Data.Row.Records


-- | The type 'target' is a type describing the eventual product that gets generated on a draw (fold).
--   Often this will be a monad such as
--   'DrawCmd' but it could also be a pure result like a text dump. The kinds 'ir' and 'dr' are type-level lists
--   of the values needed to generate the draw command. 'ir' is used when creating the 'Invoke' nodes in the scene graph.
--   'dr' is used during the actual draw, which in this case means converting a scenegraph into a draw command that takes 'dr' as input.
--   Some examples:
--   - 'NoDrawCmd' has an empty list of requrements 'Empty' for generating a draw command that does nothing.
--   - 'DebugDumpCmd' just prints out the labels on invoke nodes, so it requires a string label on invoke nodes.
type family InvokeReq target (ir :: Row Type) :: Constraint
type family DrawReq target (dr :: Row Type) :: Constraint

-- | Empty draw target - drawing produces unit @()@ and per-frame input 'dr' is ignored
data NilTarget
type instance InvokeReq NilTarget ir = ()
type instance DrawReq NilTarget dr = ()

-- | DebugDump is a draw command for dumping out text to view the tree structure. See 'dumpScene'. Each node has a label.
data DebugDumpTarget
type instance InvokeReq DebugDumpTarget ir = (HasType "label" String ir)
type instance DrawReq DebugDumpTarget dr = ()




-- | A node of a scene graph. The type has two sets of constraints:
--   - 'target' is the draw target. Different targets produce different out when you "draw" them. Targets that are Graphics APIs will typically produce
--     a monad that does the actual drawing. Other targets might produce a list of resources or simply a text string.
--     The target dictates what parameters are in 'ir', which is the set of parameters you need when you build an 'Invoke' node.
--   - 'dr' are required draw parameters, basically witness of the dynamic per-frame inputs needed by this node/graph to draw a frame. This would
--     include things like the current time, window size/aspect, etc. Anything that changes from frame to frame.
--     So when you "call" this graph using some sort of draw command you will need to provide a record holding all the parameters listed in 'dr'.
--
--   'dr' = draw record, record that is needed to draw a single frame
--   'ir' = invoke record, record that is needed to build an 'Invoke' node
--
data SceneGraph target (dr :: Row Type) where
  -- | Here is where the actual drawing happens. Static parameters in the scene graph are combined with
  --   dynamic parameters provided by the caller to produce output. Typically this is a monad that runs draw commands.
  Invoke :: (InvokeReq target ir, DrawReq target dr) => Rec ir -> SceneGraph target dr
  -- | Translates the dynamic data, adding or removing elements from the dynamic record that is
  --   per-frame data. Use this to compute per-frame values or override defaults that were passed in at the top.
  Transformer :: (Rec dr -> Rec dr2) -> SceneGraph target dr2 -> SceneGraph target dr
  -- | Holds a bunch of children nodes. All children need to be of the same type.
  Group :: [SceneGraph target dr] -> SceneGraph target dr

-- | Non-recursive functor version of SceneGraphF for recursion-schemes
data SceneGraphF target (dr :: Row Type) x where
  InvokeF :: (InvokeReq target ir, DrawReq target dr) => Rec ir -> SceneGraphF target dr x
  TransformerF :: (Rec dr -> Rec dr2) -> SceneGraph target dr2 -> SceneGraphF target dr x
  GroupF :: [x] -> SceneGraphF dr target x

type instance Base (SceneGraph target dr) = SceneGraphF target dr

instance Functor (SceneGraphF target dr) where
  fmap _ (InvokeF r)        = InvokeF r
  fmap _ (TransformerF t s) = TransformerF t s
  fmap f (GroupF xs)        = GroupF (fmap f xs)

instance Recursive (SceneGraph target dr) where
  project (Invoke s) = InvokeF s
  project (Transformer t s) = TransformerF t s
  project (Group xs) = GroupF xs

modifyFields :: (Rec rowin -> Rec rowadd) -> Rec rowin -> Rec (rowadd .// rowin)
modifyFields f x = (f x) .// x

-- | Transforms the 'dr' row in a scene graph. Takes in a function that works the record 'dr' and then applies it to the scene graph.
--   Kinda like a lens?
setFields :: (Rec rowin -> Rec rowmodify) -> SceneGraph target (rowmodify .// rowin) -> SceneGraph target rowin
setFields f m = Transformer (modifyFields f) m

{-
setViewMatrix :: (HasType "x" (V3 Float) r) => Rec r -> Rec ("viewMatrix" .== Float)
setViewMatrix r =
  let (V3 x _ _) = r .! #x
  in (#viewMatrix .== x)

viewMatrixNode :: (HasType "x" (V3 Float) r) => SceneGraph (("viewMatrix" .== Float) .// r) cmd -> SceneGraph r cmd
viewMatrixNode p = setFields setViewMatrix p
-}

--
-- DrawCmd is basically a ReaderT monad, where the input @dr@
-- is the set of render parameters for the scene graph.
--
newtype DrawCmd dr m a = DC { runDC :: Rec dr -> m a }

noopDraw :: (Monad m) => a -> DrawCmd dr m a
noopDraw x = return x

instance (Functor m) => Functor (DrawCmd dr m) where
  fmap f dc = DC $ \r -> fmap f (runDC dc r)

instance (Applicative m) => Applicative (DrawCmd dr m) where
  pure x             = DC $ \_ -> pure x
  (DC fa) <*> (DC b) = DC $ \r -> (fa r) <*> (b r)

instance (Monad m) => Monad (DrawCmd dr m) where
  return     = pure
  a >>= b    = DC $ \r -> do a' <- runDC a r
                             runDC (b a') r

instance (MonadIO m) => MonadIO (DrawCmd dr m) where
  liftIO x = DC $ \_ -> liftIO x
  
instance (Alternative m) => Alternative (DrawCmd dr m) where
  empty             = DC $ \_ -> Control.Applicative.empty
  (DC a) <|> (DC b) = DC $ \r -> (a r) <|> (b r)

instance (MonadPlus m) => MonadPlus (DrawCmd dr m) where
  mzero     = DC $ \_ -> mzero
  mplus m n = DC $ \r -> mplus (runDC m r) (runDC n r)

type DrawCmdAlgebra   target dr m =                SceneGraphF target dr (DrawCmd dr m ()) -> DrawCmd dr m ()
type DrawCmdAlgebraIO target dr m = (MonadIO m) => SceneGraphF target dr (DrawCmd dr m ()) -> DrawCmd dr m ()

cataDrawCmd :: SceneGraph target dr -> DrawCmdAlgebra target dr m -> DrawCmd dr m ()
cataDrawCmd sg alg = cata alg sg

dumpAlgebra :: (MonadIO m) => SceneGraphF DebugDumpTarget dr (DrawCmd dr m ()) -> DrawCmd dr m ()
dumpAlgebra (InvokeF x)         = liftIO $ do putStrLn $ show (x .! #label)
                                              return ()
dumpAlgebra (GroupF cmds)       = foldl (>>) (DC $ \_ -> return ()) cmds
dumpAlgebra (TransformerF t gr) = DC $ \r ->
    let s = t r
        scmd = cata dumpAlgebra gr
    in runDC scmd s

-- | Just dumps out labels of the node in order to stdout
dumpScene :: (MonadIO m) => SceneGraph DebugDumpTarget Empty-> DrawCmd Empty m ()
dumpScene sg = cata dumpAlgebra sg

sceneLabels :: (MonadIO m) => SceneGraph DebugDumpTarget Empty-> m ()
sceneLabels sg = (runDC $ dumpScene sg) Data.Row.Records.empty
                 


-- | Dot/graphviz data for a particular node. The first field is the lable for that node as a string.
--   The second field is a list of edgestrings, or strings where each element is an edge (in dot format) of the subtree.
data NodeVizData = NodeVizData { vizLabel :: String, edgeStrings :: [String] }
  deriving (Eq, Show, GHC.Generic)

-- | Given a label for the current node and 'NodeVizData' of all children, combine together to
--   form a 'NodeVizData' for this node
combineNodeViz :: String -> [NodeVizData] -> NodeVizData
combineNodeViz nodelabel childrenvizdata =
  let child_labels = fmap vizLabel childrenvizdata
      my_edgestrings = fmap (\x -> nodelabel ++ " -> " ++ x ++ "\n") child_labels
      child_edgestrings = concat (fmap edgeStrings childrenvizdata)
  in NodeVizData nodelabel ([nodelabel ++ "\n"] ++ my_edgestrings ++ child_edgestrings)

dumpDotAlgebra :: SceneGraphF DebugDumpTarget dr (State Integer NodeVizData) -> State Integer NodeVizData
dumpDotAlgebra (InvokeF x)   = let l = x .! #label
                               in return $ NodeVizData l [l ++ "\n"]
dumpDotAlgebra (GroupF cmds) = do i <- get
                                  let foldedcmds = sequenceA cmds
                                  let (x,i') = runState foldedcmds (i+1) 
                                  put i'
                                  let my_label = "node" ++ show i
                                  let my_vizdata = combineNodeViz my_label x
                                  return my_vizdata
dumpDotAlgebra (TransformerF _ gr) = do i <- get
                                        let (d, i') = runState (dumpDotScene (gr)) i
                                        put i'
                                        return d

dumpDotScene :: SceneGraph DebugDumpTarget dr -> State Integer NodeVizData
dumpDotScene sg = cata dumpDotAlgebra sg
                      
dotStringScene :: SceneGraph DebugDumpTarget dr -> String
dotStringScene sg =
  let ((NodeVizData _ edges),_) = runState (dumpDotScene sg) 0
  in "digraph {\n" ++ (concat edges) ++ "}"



-- | Shortcut you can use instead of calling constructors and 'Fix' directly
transformer :: (Rec dr -> Rec dr2) -> (SceneGraph target dr2) -> SceneGraph target dr
transformer t sg = Transformer t sg

group :: [SceneGraph dr target] -> SceneGraph dr target
group xs = Group xs

invoke :: (InvokeReq target s, DrawReq target dr) => Rec s -> SceneGraph target dr
invoke r = Invoke r

