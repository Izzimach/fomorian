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

import Data.Constraint
import Data.Kind (Constraint, Type)
import Data.Functor.Foldable
import Data.Fix (Fix(..))

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Applicative

-- row-types
import Data.Row
import Data.Row.Records


-- | The type 'cmdProxy' is a type describing the eventual product that gets generated on a draw.  Often this will be a monad such as
--   'DrawCmd' but it could also be a pure result like a text dump. The kinds 'ir' and 'dr' are type-level lists
--   of the values needed to generate the draw caommdn. 'ir' is used when creating the 'Invoke' nodes in the scene graph.
--   'dr' is used when drawing, which in this case means converting a scenegraph into a draw command.
--   Some examples:
--   - 'NoDrawCmd' has an empty list of requrements 'Empty' for generating a draw command that does nothing.
--   - 'DebugDumpCmd' just prints out the labels on invoke nodes, so it requires a string label on invoke nodes.
type family InvokeReq cmdProxy (ir :: Row Type) :: Constraint
type family DrawReq cmdProxy (dr :: Row Type) :: Constraint

-- | Empty draw command 
data NoDrawProxy
type instance InvokeReq NoDrawProxy s = ()
type instance DrawReq NoDrawProxy s = ()

-- | DebugDump is a draw command for dumping out text to view the tree structure. See 'dumpScene'. Each node has a label.
data DebugDumpProxy
type instance InvokeReq DebugDumpProxy s = (HasType "label" String s)
type instance DrawReq DebugDumpProxy s = ()




-- | A node of a scene graph. The type has two sets of constraints:
--   - 'dParam' are draw parameters, basically witness of the dynamic per-frame inputs needed by this node/graph to draw a frame.
--     So when you "call" this graph using some sort of draw command you will need to provide a record holding all the parameters listed in 'dreq'.
--     Examples of dynamic parameters typically needed: viewport size, current time
--   - 'cmd' is the draw command to use. This restricts the record that you can use in 'Invoke' nodes and the set of parameters in 'dParam' when you
--     hit an invoke node.
--
data SceneGraph (dr :: Row Type) cmdProxy where
  -- | Here is where the actual drawing happens. Static parameters in the scene graph are combined with
  --   dynamic parameters provided by the caller to produce output. Typically this is a monad that runs draw commands.
  Invoke :: (InvokeReq cmdProxy ir, DrawReq cmdProxy dr) => Rec ir -> SceneGraph dr cmdProxy
  -- | Translates the dynamic data, adding or removing elements from the dynamic record that is
  --   per-frame data. Use this to compute per-frame values or override defaults that were passed in at the top.
  Transformer :: (Rec dr -> Rec dr2) -> SceneGraph dr2 cmd -> SceneGraph dr cmd
  -- | Holds a bunch of children nodes. All children need to be of the same type.
  Group :: [SceneGraph dr cmd] -> SceneGraph dr cmd

-- | Non-recursive functor version of SceneGraphF for recursion-schemes
data SceneGraphF dreq cmd x where
  InvokeF :: (InvokeReq cmd s, DrawReq cmd dReq) => Rec s -> SceneGraphF dReq cmd x
  TransformerF :: (Rec dr -> Rec dr2) -> SceneGraph dr2 cmd -> SceneGraphF dr cmd x
  GroupF :: [x] -> SceneGraphF dReq cmd x

type instance Base (SceneGraph dReq cmd) = SceneGraphF dReq cmd

instance Functor (SceneGraphF dReq cmd) where
  fmap _ (InvokeF r)        = InvokeF r
  fmap _ (TransformerF t s) = TransformerF t s
  fmap f (GroupF xs)        = GroupF (fmap f xs)

instance Recursive (SceneGraph dreq sreq) where
  project (Invoke s) = InvokeF s
  project (Transformer t s) = TransformerF t s
  project (Group xs) = GroupF xs

modifyFields :: (Rec rowin -> Rec rowadd) -> Rec rowin -> Rec (rowadd .// rowin)
modifyFields f x = (f x) .// x

setFields :: (Rec rowin -> Rec rowmodify) -> SceneGraph (rowmodify .// rowin) cmd -> SceneGraph (rowin) cmd
setFields f m = Transformer (modifyFields f) m

setViewMatrix :: (HasType "x" (V3 Float) r) => Rec r -> Rec ("viewMatrix" .== Float)
setViewMatrix r =
  let (V3 x _ _) = r .! #x
  in (#viewMatrix .== x)

viewMatrixNode :: (HasType "x" (V3 Float) r) => SceneGraph (("viewMatrix" .== Float) .// r) cmd -> SceneGraph r cmd
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

type DrawCmdAlgebra   dreq sreq m =                SceneGraphF dreq sreq (DrawCmd dreq m ()) -> DrawCmd dreq m ()
type DrawCmdAlgebraIO dreq sreq m = (MonadIO m) => SceneGraphF dreq sreq (DrawCmd dreq m ()) -> DrawCmd dreq m ()

cataDrawCmd :: SceneGraph dreq sreq -> DrawCmdAlgebra dreq sreq m -> DrawCmd dreq m ()
cataDrawCmd sg alg = cata alg sg

dumpAlgebra :: (MonadIO m) => SceneGraphF dreq DebugDumpProxy (DrawCmd dreq m ()) -> DrawCmd dreq m ()
dumpAlgebra (InvokeF x)         = liftIO $ do putStrLn $ show (x .! #label)
                                              return ()
dumpAlgebra (GroupF cmds)       = foldl (>>) (DC $ \_ -> return ()) cmds
dumpAlgebra (TransformerF t gr) = DC $ \r ->
    let s = t r
        scmd = cata dumpAlgebra gr
    in runDC scmd s

-- | Just dumps out labels of the node in order to stdout
dumpScene :: (MonadIO m) => SceneGraph Empty DebugDumpProxy -> DrawCmd Empty m ()
dumpScene sg = cata dumpAlgebra sg

sceneLabels :: (MonadIO m) => SceneGraph Empty DebugDumpProxy -> m ()
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

dumpDotAlgebra :: SceneGraphF dreq DebugDumpProxy (State Integer NodeVizData) -> State Integer NodeVizData
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

dumpDotScene :: SceneGraph dreq DebugDumpProxy -> State Integer NodeVizData
dumpDotScene sg = cata dumpDotAlgebra sg
                      
dotStringScene :: SceneGraph dreq DebugDumpProxy -> String
dotStringScene sg =
  let ((NodeVizData _ edges),_) = runState (dumpDotScene sg) 0
  in "digraph {\n" ++ (concat edges) ++ "}"



-- | Shortcut you can use instead of calling constructors and 'Fix' directly
transformer :: (Rec dreq -> Rec dreq2) -> (SceneGraph dreq2 cmd) -> SceneGraph dreq cmd
transformer t sg = Transformer t sg

group :: [SceneGraph dreq cmd] -> SceneGraph dreq cmd
group xs = Group xs

invoke :: (InvokeReq cmd s, DrawReq cmd dReq) => Rec s -> SceneGraph dReq cmd
invoke r = Invoke r

