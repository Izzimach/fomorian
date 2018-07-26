{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Sample where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import Linear
import Data.Vinyl
import Data.Word (Word32)
import Graphics.VinylGL

import Data.IORef
import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Lens ((^.), (.~))
import Control.Exception
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System.FilePath ((</>))

import qualified Windowing as W
import SceneNode
import SceneResources


sampleScene :: SceneGraph
sampleScene = let snode = (  
                         (#shader =: "linez")
                      :& (#shaderParameters =: ((#tex =: (0 :: GLint)) :& RNil) )
                      :& (#vertexBuffers =: [
                            V2Data [V2 10 10, V2 100 10, V2 10 100, V2 100 100],
                            IndexData [0,1,2, 2,1,3]
                          ]
                         )
                      :& (#textures =: ["lollipopGreen.png"])
                      :& (#children =: ([] :: [SceneNodeBox]))
                      :& RNil
                      ) -- :: (SceneNodeR (FieldRec '[ '("tex", GLint)]))
             in SG snode
               

main :: IO ()
main = do
  let scene = sampleScene
  (win,appdata) <- W.initWindow
  W.renderLoop win appdata (const scene)
  W.terminateWindow win

