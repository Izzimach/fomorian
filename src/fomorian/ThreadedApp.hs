module Fomorian.ThreadedApp where

import Control.Exception

import Fomorian.SceneNode
import Fomorian.SceneResources
import Fomorian.OpenGLResources
import Fomorian.Windowing
import Fomorian.OpenGLCommand

import LoadUnload
import AsyncLoader

import Fomorian.SimpleApp


-- | A basic app that just runs a render function over and over.
threadedApp :: (Int, Int) -> (AppInfo -> SceneGraph TopLevel3DRow OpenGLTarget) -> IO ()
threadedApp (w,h) renderFunc = do
  let initData = WindowInitData w h "Haskell App" UseOpenGL
  let initfunc = initWindow initData
  let endfunc  = \win -> terminateWindow win
  let loopfunc = \win -> do
                           appdata <- initAppState initData win
                           renderLoop appdata renderFunc simpleAppRenderParams
  bracket initfunc endfunc loopfunc