{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.Vulkan.PlatformRenderer where

import Control.Exception

import Data.IORef
import Data.Row
import Data.Row.Records

import qualified Graphics.UI.GLFW as GLFW

import Fomorian.Windowing
import Fomorian.SceneNode
import Fomorian.SceneResources
import Fomorian.NeutralSceneTarget

import Vulkan.Core10.Queue

import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.SwapChainEtc
import Fomorian.Vulkan.VulkanResources
import Fomorian.Vulkan.Example (InFlightTracker, mkInFlightTracker, renderFrame, cMAX_FRAMES_IN_FLIGHT)

import Fomorian.ThreadedApp
import Fomorian.SimpleApp (AppInfo, TopLevel3DRow)

import LoadUnload

type instance RendererResources (VulkanRendererState j) = LoadedResources (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)

data VulkanRendererState j =
  VulkanRendererState {
    rendererAppInfo :: IORef (AppState j),
    windowBundle :: WindowBundle
  }

type StdState = 
  (
    "window"     .== GLFW.Window .+
    "windowSize" .== (Int,Int)   .+
    "curTime"    .== Float
  )


initAppState :: WindowInitData -> GLFW.Window -> IO (IORef (AppState StdState))
initAppState (WindowInitData w h _ _) win = do
  let initialAppState =    (#window .== win)
                        .+ (#windowSize .== (w,h))
                        .+ (#curTime .== (0 :: Float))
  newIORef (AppState initialAppState)

vulkanWrapRender :: (Int,Int) -> (VulkanRendererState StdState -> IO ()) -> IO ()
vulkanWrapRender (w,h) wrapped = do
  let initConfig = WindowInitConfig
                     "Vulkan App"  -- application name
                     "Vulkan Test" -- window title
                      Nothing      -- allocator
                      False        -- enable debug validation layers?
  withWindowBundle initConfig $ \windowBundle -> do
    let win = windowHandle windowBundle
    appState <- initAppState (WindowInitData w h "Vulkan" NoOpenGL) win
    wrapped (VulkanRendererState appState windowBundle)
{-  withWindowEtc vulkanConfig windowConfig allocator bracket $ \windowETC -> do
    let win = windowHandle windowETC
    appState <- initAppState windowConfig win
    inFlightData <- mkInFlightTracker windowETC 
    inFlightRef <- newIORef inFlightData
    wrapped (VulkanRendererState appState win windowETC inFlightRef)
    deviceWaitIdle (vkDevice windowETC)-}

vulkanRenderFrame :: VulkanRendererState appRow -> SceneGraph NeutralSceneTarget TopLevel3DRow -> Rec TopLevel3DRow -> IO Bool
vulkanRenderFrame v scene frameData = do
  return False
  {-
  let wEtc = windowEtc v
  let win = windowHandle wEtc
  renderFrame wEtc (flightTracker v) Nothing
  GLFW.pollEvents
  p <- GLFW.getKey win GLFW.Key'Escape
  shouldClose <- GLFW.windowShouldClose win
  return (shouldClose || (p == GLFW.KeyState'Pressed))
  -}

vulkanRendererFunctions :: PlatformRendererFunctions (VulkanRendererState StdState) StdState
vulkanRendererFunctions =
  PlatformRendererFunctions {
    wrapRenderLoop = vulkanWrapRender,
    getAppInfo = rendererAppInfo,
    runRenderFrame = vulkanRenderFrame
  }