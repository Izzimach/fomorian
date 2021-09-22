{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
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

type instance RendererResources VulkanRendererState = LoadedResources (DataSource VulkanDataSourceTypes) (Resource VulkanResourceTypes)

type VulkanResources = RendererResources VulkanRendererState

data VulkanRendererState =
  VulkanRendererState {
    rendererAppInfo :: IORef (AppInfo VulkanResources),
    rendererWindow :: GLFW.Window,
    windowBundle :: WindowBundle,
    flightTracker :: IORef InFlightTracker
  }

initAppState :: WindowInitData -> GLFW.Window -> IO (IORef (AppInfo VulkanResources))
initAppState (WindowInitData w h _ _) win = do
  let initialAppState =    (#window .== win)
                        .+ (#windowSize .== (w,h))
                        .+ (#resources .== noLoadedResources)
                        .+ (#curTime .== (0 :: Float))
                        .+ (#shouldTerminate .== False)
  newIORef initialAppState

vulkanWrapRender :: (Int,Int) -> (VulkanRendererState -> IO ()) -> IO ()
vulkanWrapRender (w,h) wrapped = do
  let initConfig = WindowInitConfig
                     "Vulkan App"  -- application name
                     "Vulkan Test" -- window title
                      Nothing      -- allocator
                      False        -- enable debug validation layers?
  withWindowBundle initConfig $ \windowBundle -> do
    return ()
{-  withWindowEtc vulkanConfig windowConfig allocator bracket $ \windowETC -> do
    let win = windowHandle windowETC
    appState <- initAppState windowConfig win
    inFlightData <- mkInFlightTracker windowETC 
    inFlightRef <- newIORef inFlightData
    wrapped (VulkanRendererState appState win windowETC inFlightRef)
    deviceWaitIdle (vkDevice windowETC)-}

vulkanRenderFrame :: VulkanRendererState -> SceneGraph NeutralSceneTarget TopLevel3DRow -> Rec TopLevel3DRow -> IO Bool
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

vulkanRendererFunctions :: PlatformRendererFunctions VulkanRendererState
vulkanRendererFunctions =
  PlatformRendererFunctions {
    wrapRenderLoop = vulkanWrapRender,
    getAppInfo = rendererAppInfo,
    runRenderFrame = vulkanRenderFrame
  }