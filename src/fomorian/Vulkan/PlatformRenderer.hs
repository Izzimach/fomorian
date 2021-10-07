{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Fomorian.Vulkan.PlatformRenderer where

import Data.IORef
import Data.Row

import Linear

import qualified Graphics.UI.GLFW as GLFW

import Fomorian.Windowing
import Fomorian.SceneNode
import Fomorian.SceneResources
import Fomorian.NeutralSceneTarget

import Fomorian.Vulkan.WindowBundle
import Fomorian.Vulkan.SwapChainEtc
import Fomorian.Vulkan.Example 

import Fomorian.PlatformRenderer

import LoadUnload

data VulkanRendererState =
  VulkanRendererState {
    windowBundle :: WindowBundle,
    vulkanStats :: IORef RenderStats
  }

vulkanWrapRender :: (Int,Int) -> (VulkanRendererState -> IO ()) -> IO ()
vulkanWrapRender (w,h) wrapped = do
  let initConfig = WindowInitConfig
                     "Vulkan App"  -- application name
                     "Vulkan Test" -- window title
                      Nothing      -- allocator
                      4            -- auxiliary queue count
                      False        -- enable debug validation layers?
  withWindowBundle initConfig $ \windowBundle -> do
    let win = windowHandle windowBundle
    let stats = RenderStats win (V2 w h) 0
    statsRef <- newIORef stats
    wrapped (VulkanRendererState windowBundle statsRef)
{-  withWindowEtc vulkanConfig windowConfig allocator bracket $ \windowETC -> do
    let win = windowHandle windowETC
    appState <- initAppState windowConfig win
    inFlightData <- mkInFlightTracker windowETC 
    inFlightRef <- newIORef inFlightData
    wrapped (VulkanRendererState appState win windowETC inFlightRef)
    deviceWaitIdle (vkDevice windowETC)-}

vulkanRenderFrame :: VulkanRendererState -> SceneGraph NeutralSceneTarget DefaultDrawFrameParams -> Rec DefaultDrawFrameParams -> IO Bool
vulkanRenderFrame v scene frameData = do
  modifyIORef (vulkanStats v) (\s -> s { frameCount = (frameCount s) + 1 })
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
    getRendererStats = readIORef . vulkanStats,
    runRenderFrame = vulkanRenderFrame
  }