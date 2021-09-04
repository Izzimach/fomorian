
module Fomorian.Vulkan.PlatformRenderer where

import Data.IORef

import qualified Graphics.UI.GLFW as GLFW

import Fomorian.Windowing
import Fomorian.SceneNode
import Fomorian.SceneResources

import Fomorian.Vulkan.WindowEtc
import Fomorian.Vulkan.SwapchainEtc

import Fomorian.ThreadedApp
import Fomorian.SimpleApp

data VulkanRendererState =
  VulkanRendererState {
    rendererAppInfo :: IORef AppInfo,
    rendererWindow :: GLFW.Window,
    windowEtc :: WindowEtc
  }

initAppState :: WindowInitData -> GLFW.Window -> IO (IORef AppInfo)
initAppState (WindowInitData w h _ _) win = do
  let initialAppState =    (#window .== win)
                        .+ (#windowSize .== (w,h))
                        .+ (#resources .== noLoadedResources)
                        .+ (#curTime .== (0 :: Float))
                        .+ (#shouldTerminate .== False)
  appIORef <- newIORef initialAppState
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GLFW.setWindowSizeCallback win (Just $ resizeWindow appIORef)
  return appIORef

vulkanWrapRender :: (Int,Int) -> (VulkanRendererState -> IO ()) -> IO ()
vulkanWrapRender (w,h) wrapped = do
  let allocator = Nothing
  let vulkanConfig = VulkanConfig (addValidation defaultInstanceConfig) cMAX_FRAMES_IN_FLIGHT
  let windowConfig = WindowInitData w h "Vulkan test window" NoOpenGL
  withWindowEtc vulkanConfig windowConfig allocator bracket $ \windowETC -> do
    let win = windowHandle windowETC
    appState <- initAppState windowConfig win
    wrapped (VulkanRendererState _ win)
    deviceWaitIdle (vkDevice windowETC)

vulkanRendererFunctions :: PlatformRendererFunctions VulkanRendererState
vulkanRendererFunctions =
  PlatformRendererFunctions  {
    wrapRenderLoop = vulkanWrapRender,
    getAppInfo = rendererAppInfo,
    runRenderFrame = undefined
  }