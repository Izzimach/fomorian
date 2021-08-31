
module Fomorian.Vulkan.PlatformRenderer where


import Fomorian.Windowing
import Fomorian.SceneNode
import Fomorian.SceneResources

import Fomorian.Vulkan.WindowEtc
import Fomorian.Vulkan.SwapchainEtc

import Fomorian.ThreadedApp
import Fomorian.SimpleApp

data VulkanRendererState =
  VulkanRendererState {

  }

vulkanRendererFunctions :: PlatformRendererFunctions VulkanRendererState
vulkanRendererFunctions =
  PlatformRendererFunctions  {
    initializeRenderer = undefined,
    getAppInfo = undefined,
    runRenderFrame = undefined,
    shutdownRenderer = undefined
  }