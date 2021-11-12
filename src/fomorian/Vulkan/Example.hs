{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.Example where

import Control.Exception
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Monad.Freer

import Data.Foldable
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Vector as V ((!), (//), Vector, fromList, empty, singleton)
import Data.Row

import Linear

import Foreign.Ptr

import Text.Pretty.Simple (pPrint)
import System.FilePath ((</>))

import STMLoader.LoadUnload
import STMLoader.AsyncLoader

import qualified Graphics.UI.GLFW as GLFW
import Vulkan.CStruct.Extends
import Vulkan.Core10 as VK
import Vulkan.Zero as VZ
import Vulkan.Exception
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN

import Fomorian.Windowing
import Fomorian.SceneResources
import Fomorian.SimpleMemoryArena (MemoryAlignment(..))
import qualified Fomorian.Vulkan.WindowBundle as WB

import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.SwapchainCarousel
import Fomorian.Vulkan.SwapchainBundle
import Fomorian.Vulkan.Resources.Pipeline
import Fomorian.Vulkan.Resources.ImageBuffers
import Fomorian.Vulkan.Resources.DescriptorSets (UniformBufferObject(..), zeroOnePerspective, updateUniformBuffer)
import Fomorian.Vulkan.Resources.VulkanResourcesBase
import Fomorian.Vulkan.Resources.DeviceMemoryTypes
import Fomorian.Vulkan.Resources.BoundCommandBuffer
import Fomorian.Vulkan.Resources.VulkanLoader


runSomeVulkan :: IO ()
runSomeVulkan = do
  let cfg = WB.WindowInitConfig "app name" "title" Nothing 1 True
  WB.withWindowBundle cfg $ \wb -> do
    let pd = WB.physicalDeviceHandle (WB.vulkanDeviceBundle wb)
    deviceProps <- VK.getPhysicalDeviceProperties pd
    let deviceLimits = VK.limits deviceProps
    putStrLn $ "buffer-image granularity = " ++ show (bufferImageGranularity deviceLimits)
    -- fork off a queue submit thread using the aux queue
    let aQueue = head $ WB.auxiliaryQueues $ WB.vulkanDeviceBundle wb
    let prebuiltResources = M.empty
    boundQueue <- aQueue `seq` forkBoundSubmitter wb aQueue
    loaderInfo <- startLoader wb boundQueue prebuiltResources
    let basicVertSource = DataSource $ IsJust #wavefrontPath "testcube.obj"
    --let basicImageSource = DataSource $ IsJust #coordinates3d [(0,0,0),(1,0,0),(0,1,0)]
    let basicImageSource = DataSource $ IsJust #texturePath "owl.png"
    --newRequest loaderInfo sceneResources
    -- wait until loader loads our stuff
    resources <- waitForResourceProcessing loaderInfo (S.empty, S.fromList [basicVertSource, basicImageSource])
    let (Just (Resource basicVertData)) = M.lookup basicVertSource resources
    let (Just (Resource basicImageData)) = M.lookup basicImageSource resources
    pPrint basicVertData
    pPrint basicImageData
    let vertices = case trial basicVertData #vkGeometry of
                    Left _ -> error "Argh"
                    Right g -> g
    let imagez = case trial basicImageData #textureImage of
                    Left _ -> error "argh! textures"
                    Right t -> t


    let d = WB.deviceHandle $ WB.vulkanDeviceBundle wb
    let gQ = WB.graphicsQueue $ WB.vulkanDeviceBundle wb
    let gQIndex = WB.graphicsQueueFamilyIndex $ WB.vulkanDeviceBundle wb
    let pQ = WB.presentQueue $ WB.vulkanDeviceBundle wb
    let runV = runM . runVulkanMonad wb

    VK.withCommandPool d (CommandPoolCreateInfo VK.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT gQIndex) Nothing bracket $ \gPool -> do
        bracket (runV $ makeCarousel gPool gQ pQ 2 Nothing) (\swc -> runV $ do flushCarousel swc; destroyCarousel swc) $ \swc ->
          runV $ do
            syncDescriptorSets swc imagez
            forM_ [1..300] (\x -> presentNextSlot swc (clearCmd swc vertices x))

    endLoader loaderInfo
    endBoundSubmitter boundQueue
  where
    clearCmd swc vertGeo curTime cBuf frameBuf cSlot = do
      let swapchainBundle = swapchainB swc
      let windowExtent@(Extent2D w h) = VKSWAPCHAIN.imageExtent $ relevantCreateInfo swapchainBundle
      let uBuf = slotUniformBuffer cSlot
      updateUni uBuf (curTime * 0.016) windowExtent
      let rPass = swapchainRenderPass $ swapchainB swc
      let renderarea = Rect2D (Offset2D 0 0) windowExtent
      let clearTo = V.fromList [Color (Float32 1 0 0 1), DepthStencil (ClearDepthStencilValue 1.0 0)]
      let viewport = Viewport 0.0 0.0 (fromIntegral w) (fromIntegral h) 0.0 1.0

      beginCommandBuffer cBuf (CommandBufferBeginInfo () VZ.zero Nothing)
      cmdSetViewport cBuf 0 (V.singleton viewport)
      cmdBeginRenderPass cBuf (RenderPassBeginInfo () rPass frameBuf renderarea clearTo) VK.SUBPASS_CONTENTS_INLINE

      cmdBindPipeline cBuf VK.PIPELINE_BIND_POINT_GRAPHICS (swapchainPipeline swapchainBundle)
      cmdBindDescriptorSets cBuf VK.PIPELINE_BIND_POINT_GRAPHICS (swapchainPipeLayout swapchainBundle) 0 (V.singleton $ slotDescriptorSet cSlot) V.empty
      
      let (GeometryResource (VBuffer vBuf _) (Just (IxBuffer ixBuf _)) elements _) = vertGeo
      cmdBindVertexBuffers cBuf 0 (V.singleton vBuf) (V.singleton 0)
      cmdBindIndexBuffer cBuf ixBuf 0 INDEX_TYPE_UINT32
      cmdDrawIndexed cBuf (fromIntegral elements) 1 0 0 0

      cmdEndRenderPass cBuf
      endCommandBuffer cBuf


updateUni :: (InVulkanMonad effs) => UBuffer -> Float -> Extent2D -> Eff effs ()
updateUni ub elapsedTime (Extent2D width height) = do
  -- our shaders use premultiply so matrices need to be transposed
  let modelMatrix = transpose $ mkTransformation (axisAngle (V3 0 0 1) (elapsedTime*5)) (V3 (sin elapsedTime) 0 0.1)
  let viewMatrix = transpose $ lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 0 1)
  let scaleMatrix sx sy sz = V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1)
  let aspect = fromIntegral width / fromIntegral height
  let projMatrix = transpose $ zeroOnePerspective (45 * 3.14159 / 180.0) aspect 0.1 10 !*! scaleMatrix 1 (-1) 1
  let newUniforms = UBO modelMatrix viewMatrix projMatrix
  updateUniformBuffer ub newUniforms


