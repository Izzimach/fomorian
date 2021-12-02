{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Fomorian.Vulkan.Example where

import Control.Exception
import Control.Monad.Freer

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Row
import Data.HList

import Linear

import Foreign.Storable (sizeOf, alignment)

import Text.Pretty.Simple (pPrint)
import System.FilePath ((</>))

import STMLoader.LoadUnload
import STMLoader.AsyncLoader

import Vulkan.CStruct.Extends
import Vulkan.Core10 (Extent2D(..), ClearValue(Color, DepthStencil), Offset2D(..), Rect2D(..), ClearColorValue(Float32), ClearDepthStencilValue(..))
import qualified Vulkan.Core10 as VK
import Vulkan.Zero as VZ
import Vulkan.Exception

import Fomorian.SceneResources
import qualified Fomorian.Vulkan.WindowBundle as WB

import Fomorian.Vulkan.VulkanMonads
import Fomorian.Vulkan.SwapchainCarousel
import Fomorian.Vulkan.SwapchainBundle
import Fomorian.Vulkan.Resources.Pipeline
import Fomorian.Vulkan.Resources.ImageBuffers
import Fomorian.Vulkan.Resources.DescriptorSets
import Fomorian.Vulkan.Resources.DescriptorSetHelper
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
    putStrLn $ "buffer-image granularity = " ++ show (VK.bufferImageGranularity deviceLimits)
    -- fork off a queue submit thread using the aux queue
    let aQueue = head $ WB.auxiliaryQueues $ WB.vulkanDeviceBundle wb
    let prebuiltResources = M.empty
    boundQueue <- aQueue `seq` forkBoundSubmitter wb aQueue
    loaderInfo <- startLoader wb boundQueue prebuiltResources

    let runV = runM . runVulkanMonad wb

    finally 
      (runV $ withCarousel 2 $ \swc -> forM_ [1..300] (\x -> presentNextSlot swc (clearCmd swc loaderInfo x)))
      (do
         endLoader loaderInfo
         endBoundSubmitter boundQueue
         )
  where
    clearCmd swc resourceLoader curTime cBuf frameBuf cSlot = do
      sb <- sendM $ readIORef (bundleRef swc)
      let (SwapchainPresentInfo cFormat dFormat ext2d@(Extent2D w h)) = swapchainPresentInfo sb
      let renderarea = Rect2D (Offset2D 0 0) ext2d
      let clearTo = V.fromList [Color (Float32 1 0 0 1), DepthStencil (ClearDepthStencilValue 1.0 0)]
      let viewport = VK.Viewport 0.0 0.0 (fromIntegral w) (fromIntegral h) 0.0 1.0
      let scissor = VK.Rect2D (Offset2D 0 0) ext2d

      let basicVertSource = DataSource $ IsJust #wavefrontPath "testcube.obj"
          basicImageSource = DataSource $ IsJust #texturePath "sad-crab.png"
          basicDescriptorInfo = DescriptorSetInfo [
              UniformDescriptor 0 VK.SHADER_STAGE_VERTEX_BIT (fromIntegral $ sizeOf @HelperExample undefined) (fromIntegral $ Foreign.Storable.alignment @HelperExample undefined),
              CombinedDescriptor 1 VK.SHADER_STAGE_FRAGMENT_BIT 1 V.empty
            ]
          basicDescriptorSource = (DataSource $ IsJust #descriptorHelperSettings basicDescriptorInfo :: VulkanDataSource)
          basicRenderpassFormat = (cFormat,dFormat)
          basicRenderpassSource = DataSource $ IsJust #renderPassFormat basicRenderpassFormat
          basicPipelineSource = DataSource $ IsJust #pipelineSettings $ SimplePipelineSettings {
              renderPassFormat = basicRenderpassFormat,
              shaderSource = "tut",
              descriptorSetLayouts = [basicDescriptorInfo]
            }
      -- wait until loader loads our stuff
      resources <- sendM $ waitForResourceProcessing resourceLoader (S.empty, S.fromList [basicVertSource, basicImageSource, basicRenderpassSource, basicDescriptorSource, basicPipelineSource])
      let (Just vertices)       = pullResource resources basicVertSource #vkGeometry
          (Just imagez)         = pullResource resources basicImageSource #textureImage
          (Just dData)          = pullResource resources basicDescriptorSource #descriptorSetHelperSource
          (Just rPass)          = pullResource resources basicRenderpassSource #renderPass
          (Just pBundle)        = (pullResource resources basicPipelineSource #simplePipeline :: Maybe PipelineBundle)
          (PipelineBundle pLayout pipe) = pBundle

      curDSets <- useDescriptorSetHelperSource dData (slotIndex cSlot) $ do
        resetDescriptorSetHelper
        forM [1,2,3] $ \ix -> do
          dSetBundle <- nextDescriptorSetBundle
          let (ImageBuffer _ _ _ iv samp) = imagez
          let uBuf = computeUniforms (curTime * 0.016 + fromIntegral ix) ext2d
          writeToHelperBundle dSetBundle $ uBuf `HCons` (iv,samp) `HCons` HNil
          return $ dSetHandle dSetBundle

      VK.beginCommandBuffer cBuf (VK.CommandBufferBeginInfo () VZ.zero Nothing)
      VK.cmdSetViewport cBuf 0 (V.singleton viewport)
      VK.cmdSetScissor cBuf 0 (V.singleton scissor)
      
      VK.cmdBeginRenderPass cBuf (VK.RenderPassBeginInfo () rPass frameBuf renderarea clearTo) VK.SUBPASS_CONTENTS_INLINE

      VK.cmdBindPipeline cBuf VK.PIPELINE_BIND_POINT_GRAPHICS pipe --(swapchainPipeline swapchainBundle)
      
      let (GeometryResource (VBuffer vBuf _) (Just (IxBuffer ixBuf _)) elements _) = vertices
      VK.cmdBindVertexBuffers cBuf 0 (V.singleton vBuf) (V.singleton 0)
      VK.cmdBindIndexBuffer cBuf ixBuf 0 VK.INDEX_TYPE_UINT32

      mapM_ (\curDSet -> do
        VK.cmdBindDescriptorSets cBuf VK.PIPELINE_BIND_POINT_GRAPHICS pLayout 0 (V.singleton curDSet) V.empty
        VK.cmdDrawIndexed cBuf (fromIntegral elements) 1 0 0 0
        )
        curDSets

      VK.cmdEndRenderPass cBuf
      VK.endCommandBuffer cBuf

whatResourcesForScene :: (InVulkanMonad effs) => SwapchainPresentInfo -> Eff effs (S.Set VulkanDataSource)
whatResourcesForScene = undefined


computeUniforms :: Float -> Extent2D -> HelperExample
computeUniforms elapsedTime (Extent2D width height) = 
  -- our shaders use premultiply so matrices need to be transposed
  let modelMatrix = transpose $ mkTransformation (axisAngle (V3 0 0 1) (elapsedTime*5)) (V3 (sin elapsedTime) 0 0.1)
      viewMatrix = transpose $ lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 0 1)
      scaleMatrix sx sy sz = V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1)
      aspect = fromIntegral width / fromIntegral height
      projMatrix = transpose $ zeroOnePerspective (45 * 3.14159 / 180.0) aspect 0.1 10 !*! scaleMatrix 1 (-1) 1
  in
    HelperExample modelMatrix viewMatrix projMatrix


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