{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.Resources.Pipeline where

import Control.Monad.Freer

import Data.Bits
import qualified Data.Vector as V
import Data.ByteString (readFile)

import System.FilePath

import Linear

import Vulkan.Core10 as VK
import Vulkan.Core10.PipelineLayout as VKPL
import Vulkan.CStruct.Extends (SomeStruct(SomeStruct))
import qualified Vulkan.Zero as VZ


import Fomorian.Vulkan.VulkanMonads
import Foreign.Storable (Storable (peekByteOff, pokeByteOff), sizeOf, poke, peek, alignment)
import Foreign.Ptr (castPtr)

-- | Make a render pass that draws to a single color and depth attachment. You need to pass in the formats for the color and depth buffer.
makeSimpleRenderPass :: (InVulkanMonad effs) => Format -> Format -> Eff effs VK.RenderPass
makeSimpleRenderPass colorFormat depthFormat = do
  let colorAttachmentDescription =
        VK.AttachmentDescription
          VZ.zero
          colorFormat
          SAMPLE_COUNT_1_BIT
          ATTACHMENT_LOAD_OP_CLEAR -- load
          ATTACHMENT_STORE_OP_STORE  -- store
          ATTACHMENT_LOAD_OP_DONT_CARE  -- stencil load
          ATTACHMENT_STORE_OP_DONT_CARE -- stencil store
          IMAGE_LAYOUT_UNDEFINED  -- initialLayout
          IMAGE_LAYOUT_PRESENT_SRC_KHR  -- finalLayout
  let depthAttachmentDescription =
        VK.AttachmentDescription
          VZ.zero
          depthFormat
          SAMPLE_COUNT_1_BIT
          ATTACHMENT_LOAD_OP_CLEAR  -- load
          ATTACHMENT_STORE_OP_DONT_CARE -- store
          ATTACHMENT_LOAD_OP_DONT_CARE  -- stencil load
          ATTACHMENT_STORE_OP_DONT_CARE -- stencil store
          IMAGE_LAYOUT_UNDEFINED
          IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  let colorAttachmentReference = AttachmentReference 0 IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  let depthAttachmentReference = AttachmentReference 1 IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  let subpassDescription =
        SubpassDescription
          VZ.zero
          PIPELINE_BIND_POINT_GRAPHICS
          V.empty -- inputAttachments
          (V.fromList [colorAttachmentReference]) -- colorAttachments
          V.empty -- resolveAttachments
          (Just depthAttachmentReference) -- depthStencilAttachment
          V.empty -- preserveAttachments
  let subpassDependency =
        SubpassDependency
          SUBPASS_EXTERNAL -- srcSubpass
          0                -- dstSubpass
          (PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) -- srcStageMask
          (PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) -- dstStageMask
          VZ.zero                                       -- srcAccessMask
          (ACCESS_COLOR_ATTACHMENT_WRITE_BIT .|. ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT)        -- dstAccessMask
          VZ.zero                                       -- dependencyFlags
  let renderPassCreateInfo =
        RenderPassCreateInfo
          ()
          VZ.zero
          (V.fromList [colorAttachmentDescription, depthAttachmentDescription])
          (V.fromList [subpassDescription])
          (V.fromList [subpassDependency])
  d <- getDevice
  createRenderPass d renderPassCreateInfo Nothing


buildSimplePipeline :: (InVulkanMonad effs) => (ShaderModule, ShaderModule) -> RenderPass -> PipelineLayout -> Extent2D -> Eff effs Pipeline
buildSimplePipeline (vm,fm) rPass pipelineLayout windowExtent@(Extent2D w h) = do
  let sampleCount = SAMPLE_COUNT_1_BIT
  let allocator = Nothing
  device <- getDevice
  let shaderStages =
        V.fromList
          [ SomeStruct $ PipelineShaderStageCreateInfo () VZ.zero SHADER_STAGE_VERTEX_BIT vm "main" Nothing,
            SomeStruct $ PipelineShaderStageCreateInfo () VZ.zero SHADER_STAGE_FRAGMENT_BIT fm "main" Nothing
          ]
  let vertexStageInfo =
        PipelineVertexInputStateCreateInfo
          ()
          VZ.zero
          (V.fromList [vertexDataBinding])
          (V.fromList vertexInputAttrs)
  let inputAssembly = PipelineInputAssemblyStateCreateInfo VZ.zero PRIMITIVE_TOPOLOGY_TRIANGLE_LIST False
  let rasterizerState = PipelineRasterizationStateCreateInfo () VZ.zero False False POLYGON_MODE_FILL CULL_MODE_BACK_BIT FRONT_FACE_COUNTER_CLOCKWISE False 0 0 0 1.0
  -- multisample is basically disabled
  let initMultisampleState = PipelineMultisampleStateCreateInfo () VZ.zero sampleCount False 1.0 V.empty False False
  let viewport = Viewport 0.0 0.0 (fromIntegral w) (fromIntegral h) 0.0 1.0
  let scissor = Rect2D (Offset2D 0 0) windowExtent
  let initViewportState = PipelineViewportStateCreateInfo () VZ.zero 1 (V.singleton viewport) 1 (V.singleton scissor)
  let blendAllBits = COLOR_COMPONENT_R_BIT .|. COLOR_COMPONENT_G_BIT .|. COLOR_COMPONENT_B_BIT .|. COLOR_COMPONENT_A_BIT
  let initColorBlendState =
        PipelineColorBlendAttachmentState
          False
          BLEND_FACTOR_ONE
          BLEND_FACTOR_ZERO
          BLEND_OP_ADD
          BLEND_FACTOR_ONE
          BLEND_FACTOR_ZERO
          BLEND_OP_ADD
          blendAllBits
  let colorBlendCreate = PipelineColorBlendStateCreateInfo () VZ.zero False LOGIC_OP_COPY (V.fromList [initColorBlendState]) (0, 0, 0, 0)
  let depthStencil =
        VK.PipelineDepthStencilStateCreateInfo
        VZ.zero
        True   -- depthTestEnable
        True   -- depthWriteEnable
        COMPARE_OP_LESS -- depthCompareOp
        False           -- depthBoundsTestEnable
        False           -- stencilTestEnable
        VZ.zero            -- front (stencil)
        VZ.zero            -- back (stencil)
        0.0             -- minDepthBounds
        1.0             -- maxDepthBounds
  let dynamicStateCreate = PipelineDynamicStateCreateInfo VZ.zero (V.fromList [DYNAMIC_STATE_VIEWPORT])
  --let pipelineLayoutCreate = VZ.zero { VKPL.setLayouts = V.fromList dSets }
  --pipelineLayout <- createPipelineLayout device pipelineLayoutCreate allocator
  let pipelineCreateInfo =
        GraphicsPipelineCreateInfo
          () -- next
          VZ.zero -- flags
          shaderStages -- stages
          (Just (SomeStruct vertexStageInfo)) -- vertexInputState
          (Just inputAssembly) -- inputAssemblyState
          Nothing -- tessellationState
          (Just (SomeStruct initViewportState)) -- viewportState
          (SomeStruct rasterizerState) -- rasterizationState
          (Just (SomeStruct initMultisampleState)) -- multisampleState
          (Just depthStencil) -- depthStencilState
          (Just (SomeStruct colorBlendCreate)) -- colorBlendState
          (Just dynamicStateCreate) -- dynamicState
          pipelineLayout -- layout
          rPass -- renderPass
          0 -- subpass
          NULL_HANDLE -- basePipelineHandle
          (-1) -- basePipelineIndex
  (_, pipelines) <- createGraphicsPipelines device NULL_HANDLE (V.fromList [SomeStruct pipelineCreateInfo]) allocator
  let onepipe = pipelines V.! 0
  return onepipe

destroySimplePipeline :: (InVulkanMonad effs) => Pipeline -> Eff effs ()
destroySimplePipeline pipeline = do
  let allocator = Nothing
  d <- getDevice
  VK.destroyPipeline d pipeline allocator
  return ()

-- hard coded vertex data for testing

-- | Position, Color, texCoord
data OneVertex = OneVertex (V3 Float) (V3 Float) (V2 Float)

blankVertex :: OneVertex
blankVertex = OneVertex (V3 0 0 0) (V3 0 0 0) (V2 0 0)

instance Storable OneVertex where
  sizeOf (OneVertex a1 a2 a3) = sizeOf a1 + sizeOf a2 + sizeOf a3
  alignment _ = 4
  peek p = do
    a1 <- peek (castPtr @OneVertex @(V3 Float) p)
    a2 <- peekByteOff p 12
    a3 <- peekByteOff p 24
    return (OneVertex a1 a2 a3)
  poke p (OneVertex a1 a2 a3) = do
    poke (castPtr @OneVertex @(V3 Float) p) a1
    pokeByteOff p 12 a2
    pokeByteOff p 24 a3

vertexDataBinding :: VertexInputBindingDescription
vertexDataBinding = VertexInputBindingDescription 0 (fromIntegral $ sizeOf blankVertex) VERTEX_INPUT_RATE_VERTEX

vertexInputAttrs :: [VertexInputAttributeDescription]
vertexInputAttrs =
  [ (VertexInputAttributeDescription 0 0 FORMAT_R32G32B32_SFLOAT 0),
    (VertexInputAttributeDescription 1 0 FORMAT_R32G32B32_SFLOAT 12),
    (VertexInputAttributeDescription 2 0 FORMAT_R32G32_SFLOAT 24)
  ]

-- Vertex data for drawing
{-- vertexData :: [OneVertex]
vertexData =
  [ 
    (OneVertex (V3 (-0.5) (-0.5) 0.0) (V3 1.0 0.0 0.0) (V2 1.0 0.0)),
    (OneVertex (V3 0.5    (-0.5) 0.0) (V3 0.0 1.0 0.0) (V2 0.0 0.0)),
    (OneVertex (V3 0.5    0.5    0.0) (V3 1.0 1.0 1.0) (V2 0.0 1.0)),
    (OneVertex (V3 (-0.5) (0.5)  0.0) (V3 0.0 0.0 1.0) (V2 1.0 1.0)),

    (OneVertex (V3 (-0.5) (-0.5) (-0.5)) (V3 1.0 0.0 0.0) (V2 1.0 0.0)),
    (OneVertex (V3 0.5    (-0.5) (-0.5)) (V3 0.0 1.0 0.0) (V2 0.0 0.0)),
    (OneVertex (V3 0.5    0.5    (-0.5)) (V3 1.0 1.0 1.0) (V2 0.0 1.0)),
    (OneVertex (V3 (-0.5) (0.5)  (-0.5)) (V3 0.0 0.0 1.0) (V2 1.0 1.0))
  ]

indexData :: [Word32]
indexData = [0, 1, 2, 2, 3, 0,
             4, 5, 6, 6, 7, 4]
--}

