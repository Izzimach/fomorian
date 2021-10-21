{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.TransientResources (
  PipelineEtc(..),
  TransientResources(..),
  ImageFrameResources(..),
  withTransientResources,
  loadTransientResources,
  unloadTransientResources,
  buildSimplePipeline,
  destroyPipelineEtc,
  makeImageFrameResources,
  destroyImageFrameResources,
  updateUniformBuffer,
  makeDescriptorPool,
  unmakeDescriptorPool,
  makeDescriptorSetLayout,
  unmakeDescriptorSetLayout,
  makeDescriptorSets,
  unmakeDescriptorSets,
  syncDescriptorSets,
  recordCommandBuffers,
  DepthBuffer(..),
  ColorBuffer(..),
  makeDepthBuffer,
  unmakeDepthBuffer,
  makeColorBuffer,
  unmakeColorBuffer
  ) where

import Control.Exception

import Data.Bits
import Data.ByteString (readFile)
import Data.Coerce (coerce)
import Data.Foldable
import Data.Vector ((!), Vector, empty, fromList, findIndex)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word16, Word32)
import Data.Row ((.!))

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import Linear (V2 (..), V3 (..), V4(..), M44, identity, (!*!), mkTransformation, mkTransformationMat, Quaternion, axisAngle, lookAt, perspective, ortho, transpose)

import System.FilePath

import Vulkan.CStruct.Extends
import Vulkan.Core10 as VKCORE
    ( Sampler,
      ImageView,
      Image,
      DeviceMemory,
      DeviceSize,
      BufferCreateInfo(BufferCreateInfo),
      BufferUsageFlags,
      MemoryRequirements(memoryTypeBits),
      Device,
      Buffer,
      MemoryType(propertyFlags),
      PhysicalDeviceMemoryProperties(memoryTypes, memoryTypeCount),
      MemoryPropertyFlags,
      QUEUE_FAMILY_IGNORED,
      SUBPASS_EXTERNAL,
      createBuffer,
      destroyBuffer,
      allocateCommandBuffers,
      beginCommandBuffer,
      endCommandBuffer,
      freeCommandBuffers,
      cmdBeginRenderPass,
      cmdBindDescriptorSets,
      cmdBindIndexBuffer,
      cmdBindPipeline,
      cmdBindVertexBuffers,
      cmdBlitImage,
      cmdCopyBuffer,
      cmdCopyBufferToImage,
      cmdDrawIndexed,
      cmdEndRenderPass,
      cmdPipelineBarrier,
      allocateDescriptorSets,
      createDescriptorPool,
      createDescriptorSetLayout,
      destroyDescriptorPool,
      destroyDescriptorSetLayout,
      freeDescriptorSets,
      updateDescriptorSets,
      getPhysicalDeviceFormatProperties,
      getPhysicalDeviceMemoryProperties,
      getPhysicalDeviceProperties,
      createImage,
      destroyImage,
      createImageView,
      destroyImageView,
      allocateMemory,
      freeMemory,
      withMappedMemory,
      bindBufferMemory,
      bindImageMemory,
      getBufferMemoryRequirements,
      getImageMemoryRequirements,
      createRenderPass,
      destroyRenderPass,
      createGraphicsPipelines,
      destroyPipeline,
      createPipelineLayout,
      destroyPipelineLayout,
      queueSubmit,
      queueWaitIdle,
      createSampler,
      destroySampler,
      createShaderModule,
      destroyShaderModule,
      AllocationCallbacks,
      CommandBufferAllocateInfo(CommandBufferAllocateInfo),
      CommandBufferBeginInfo(CommandBufferBeginInfo),
      BufferCopy(BufferCopy),
      BufferImageCopy(BufferImageCopy),
      ClearColorValue(Float32),
      ClearDepthStencilValue(ClearDepthStencilValue),
      ClearValue(DepthStencil, Color),
      ImageBlit(ImageBlit),
      ImageSubresourceLayers(ImageSubresourceLayers),
      RenderPassBeginInfo(RenderPassBeginInfo),
      DescriptorBufferInfo(DescriptorBufferInfo),
      DescriptorImageInfo(DescriptorImageInfo),
      DescriptorPoolCreateInfo(DescriptorPoolCreateInfo),
      DescriptorPoolSize(DescriptorPoolSize),
      DescriptorSetAllocateInfo(DescriptorSetAllocateInfo),
      DescriptorSetLayoutBinding(DescriptorSetLayoutBinding),
      DescriptorSetLayoutCreateInfo(DescriptorSetLayoutCreateInfo),
      WriteDescriptorSet(WriteDescriptorSet),
      FormatProperties(optimalTilingFeatures, linearTilingFeatures),
      PhysicalDeviceLimits(maxSamplerAnisotropy),
      PhysicalDeviceProperties(limits),
      AccessFlagBits(ACCESS_SHADER_READ_BIT,
                     ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
                     ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT,
                     ACCESS_TRANSFER_READ_BIT, ACCESS_TRANSFER_WRITE_BIT),
      AttachmentLoadOp(ATTACHMENT_LOAD_OP_DONT_CARE,
                       ATTACHMENT_LOAD_OP_CLEAR),
      AttachmentStoreOp(ATTACHMENT_STORE_OP_DONT_CARE,
                        ATTACHMENT_STORE_OP_STORE),
      BlendFactor(BLEND_FACTOR_ZERO, BLEND_FACTOR_ONE),
      BlendOp(BLEND_OP_ADD),
      BorderColor(BORDER_COLOR_INT_OPAQUE_BLACK),
      BufferUsageFlagBits(BUFFER_USAGE_UNIFORM_BUFFER_BIT,
                          BUFFER_USAGE_TRANSFER_SRC_BIT, BUFFER_USAGE_VERTEX_BUFFER_BIT,
                          BUFFER_USAGE_TRANSFER_DST_BIT, BUFFER_USAGE_INDEX_BUFFER_BIT),
      ColorComponentFlagBits(COLOR_COMPONENT_A_BIT,
                             COLOR_COMPONENT_R_BIT, COLOR_COMPONENT_G_BIT,
                             COLOR_COMPONENT_B_BIT),
      CommandBufferLevel(COMMAND_BUFFER_LEVEL_PRIMARY),
      CommandBufferUsageFlagBits(COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT),
      CompareOp(COMPARE_OP_ALWAYS, COMPARE_OP_LESS),
      CullModeFlagBits(CULL_MODE_BACK_BIT),
      DescriptorType(DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                     DESCRIPTOR_TYPE_UNIFORM_BUFFER),
      DynamicState(DYNAMIC_STATE_LINE_WIDTH, DYNAMIC_STATE_VIEWPORT),
      Filter(FILTER_LINEAR),
      Format(FORMAT_D24_UNORM_S8_UINT, FORMAT_R32G32B32_SFLOAT,
             FORMAT_R32G32_SFLOAT, FORMAT_R8G8B8A8_SRGB, FORMAT_D32_SFLOAT,
             FORMAT_D32_SFLOAT_S8_UINT),
      FormatFeatureFlagBits(FormatFeatureFlagBits,
                            FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT,
                            FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT),
      FormatFeatureFlags,
      FrontFace(FRONT_FACE_COUNTER_CLOCKWISE),
      ImageAspectFlagBits(IMAGE_ASPECT_DEPTH_BIT,
                          IMAGE_ASPECT_COLOR_BIT),
      ImageAspectFlags,
      ImageLayout(IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                  IMAGE_LAYOUT_PRESENT_SRC_KHR,
                  IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                  IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                  IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, IMAGE_LAYOUT_UNDEFINED,
                  IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
      ImageTiling(IMAGE_TILING_LINEAR, IMAGE_TILING_OPTIMAL),
      ImageType(IMAGE_TYPE_2D),
      ImageUsageFlagBits(IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
                         IMAGE_USAGE_TRANSFER_SRC_BIT, IMAGE_USAGE_TRANSFER_DST_BIT,
                         IMAGE_USAGE_SAMPLED_BIT, IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT,
                         IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
      ImageUsageFlags,
      ImageViewType(IMAGE_VIEW_TYPE_2D),
      IndexType(INDEX_TYPE_UINT32),
      LogicOp(LOGIC_OP_COPY),
      MemoryPropertyFlagBits(MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
                             MEMORY_PROPERTY_HOST_VISIBLE_BIT,
                             MEMORY_PROPERTY_HOST_COHERENT_BIT),
      PipelineBindPoint(PIPELINE_BIND_POINT_GRAPHICS),
      PipelineStageFlagBits(PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
                            PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
                            PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT,
                            PIPELINE_STAGE_TOP_OF_PIPE_BIT, PIPELINE_STAGE_TRANSFER_BIT),
      PolygonMode(POLYGON_MODE_FILL),
      PrimitiveTopology(PRIMITIVE_TOPOLOGY_TRIANGLE_LIST),
      SampleCountFlagBits(SAMPLE_COUNT_1_BIT),
      SamplerAddressMode(SAMPLER_ADDRESS_MODE_REPEAT),
      SamplerMipmapMode(SAMPLER_MIPMAP_MODE_LINEAR),
      ShaderStageFlagBits(SHADER_STAGE_FRAGMENT_BIT,
                          SHADER_STAGE_VERTEX_BIT),
      SharingMode(SHARING_MODE_EXCLUSIVE),
      SubpassContents(SUBPASS_CONTENTS_INLINE),
      VertexInputRate(VERTEX_INPUT_RATE_VERTEX),
      Extent2D(Extent2D),
      Extent3D(Extent3D),
      Offset2D(Offset2D),
      Offset3D(Offset3D),
      Rect2D(Rect2D),
      CommandBuffer(commandBufferHandle),
      CommandPool,
      DescriptorPool,
      DescriptorSet,
      DescriptorSetLayout,
      Framebuffer,
      PhysicalDevice,
      Pipeline,
      PipelineLayout,
      Queue,
      RenderPass,
      ShaderModule,
      ImageCreateInfo(ImageCreateInfo),
      ImageSubresourceRange(ImageSubresourceRange),
      ImageViewCreateInfo(ImageViewCreateInfo),
      MemoryAllocateInfo(MemoryAllocateInfo),
      ImageMemoryBarrier(ImageMemoryBarrier),
      AttachmentDescription(AttachmentDescription),
      AttachmentReference(AttachmentReference),
      RenderPassCreateInfo(RenderPassCreateInfo),
      SubpassDependency(SubpassDependency),
      SubpassDescription(SubpassDescription),
      GraphicsPipelineCreateInfo(GraphicsPipelineCreateInfo),
      PipelineColorBlendAttachmentState(PipelineColorBlendAttachmentState),
      PipelineColorBlendStateCreateInfo(PipelineColorBlendStateCreateInfo),
      PipelineDepthStencilStateCreateInfo(PipelineDepthStencilStateCreateInfo),
      PipelineDynamicStateCreateInfo(PipelineDynamicStateCreateInfo),
      PipelineInputAssemblyStateCreateInfo(PipelineInputAssemblyStateCreateInfo),
      PipelineMultisampleStateCreateInfo(PipelineMultisampleStateCreateInfo),
      PipelineRasterizationStateCreateInfo(PipelineRasterizationStateCreateInfo),
      PipelineShaderStageCreateInfo(PipelineShaderStageCreateInfo),
      PipelineVertexInputStateCreateInfo(PipelineVertexInputStateCreateInfo),
      PipelineViewportStateCreateInfo(PipelineViewportStateCreateInfo),
      VertexInputAttributeDescription(VertexInputAttributeDescription),
      VertexInputBindingDescription(VertexInputBindingDescription),
      Viewport(Viewport),
      PipelineLayoutCreateInfo(PipelineLayoutCreateInfo),
      SubmitInfo(SubmitInfo),
      SamplerCreateInfo(SamplerCreateInfo),
      ShaderModuleCreateInfo(ShaderModuleCreateInfo) )
import Vulkan.Core10.MemoryManagement as VKMEM
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN
import Vulkan.Zero

import qualified Codec.Picture as JUICY
import qualified Codec.Picture.Types as JC

import qualified Fomorian.GraphicsLoaders.ProcessWavefront as WF

-- | Holds all resources that should then be unloaded later
data TransientResources = TransientResources
  { 
    vertexBuffer :: VBuffer,
    indexBuffer :: VBuffer,
    indexBufferSize :: Int,
    textureImage :: IMGBuffer
  }

-- | Resources that need to be allocated per imageframe, which may vary depending on the SwapChain.
--   Thus these need to be re-allocated when the swapchain is created or recreated
data ImageFrameResources = ImageFrameResources
  {
    uniforms :: UBuffer
  }
  deriving (Show)

withTransientResources :: Device -> PhysicalDevice -> CommandPool -> Queue -> Maybe AllocationCallbacks -> (IO TransientResources -> (TransientResources -> IO ()) -> r) -> r
withTransientResources d pd cPool gq allocator wrapper = wrapper rStart rEnd
  where
    rStart = (loadTransientResources d pd cPool gq allocator)
    rEnd = (\tRes -> unloadTransientResources d tRes allocator)

loadTransientResources :: Device -> PhysicalDevice -> CommandPool -> Queue -> Maybe AllocationCallbacks -> IO TransientResources
loadTransientResources d pd cPool gq allocator = do
  (vertexData, indexData) <- loadGeometry "viking_room.obj"
  v <- createLocalVertexBuffer d pd cPool gq vertexData allocator
  ix <- createLocalIndexBuffer d pd cPool gq indexData allocator
  img <- makeTextureImage d pd cPool gq ("resources" </> "textures" </> "viking_room.png") allocator
  return (TransientResources v ix (length indexData) img)

unloadTransientResources :: Device -> TransientResources -> Maybe AllocationCallbacks -> IO ()
unloadTransientResources d (TransientResources vb ix ixSize img) allocator = do
  destroyVertexBuffer d vb allocator
  destroyIndexBuffer d ix allocator
  unmakeTextureImage d img allocator





--
-- Pipeline creationg and handling
--


data PipelineEtc = PipelineEtc
  { pipelineInstance :: Pipeline,
    rendPass :: RenderPass,
    descriptorLayout :: DescriptorSetLayout,
    pipeLayout :: PipelineLayout,
    shaderModules :: Vector ShaderModule
  }
  deriving (Show)





-- | Read in SPIR-V shader files
readSPIRV :: Device -> FilePath -> IO (ShaderModule, ShaderModule)
readSPIRV dev shaderPath = do
  let vertPath = "resources" </> "shaders" </> (shaderPath ++ "vert.spv")
  vertBytes <- Data.ByteString.readFile vertPath
  vertModule <- createShaderModule dev (ShaderModuleCreateInfo () zero vertBytes) Nothing
  let fragPath = "resources" </> "shaders" </> (shaderPath ++ "frag.spv")
  fragBytes <- Data.ByteString.readFile fragPath
  fragModule <- createShaderModule dev (ShaderModuleCreateInfo () zero fragBytes) Nothing
  return (vertModule, fragModule)

-- | Put together a simple pipeline:
--   - Use shader "tut" which has "main" entry points
--   - triangle list
--   - front face culling
--   - one color attachment
--   - one simple renderpass
buildSimplePipeline :: Device -> PhysicalDevice -> SampleCountFlagBits -> Maybe AllocationCallbacks -> DescriptorSetLayout -> SwapchainCreateInfoKHR '[] -> IO PipelineEtc
buildSimplePipeline device phy sampleCount allocator descriptorLayout swapchainInfo = do
  (vm, fm) <- readSPIRV device "tut"
  let shaderStages =
        fromList
          [ SomeStruct $ PipelineShaderStageCreateInfo () zero SHADER_STAGE_VERTEX_BIT vm "main" Nothing,
            SomeStruct $ PipelineShaderStageCreateInfo () zero SHADER_STAGE_FRAGMENT_BIT fm "main" Nothing
          ]
  let vertexStageInfo =
        PipelineVertexInputStateCreateInfo
          ()
          zero
          (fromList [vertexDataBinding])
          (fromList vertexInputAttrs)
  let inputAssembly = PipelineInputAssemblyStateCreateInfo zero PRIMITIVE_TOPOLOGY_TRIANGLE_LIST False
  let windowExtent@(Extent2D w h) = VKSWAPCHAIN.imageExtent swapchainInfo
  let viewport = Viewport 0.0 0.0 (fromIntegral w) (fromIntegral h) 0.0 1.0
  let scissor = Rect2D (Offset2D 0 0) windowExtent
  let initViewportState = PipelineViewportStateCreateInfo () zero 1 (fromList [viewport]) 1 (fromList [scissor])
  -- use simple fill mode and culling
  let rasterizerState = PipelineRasterizationStateCreateInfo () zero False False POLYGON_MODE_FILL CULL_MODE_BACK_BIT FRONT_FACE_COUNTER_CLOCKWISE False 0 0 0 1.0
  -- multisample is basically disabled
  let initMultisampleState = PipelineMultisampleStateCreateInfo () zero sampleCount False 1.0 empty False False
  let blendAllBits = (COLOR_COMPONENT_R_BIT .|. COLOR_COMPONENT_G_BIT .|. COLOR_COMPONENT_B_BIT .|. COLOR_COMPONENT_A_BIT)
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
  let colorBlendCreate = PipelineColorBlendStateCreateInfo () zero False LOGIC_OP_COPY (fromList [initColorBlendState]) (0, 0, 0, 0)
  let dynamicStateCreate = PipelineDynamicStateCreateInfo zero (fromList [DYNAMIC_STATE_VIEWPORT, DYNAMIC_STATE_LINE_WIDTH])
  let pipelineLayoutCreate = PipelineLayoutCreateInfo zero (fromList [descriptorLayout]) empty
  pipelineLayout <- createPipelineLayout device pipelineLayoutCreate allocator
  let swapchainFormat = VKSWAPCHAIN.imageFormat swapchainInfo
  let colorAttachmentDescription =
        AttachmentDescription
          zero
          swapchainFormat
          sampleCount
          ATTACHMENT_LOAD_OP_CLEAR -- load
          ATTACHMENT_STORE_OP_STORE  -- store
          ATTACHMENT_LOAD_OP_DONT_CARE  -- stencil load
          ATTACHMENT_STORE_OP_DONT_CARE -- stencil store
          IMAGE_LAYOUT_UNDEFINED  -- initialLayout
          IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL  -- finalLayout
  let colorAttachmentResolve =
        AttachmentDescription
          zero
          swapchainFormat
          SAMPLE_COUNT_1_BIT
          ATTACHMENT_LOAD_OP_DONT_CARE  -- load
          ATTACHMENT_STORE_OP_DONT_CARE -- store
          ATTACHMENT_LOAD_OP_DONT_CARE  -- stencil load
          ATTACHMENT_STORE_OP_DONT_CARE -- stencil store
          IMAGE_LAYOUT_UNDEFINED        -- initialLayout
          IMAGE_LAYOUT_PRESENT_SRC_KHR  -- finalLayout
  depthFormat <- findDepthFormat phy
  let depthAttachmentDescription =
        AttachmentDescription
          zero
          depthFormat
          sampleCount
          ATTACHMENT_LOAD_OP_CLEAR  -- load
          ATTACHMENT_STORE_OP_DONT_CARE -- store
          ATTACHMENT_LOAD_OP_DONT_CARE  -- stencil load
          ATTACHMENT_STORE_OP_DONT_CARE -- stencil store
          IMAGE_LAYOUT_UNDEFINED
          IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  let colorAttachmentReference = AttachmentReference 0 IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  let depthAttachmentReference = AttachmentReference 1 IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  let colorAttachmentResolveRef = AttachmentReference 2 IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  let depthStencil =
        PipelineDepthStencilStateCreateInfo
          zero
          True   -- depthTestEnable
          True   -- depthWriteEnable
          COMPARE_OP_LESS -- depthCompareOp
          False           -- depthBoundsTestEnable
          False           -- stencilTestEnable
          zero            -- front (stencil)
          zero            -- back (stencil)
          0.0             -- minDepthBounds
          1.0             -- maxDepthBounds
  let subpassDescription =
        SubpassDescription
          zero
          PIPELINE_BIND_POINT_GRAPHICS
          empty -- inputAttachments
          (fromList [colorAttachmentReference]) -- colorAttachments
          (fromList [colorAttachmentResolveRef]) -- resolveAttachments
          (Just depthAttachmentReference) -- depthStencilAttachment
          empty -- preserveAttachments
  let subpassDependency =
        SubpassDependency
          SUBPASS_EXTERNAL -- srcSubpass
          0                -- dstSubpass
          (PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) -- srcStageMask
          (PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) -- dstStageMask
          zero                                       -- srcAccessMask
          (ACCESS_COLOR_ATTACHMENT_WRITE_BIT .|. ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT)        -- dstAccessMask
          zero                                       -- dependencyFlags
  let renderPassCreateInfo =
        RenderPassCreateInfo
          ()
          zero
          (fromList [colorAttachmentDescription, depthAttachmentDescription, colorAttachmentResolve])
          (fromList [subpassDescription])
          (fromList [subpassDependency])
  initRenderPass <- createRenderPass device renderPassCreateInfo allocator
  let pipelineCreateInfo =
        GraphicsPipelineCreateInfo
          () -- next
          zero -- flags
          shaderStages -- stages
          (Just (SomeStruct vertexStageInfo)) -- vertexInputState
          (Just inputAssembly) -- inputAssemblyState
          Nothing -- tessellationState
          (Just (SomeStruct initViewportState)) -- viewportState
          (SomeStruct rasterizerState) -- rasterizationState
          (Just (SomeStruct initMultisampleState)) -- multisampleState
          (Just depthStencil) -- depthStencilState
          (Just (SomeStruct colorBlendCreate)) -- colorBlendState
          Nothing -- dynamicState
          pipelineLayout -- layout
          initRenderPass -- renderPass
          0 -- subpass
          NULL_HANDLE -- basePipelineHandle
          (-1) -- basePipelineIndex
  (_, pipelines) <- createGraphicsPipelines device NULL_HANDLE (fromList [SomeStruct pipelineCreateInfo]) allocator
  let onepipe = pipelines ! 0
  return $ PipelineEtc onepipe initRenderPass descriptorLayout pipelineLayout (fromList [vm, fm])

-- | Clean up the pipeline - usually called via 'bracket'
destroyPipelineEtc :: Device -> PipelineEtc -> Maybe AllocationCallbacks -> IO ()
destroyPipelineEtc dev (PipelineEtc pipeline renderPassInstance descriptorLayoutInstance layoutInstance modules) allocator = do
  mapM_ (\m -> destroyShaderModule dev m Nothing) modules
  destroyPipelineLayout dev layoutInstance allocator
  destroyRenderPass dev renderPassInstance allocator
  destroyPipeline dev pipeline allocator








--
-- Vertex buffer handling
--


-- | Ref to a vertex buffer.
data VBuffer = VBuffer Buffer DeviceMemory
  deriving (Eq, Show)

-- | A temporary buffer to hold CPU-side data. Typically you create a CPU-accessible
--   staging buffer, copy data into it on the CPU, and then transfer the data to a
--   GPU-local buffer so the GPU can access it faster.
data StagingBuffer = StagingBuffer Buffer DeviceMemory



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

loadGeometry :: String -> IO ([OneVertex], [Word32])
loadGeometry fileName = do
  objData <- WF.loadWavefrontOBJFile ("resources" </> "geometry" </> fileName)
  case objData of
    Left s -> fail s
    Right (vertices, indices) -> do
      putStrLn (show (length vertices) ++ " vertices!")
      let vertices' = fmap toOneVertex vertices
      let indices' = fmap fromIntegral indices
      return (vertices', indices')
  where
    toOneVertex :: WF.OBJBufferRecord -> OneVertex
    toOneVertex (WF.OBJBufferRecord rec) =
      let pos3 = rec .! #pos3
          (V2 tu tv) = rec .! #texCoord
          tex2 = (V2 tu (1-tv))
          color3 = (V3 1 1 1)
      in OneVertex pos3 color3 tex2




newtype MemoryTypeMask = MemoryTypeMask Word32
  deriving (Eq, Show)

-- Look through the physical device memory types and find one that matches a bit in the MemoryTypeMAsk
-- and has all the flags requested (set) in MemoryProperyFlags
findMemoryType :: PhysicalDevice -> MemoryTypeMask -> MemoryPropertyFlags -> IO Word32
findMemoryType pd (MemoryTypeMask allowedTypeBits) memFlags = do
  memProps <- getPhysicalDeviceMemoryProperties pd
  let memTypeList = memoryTypes memProps
  let typeIndices = [0 .. (fromIntegral $ memoryTypeCount memProps - 1)]
  let isAllowedType = \i -> testBit allowedTypeBits i
  let bitsMatch b1 b2 = (b1 .&. b2) /= zeroBits
  let matchesProperties = \i -> bitsMatch
                                  (propertyFlags (memTypeList ! i))
                                  memFlags
  -- look through all the properties and find one where both type bits match and 
  let fullMatch = \i -> isAllowedType i && matchesProperties i
  case find fullMatch typeIndices of
    Nothing -> fail "No memory type found"
    Just ix -> return $ fromIntegral ix

allocBuffer :: Device -> PhysicalDevice -> DeviceSize -> BufferUsageFlags -> MemoryPropertyFlags -> Maybe AllocationCallbacks -> IO VBuffer
allocBuffer d pd bufSize usageFlags memoryProps allocator = do
  let bufInfo =
        BufferCreateInfo
          ()
          zero
          bufSize
          usageFlags
          SHARING_MODE_EXCLUSIVE
          empty
  buf <- createBuffer d bufInfo allocator
  req <- getBufferMemoryRequirements d buf
  memIndex <-
    findMemoryType
      pd
      (MemoryTypeMask (memoryTypeBits req))
      memoryProps
  let allocInfo = MemoryAllocateInfo () (VKMEM.size req) memIndex
  mem <- allocateMemory d allocInfo allocator
  bindBufferMemory d buf mem 0
  return (VBuffer buf mem)

-- | Creates a vertex buffer that is in CPU memory so you can map it and copy to it directly.
--   May not be as fast as GPU local memory; use 'createLocalVertexBuffer' to dump data into a GPU local buffer instead.
createMappedBuffer :: forall v. (Storable v) => Device -> PhysicalDevice -> [v] -> Maybe AllocationCallbacks -> IO VBuffer
createMappedBuffer d pd verts allocator = do
  let bufferSize = (fromIntegral ((length verts) * (sizeOf (head verts))))
  vBuf@(VBuffer _ mem) <-
    allocBuffer
      d
      pd
      bufferSize
      BUFFER_USAGE_VERTEX_BUFFER_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
      allocator
  withMappedMemory d mem 0 bufferSize zero bracket $ \ptr -> pokeArray (castPtr ptr) verts
  return vBuf

-- | Create a CPU-accessable buffer used just for copying over to the GPU.
createStagingBuffer :: forall v. (Storable v) => Device -> PhysicalDevice -> [v] -> Maybe AllocationCallbacks -> IO StagingBuffer
createStagingBuffer d pd verts allocator = do
  let bufferSize = (fromIntegral ((length verts) * (sizeOf (head verts))))
  (VBuffer buf mem) <-
    allocBuffer
      d
      pd
      bufferSize
      BUFFER_USAGE_TRANSFER_SRC_BIT
      (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
      allocator
  withMappedMemory d mem 0 bufferSize zero bracket $ \ptr -> pokeArray (castPtr ptr) verts
  return (StagingBuffer buf mem)

-- | Create a buffer in GPU local memory; writes the data to a CPU-side staging buffer and then copies it over to the GPU
--   using a command buffer copy.
createLocalVertexBuffer :: forall v. (Storable v) => Device -> PhysicalDevice -> CommandPool -> Queue -> [v] -> Maybe AllocationCallbacks -> IO VBuffer
createLocalVertexBuffer d pd cPool gq verts allocator = do
  let bufferSize = (fromIntegral ((length verts) * (sizeOf (head verts))))
  st <- createStagingBuffer d pd verts allocator
  vb <-
    allocBuffer
      d
      pd
      bufferSize
      (BUFFER_USAGE_TRANSFER_DST_BIT .|. BUFFER_USAGE_VERTEX_BUFFER_BIT)
      MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      allocator
  copyBuffer d cPool gq st vb bufferSize
  destroyStagingBuffer d st allocator
  return vb

createLocalIndexBuffer :: forall v. (Storable v) => Device -> PhysicalDevice -> CommandPool -> Queue -> [v] -> Maybe AllocationCallbacks -> IO VBuffer
createLocalIndexBuffer d pd cPool gq verts allocator = do
  let bufferSize = (fromIntegral ((length verts) * (sizeOf (head verts))))
  st <- createStagingBuffer d pd verts allocator
  vb <-
    allocBuffer
      d
      pd
      bufferSize
      (BUFFER_USAGE_TRANSFER_DST_BIT .|. BUFFER_USAGE_INDEX_BUFFER_BIT)
      MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      allocator
  copyBuffer d cPool gq st vb bufferSize
  destroyStagingBuffer d st allocator
  return vb

destroyVertexBuffer :: Device -> VBuffer -> Maybe AllocationCallbacks -> IO ()
destroyVertexBuffer d (VBuffer buf mem) allocator = do
  destroyBuffer d buf allocator
  freeMemory d mem allocator

destroyIndexBuffer :: Device -> VBuffer -> Maybe AllocationCallbacks -> IO ()
destroyIndexBuffer = destroyVertexBuffer

destroyStagingBuffer :: Device -> StagingBuffer -> Maybe AllocationCallbacks -> IO ()
destroyStagingBuffer d (StagingBuffer buf mem) allocator = do
  destroyBuffer d buf allocator
  freeMemory d mem allocator

-- | Run a copy command to transfer data from a staging buffer into a (possibly GPU local) buffer.
copyBuffer :: Device -> CommandPool -> Queue -> StagingBuffer -> VBuffer -> DeviceSize -> IO ()
copyBuffer d cp gq (StagingBuffer srcBuf _srcMem) (VBuffer dstBuf _dstMem) bufSize = do
  withOneTimeCommand d cp gq bracket $ \cBuf -> cmdCopyBuffer cBuf srcBuf dstBuf (fromList [BufferCopy 0 0 bufSize])

--
-- Descriptor sets
--

data UniformBufferObject = UBO {
  uboModel :: M44 Float,
  uboView  :: M44 Float,
  uboProjection :: M44 Float
  }
  deriving (Eq,Show)

data UBuffer = UBuffer Buffer DeviceMemory
  deriving (Eq, Show)

instance Storable UniformBufferObject where
  sizeOf _ = sizeOf @(M44 Float) undefined + sizeOf @(M44 Float) undefined + sizeOf @(M44 Float) undefined
  alignment _ = 16 -- maybe only needs to be 4?
  peek p = do
    -- treat as an array of @M44 Float@ objects
    let pMats = castPtr @UniformBufferObject @(M44 Float) p
    m <- peekElemOff pMats 0
    v <- peekElemOff pMats 1
    p <- peekElemOff pMats 2
    return (UBO m v p)
  poke p (UBO m v prj) = do
    let pMats = castPtr @UniformBufferObject @(M44 Float) p
    pokeElemOff pMats 0 m
    pokeElemOff pMats 1 v
    pokeElemOff pMats 2 prj

makeDescriptorSetLayout :: Device -> Maybe AllocationCallbacks -> IO DescriptorSetLayout
makeDescriptorSetLayout device allocator = do
  let dBinding = DescriptorSetLayoutBinding 0 DESCRIPTOR_TYPE_UNIFORM_BUFFER 1 SHADER_STAGE_VERTEX_BIT empty
  let dBinding2 = DescriptorSetLayoutBinding 1 DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1 SHADER_STAGE_FRAGMENT_BIT empty
  let createInfo = DescriptorSetLayoutCreateInfo () zero (fromList [dBinding, dBinding2])
  createDescriptorSetLayout device createInfo allocator

unmakeDescriptorSetLayout :: Device -> DescriptorSetLayout -> Maybe AllocationCallbacks -> IO ()
unmakeDescriptorSetLayout device descriptorSetLayoutInstance allocator = destroyDescriptorSetLayout device descriptorSetLayoutInstance allocator

makeImageFrameResources :: Device -> PhysicalDevice -> Vector Image -> Maybe AllocationCallbacks -> IO (Vector ImageFrameResources)
makeImageFrameResources device phy images allocator = mapM makeSingleFrameResource images
  where
    makeSingleFrameResource _ = do
      ub <- makeUniformBuffer device phy allocator
      return (ImageFrameResources ub)


destroyImageFrameResources :: Device -> Vector ImageFrameResources -> Maybe AllocationCallbacks -> IO ()
destroyImageFrameResources device fRes allocator = mapM_ destroySingleFrameResource fRes
  where
    destroySingleFrameResource (ImageFrameResources ub) = do
      destroyUniformBuffer device ub allocator
      return ()

makeUniformBuffer :: Device -> PhysicalDevice -> Maybe AllocationCallbacks -> IO UBuffer
makeUniformBuffer device phy allocator = do
  let bufferSize = sizeOf (undefined :: UniformBufferObject)
  (VBuffer b m) <- allocBuffer
                    device
                    phy
                    (fromIntegral bufferSize)
                    BUFFER_USAGE_UNIFORM_BUFFER_BIT
                    (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
                    allocator
  return (UBuffer b m)

destroyUniformBuffer :: Device -> UBuffer -> Maybe AllocationCallbacks -> IO ()
destroyUniformBuffer device (UBuffer buf mem) allocator = do
  destroyBuffer device buf allocator
  freeMemory device mem allocator

createUniformBuffers :: Device -> PhysicalDevice -> Int -> Maybe AllocationCallbacks -> IO (Vector UBuffer)
createUniformBuffers d pd frameCount allocator = traverse (\_ -> makeUniformBuffer d pd allocator) (fromList [0..(frameCount-1)])

destroyUniformBuffers :: Device -> Vector UBuffer -> Maybe AllocationCallbacks -> IO ()
destroyUniformBuffers d ubs allocator = do
  forM_ ubs (\u -> destroyUniformBuffer d u allocator)



-- | Perspective that projects z to the range [0,1] used by Vulkan.  We can't use 'Linear.Projection.perspective' since it projects z to [-1,1]
zeroOnePerspective :: Floating a =>  a -> a -> a -> a -> M44 a
zeroOnePerspective fov aspect near far =
  V4 (V4 x 0 0 0)
     (V4 0 y 0 0)
     (V4 0 0 z w)
     (V4 0 0 m 0)
  where
    tanHalfFov = tan $ fov / 2
    x = 1 / (aspect * tanHalfFov)
    y = 1 / tanHalfFov
    fpn = far + near
    fmn = far - near
    oon = 1 / near
    oof = 1 / far
    z = - far / fmn
    w = 1 / (oof-oon)
    m = (-1)

updateUniformBuffer :: Device -> UBuffer -> Float -> Extent2D -> IO ()
updateUniformBuffer device (UBuffer _ mem) elapsedTime (Extent2D width height) = do
  -- our shaders use premultiply so matrices need to be transposed
  let modelMatrix = transpose $ mkTransformation (axisAngle (V3 0 0 1) elapsedTime) (V3 (sin elapsedTime) 0 (0.1))
  let viewMatrix = transpose $ lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 0 1)
  let scaleMatrix sx sy sz = V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1)
  let aspect = (fromIntegral width) / (fromIntegral height)
  let projMatrix = transpose $ (zeroOnePerspective (45 * 3.14159 / 180.0) aspect 0.1 10) !*! (scaleMatrix 1 (-1) 1)
  let newUniforms = UBO modelMatrix viewMatrix projMatrix
  withMappedMemory device mem 0 (fromIntegral $ sizeOf newUniforms) zero bracket $ \ptr -> poke (castPtr ptr) newUniforms
  return ()


makeDescriptorPool :: Device -> Int -> Maybe AllocationCallbacks -> IO DescriptorPool
makeDescriptorPool device count allocator = do
  let uniformPoolSize = DescriptorPoolSize DESCRIPTOR_TYPE_UNIFORM_BUFFER (fromIntegral count)
  let imagePoolSize = DescriptorPoolSize DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER (fromIntegral count)
  let dCreateInfo = DescriptorPoolCreateInfo () zero (fromIntegral count) (fromList [uniformPoolSize, imagePoolSize])
  createDescriptorPool device dCreateInfo allocator

unmakeDescriptorPool :: Device -> DescriptorPool -> Maybe AllocationCallbacks -> IO ()
unmakeDescriptorPool device dPool allocator = destroyDescriptorPool device dPool allocator


makeDescriptorSets :: Device -> DescriptorPool -> DescriptorSetLayout -> Int -> IO (Vector DescriptorSet)
makeDescriptorSets device dPool dLayout count = do
  let layouts = replicate count dLayout
  let allocateInfo = DescriptorSetAllocateInfo () dPool (fromList layouts)
  allocateDescriptorSets device allocateInfo


-- | Descriptor set are automatically freed when their pool is destroyed, so this function is optional
unmakeDescriptorSets :: Device -> DescriptorPool -> (Vector DescriptorSet) -> IO ()
unmakeDescriptorSets device dPool dSets = freeDescriptorSets device dPool dSets

syncDescriptorSets :: Device -> Vector UBuffer -> IMGBuffer -> Vector DescriptorSet -> IO ()
syncDescriptorSets device dBufs (IMGBuffer _ _ _ iView iSampler) dSets = mapM_ sync1 $ zip (toList dBufs) (toList dSets)
  where
    sync1 ((UBuffer dBuf _), dSet) = do
      let bufferInfo = DescriptorBufferInfo dBuf 0 (fromIntegral $ sizeOf (undefined :: UniformBufferObject))
      let imageInfo = DescriptorImageInfo iSampler iView IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
      let writeDescriptor1 = SomeStruct $ WriteDescriptorSet () dSet 0 0 1 DESCRIPTOR_TYPE_UNIFORM_BUFFER          empty (fromList [bufferInfo]) empty
      let writeDescriptor2 = SomeStruct $ WriteDescriptorSet () dSet 1 0 1 DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER (fromList [imageInfo]) empty empty
      updateDescriptorSets device (fromList [writeDescriptor1, writeDescriptor2]) empty



--
-- images
--

data IMGBuffer = IMGBuffer Image Int DeviceMemory ImageView Sampler


partialLoadImage :: FilePath -> IO (Either String (JUICY.Image JUICY.PixelRGBA8, Int, Int))
partialLoadImage filePath = do
  dImage <- JUICY.readImage filePath
  case dImage of
    Left errorMessage -> return $ Left $ "Error loading file " ++ filePath ++ ": " ++ errorMessage  
    Right img -> do
      let w = JUICY.dynamicMap JUICY.imageWidth img
      let h = JUICY.dynamicMap JUICY.imageHeight img
      let stdImage = toRGBA8 img
      return $ Right $ (stdImage, w, h)
  where
    toRGBA8 :: JUICY.DynamicImage -> JUICY.Image JUICY.PixelRGBA8
    toRGBA8 (JC.ImageRGBA8 img) = JC.promoteImage img
    toRGBA8 (JC.ImageRGB8 img)  = JC.promoteImage img
    toRGBA8 _                   = undefined

makeImageParts :: Device -> PhysicalDevice -> Int -> Int -> Int -> SampleCountFlagBits -> Format -> ImageTiling -> ImageUsageFlags -> ImageAspectFlags -> MemoryPropertyFlags -> Maybe AllocationCallbacks -> IO (Image, DeviceMemory, ImageView)
makeImageParts device phy w h mipmaps numSamples imgFormat imgTiling imgUsage imgAspect memoryProps allocator = do
  let imgInfo = ImageCreateInfo ()
                  zero
                  IMAGE_TYPE_2D
                  imgFormat
                  (Extent3D (fromIntegral w) (fromIntegral h) 1)
                  (fromIntegral mipmaps) -- mipLevels
                  1 -- arrayLayers
                  numSamples -- sample
                  imgTiling --IMAGE_TILING_OPTIMAL
                  imgUsage --(IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
                  SHARING_MODE_EXCLUSIVE
                  (fromList [])
                  IMAGE_LAYOUT_UNDEFINED
  imageHandle <- createImage device imgInfo allocator
  memReq <- getImageMemoryRequirements device imageHandle
  memIndex <-
    findMemoryType
      phy
      (MemoryTypeMask (memoryTypeBits memReq))
      memoryProps
  let allocInfo = MemoryAllocateInfo () (VKMEM.size memReq) memIndex
  iMem <- allocateMemory device allocInfo allocator
  bindImageMemory device imageHandle iMem 0
  let viewInfo = ImageViewCreateInfo () zero imageHandle IMAGE_VIEW_TYPE_2D imgFormat zero (ImageSubresourceRange imgAspect 0 (fromIntegral mipmaps) 0 1)
  imgView <- createImageView device viewInfo allocator
  return (imageHandle, iMem, imgView)

makeIMGBuffer :: Device -> PhysicalDevice -> Int -> Int -> Int -> Format -> ImageTiling -> ImageUsageFlags -> MemoryPropertyFlags -> Maybe AllocationCallbacks -> IO IMGBuffer
makeIMGBuffer device phy w h mipLevels imgFormat imgTiling imgUsage memoryProps allocator = do
  (imageHandle, iMem, imgView) <- makeImageParts device phy w h mipLevels SAMPLE_COUNT_1_BIT imgFormat imgTiling imgUsage IMAGE_ASPECT_COLOR_BIT memoryProps allocator
  phyProps <- getPhysicalDeviceProperties phy
  let samplerInfo = SamplerCreateInfo () zero 
                      FILTER_LINEAR -- magFilter
                      FILTER_LINEAR -- minFilter
                      SAMPLER_MIPMAP_MODE_LINEAR -- mapmapMode
                      SAMPLER_ADDRESS_MODE_REPEAT -- addressModeU
                      SAMPLER_ADDRESS_MODE_REPEAT -- addressModeV
                      SAMPLER_ADDRESS_MODE_REPEAT -- addressModeW
                      0.0 -- mipLodBias
                      True -- anisotropyEnable
                      (maxSamplerAnisotropy $ limits phyProps)  -- maxAnisotropy
                      False -- compareEnable
                      COMPARE_OP_ALWAYS -- compareOp
                      0.0 -- minLod
                      (fromIntegral mipLevels) -- maxLod
                      BORDER_COLOR_INT_OPAQUE_BLACK -- borderColor
                      False -- unnormalizedCoordinates
  imgSampler <- createSampler device samplerInfo allocator
  return (IMGBuffer imageHandle mipLevels iMem imgView imgSampler)

unmakeIMGBuffer :: Device -> IMGBuffer -> Maybe AllocationCallbacks -> IO ()
unmakeIMGBuffer device (IMGBuffer imgHandle _mipmaps imgMem imgView imgSampler) allocator = do
  destroySampler device imgSampler allocator
  destroyImageView device imgView allocator
  destroyImage device imgHandle allocator
  freeMemory device imgMem allocator
  return ()


makeTextureImage :: Device -> PhysicalDevice -> CommandPool -> Queue -> FilePath -> Maybe AllocationCallbacks -> IO IMGBuffer
makeTextureImage device phy cPool gQueue filePath allocator = do
  imageResult <- partialLoadImage filePath
  case imageResult of
    Left errString -> fail errString
    Right (imgRGBA, w, h) -> do
      let imageByteSize = fromIntegral (w * h * 4)
      let mipLevels = 1 + (floor (log (fromIntegral $ max w h) / log 2.0))
      let imagePixels = (JC.imageData imgRGBA) :: VS.Vector Word8
      let imageBytes = VS.toList imagePixels
      staging@(StagingBuffer sBuf _) <- createStagingBuffer device phy imageBytes allocator
      imgBuffer@(IMGBuffer img _ _ _ _) <- makeIMGBuffer device phy w h mipLevels 
                                              FORMAT_R8G8B8A8_SRGB
                                              IMAGE_TILING_OPTIMAL 
                                              (IMAGE_USAGE_TRANSFER_SRC_BIT .|. IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT) 
                                              MEMORY_PROPERTY_DEVICE_LOCAL_BIT 
                                              allocator
      
      transitionImageLayout device cPool gQueue img mipLevels FORMAT_R8G8B8A8_SRGB IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      copyBufferToImage device cPool gQueue sBuf img w h
      generateMipmaps device phy cPool gQueue img FORMAT_R8G8B8A8_SRGB w h  mipLevels
      --transitionImageLayout device cPool gQueue img mipLevels FORMAT_R8G8B8A8_SRGB IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
      destroyStagingBuffer device staging allocator
      return imgBuffer

generateMipmaps :: Device -> PhysicalDevice -> CommandPool -> Queue -> Image -> Format -> Int -> Int -> Int -> IO ()
generateMipmaps device phy cPool gQueue img mipFormat w h mipLevels = do
  formatProps <- getPhysicalDeviceFormatProperties phy mipFormat
  if (optimalTilingFeatures formatProps .&. FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT) > zero
  then return ()
  else fail "Linear filtering not supported!"
  withOneTimeCommand device cPool gQueue bracket $ \cBuf -> do
    forM_ [1..(mipLevels-1)] $ \thisLevel -> do
      let baseLevel = thisLevel - 1
      let mipWidth = max 1 (w `div` (2 ^ baseLevel))
      let mipHeight = max 1 (h `div` (2 ^ baseLevel))
      let nextMipWidth = max 1 (mipWidth `div`2)
      let nextMipHeight = max 1 (mipHeight `div` 2)
      let barrier1 = SomeStruct $ ImageMemoryBarrier () 
                        ACCESS_TRANSFER_WRITE_BIT 
                        ACCESS_TRANSFER_READ_BIT 
                        IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                        IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                        QUEUE_FAMILY_IGNORED
                        QUEUE_FAMILY_IGNORED
                        img
                        (ImageSubresourceRange IMAGE_ASPECT_COLOR_BIT (fromIntegral baseLevel) 1 0 1)
      cmdPipelineBarrier cBuf PIPELINE_STAGE_TRANSFER_BIT PIPELINE_STAGE_TRANSFER_BIT zero empty empty (fromList [barrier1])                       
      let blit = ImageBlit
                   (ImageSubresourceLayers IMAGE_ASPECT_COLOR_BIT (fromIntegral baseLevel) 0 1)
                   ((Offset3D 0 0 0), (Offset3D (fromIntegral mipWidth) (fromIntegral mipHeight) 1))
                   (ImageSubresourceLayers IMAGE_ASPECT_COLOR_BIT (fromIntegral thisLevel) 0 1)
                   ((Offset3D 0 0 0), (Offset3D (fromIntegral nextMipWidth) (fromIntegral nextMipHeight) 1))
      cmdBlitImage cBuf img IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                        img IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                        (fromList [blit])
                        FILTER_LINEAR
      let barrier2 = SomeStruct $ ImageMemoryBarrier () 
                        ACCESS_TRANSFER_READ_BIT 
                        ACCESS_SHADER_READ_BIT 
                        IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                        IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                        QUEUE_FAMILY_IGNORED
                        QUEUE_FAMILY_IGNORED
                        img
                        (ImageSubresourceRange IMAGE_ASPECT_COLOR_BIT (fromIntegral baseLevel) 1 0 1)
      cmdPipelineBarrier cBuf PIPELINE_STAGE_TRANSFER_BIT PIPELINE_STAGE_FRAGMENT_SHADER_BIT zero empty empty (fromList [barrier2])
    -- outside the form_
    let barrier3 = SomeStruct $ ImageMemoryBarrier () 
                        ACCESS_TRANSFER_WRITE_BIT 
                        ACCESS_SHADER_READ_BIT 
                        IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                        IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                        QUEUE_FAMILY_IGNORED
                        QUEUE_FAMILY_IGNORED
                        img
                        (ImageSubresourceRange IMAGE_ASPECT_COLOR_BIT (fromIntegral (mipLevels-1)) 1 0 1)
    cmdPipelineBarrier cBuf PIPELINE_STAGE_TRANSFER_BIT PIPELINE_STAGE_FRAGMENT_SHADER_BIT zero empty empty (fromList [barrier3])  

unmakeTextureImage :: Device -> IMGBuffer -> Maybe AllocationCallbacks -> IO ()
unmakeTextureImage = unmakeIMGBuffer

transitionImageLayout :: Device -> CommandPool -> Queue -> Image -> Int -> Format -> ImageLayout -> ImageLayout -> IO ()
transitionImageLayout device cPool gQueue img mipLevels format oldLayout newLayout =
  withOneTimeCommand device cPool gQueue bracket $ \cBuf -> do
    let (srcMask, dstMask,srcStage,dstStage) = computeBarrierValues oldLayout newLayout
    let barrier = ImageMemoryBarrier () srcMask dstMask oldLayout newLayout QUEUE_FAMILY_IGNORED QUEUE_FAMILY_IGNORED img (ImageSubresourceRange IMAGE_ASPECT_COLOR_BIT 0 (fromIntegral mipLevels) 0 1)
    cmdPipelineBarrier cBuf srcStage dstStage zero empty empty (fromList [SomeStruct barrier])
    return ()
  where
    computeBarrierValues IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL                = (zero, ACCESS_TRANSFER_WRITE_BIT,
                                                                                                    PIPELINE_STAGE_TOP_OF_PIPE_BIT,
                                                                                                    PIPELINE_STAGE_TRANSFER_BIT)
    computeBarrierValues IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = (ACCESS_TRANSFER_WRITE_BIT,ACCESS_SHADER_READ_BIT,
                                                                                                    PIPELINE_STAGE_TRANSFER_BIT,
                                                                                                    PIPELINE_STAGE_FRAGMENT_SHADER_BIT)


copyBufferToImage :: Device -> CommandPool -> Queue -> Buffer -> Image -> Int -> Int -> IO ()
copyBufferToImage device cPool gQueue iBuf img width height =
  withOneTimeCommand device cPool gQueue bracket $ \cBuf -> do
    let copyInfo = BufferImageCopy 0 0 0 (ImageSubresourceLayers IMAGE_ASPECT_COLOR_BIT 0 0 1) (Offset3D 0 0 0) (Extent3D (fromIntegral width) (fromIntegral height) 1)
    cmdCopyBufferToImage cBuf iBuf img IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (fromList [copyInfo])


data ColorBuffer = ColorBuffer Image DeviceMemory ImageView
  deriving Show

makeColorBuffer :: Device -> PhysicalDevice -> Int -> Int -> SampleCountFlagBits -> Format -> Maybe AllocationCallbacks -> IO ColorBuffer
makeColorBuffer device phy w h numSamples colorFormat allocator = do
  (img, iMem, imgView) <- makeImageParts device phy w h 1 numSamples colorFormat
                              IMAGE_TILING_OPTIMAL
                              (IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT .|. IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
                              IMAGE_ASPECT_COLOR_BIT
                              MEMORY_PROPERTY_DEVICE_LOCAL_BIT
                              allocator
  return (ColorBuffer img iMem imgView)


unmakeColorBuffer :: Device -> ColorBuffer -> Maybe AllocationCallbacks -> IO ()
unmakeColorBuffer device (ColorBuffer cBuf cMem cView) allocator = do
  destroyImageView device cView allocator
  destroyImage device cBuf allocator
  freeMemory device cMem allocator


data DepthBuffer = DepthBuffer Image DeviceMemory ImageView
  deriving Show

makeDepthBuffer :: Device -> PhysicalDevice -> Int -> Int -> SampleCountFlagBits -> Maybe AllocationCallbacks -> IO DepthBuffer
makeDepthBuffer device phy w h numSamples allocator = do
  depthFormat <- findDepthFormat phy
  (img, iMem, imgView) <- makeImageParts device phy w h 1 numSamples depthFormat
                              IMAGE_TILING_OPTIMAL
                              IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
                              IMAGE_ASPECT_DEPTH_BIT
                              MEMORY_PROPERTY_DEVICE_LOCAL_BIT
                              allocator
  return (DepthBuffer img iMem imgView)

unmakeDepthBuffer :: Device -> DepthBuffer -> Maybe AllocationCallbacks -> IO ()
unmakeDepthBuffer device (DepthBuffer img imgMem imgView) allocator = do
  destroyImageView device imgView allocator
  destroyImage device img allocator
  freeMemory device imgMem allocator

findDepthFormat :: PhysicalDevice -> IO Format
findDepthFormat phy =
  findSupportedFormat
    phy
    (fromList [FORMAT_D32_SFLOAT, FORMAT_D32_SFLOAT_S8_UINT, FORMAT_D24_UNORM_S8_UINT])
    IMAGE_TILING_OPTIMAL
    FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
  
hasStencilComponent :: Format -> Bool
hasStencilComponent f = (f == FORMAT_D32_SFLOAT_S8_UINT) || (f == FORMAT_D24_UNORM_S8_UINT)

findSupportedFormat :: PhysicalDevice -> Vector Format -> ImageTiling -> FormatFeatureFlags -> IO Format
findSupportedFormat phy possibleFormats tiling chooseFeatures = do
  vecFeatures <- mapM (relevantFeatures ) possibleFormats
  case findIndex (hasFeatures) vecFeatures of
    Nothing -> fail "No allowed format"
    Just foundIndex -> return (possibleFormats ! foundIndex)
  where
    relevantFeatures fmt = do
      props <- getPhysicalDeviceFormatProperties phy fmt
      case tiling of
                                IMAGE_TILING_OPTIMAL -> return $ optimalTilingFeatures props
                                IMAGE_TILING_LINEAR -> return $ linearTilingFeatures props
                                _ -> return zero
    hasFeatures features = (features .&. chooseFeatures) > (FormatFeatureFlagBits zero)

--
-- command buffers
--



-- | Record the same set of commands (pre-defined in 'recordCommands') to all the commandbuffers passed
--   in. Needs the framebuffers associated with each command buffer.
recordCommandBuffers :: Vector CommandBuffer -> Vector Framebuffer -> TransientResources -> SwapchainCreateInfoKHR '[] -> PipelineEtc -> Vector DescriptorSet -> IO ()
recordCommandBuffers cbs fbs tRes sci pipeETC dSets = do
  let buffers = zip3 (toList cbs) (toList fbs) (toList dSets)
  let goBuffers (cb, fb, dSet) = recordCommands cb tRes sci (rendPass pipeETC) (pipelineInstance pipeETC) fb (pipeLayout pipeETC) dSet
  mapM_ goBuffers buffers
  return ()

-- | Simple recording of a command buffer to draw using the given renderpass, pipeline, and framebuffer.
recordCommands :: CommandBuffer -> TransientResources -> SwapchainCreateInfoKHR '[] -> RenderPass -> Pipeline -> Framebuffer -> PipelineLayout -> DescriptorSet -> IO ()
recordCommands buf tRes sce pass pipe fb pLayout dSet = do
  beginCommandBuffer buf (CommandBufferBeginInfo () zero Nothing)
  let renderarea = Rect2D (Offset2D 0 0) (VKSWAPCHAIN.imageExtent $ sce)
  let clearTo = fromList [Color (Float32 0 0 0 1), DepthStencil (ClearDepthStencilValue 1.0 0)]
  cmdBeginRenderPass buf (RenderPassBeginInfo () pass fb renderarea clearTo) SUBPASS_CONTENTS_INLINE
  cmdBindPipeline buf PIPELINE_BIND_POINT_GRAPHICS pipe
  let (VBuffer vBuf _) = vertexBuffer tRes
  let vBufs = [vBuf]
  let offsets = [0]
  cmdBindVertexBuffers buf 0 (fromList vBufs) (fromList offsets)
  let (VBuffer ixBuf _) = indexBuffer tRes
  cmdBindIndexBuffer buf ixBuf 0 INDEX_TYPE_UINT32
  cmdBindDescriptorSets buf PIPELINE_BIND_POINT_GRAPHICS pLayout 0 (fromList [dSet]) empty
  cmdDrawIndexed buf (fromIntegral $ indexBufferSize tRes) 1 0 0 0
  cmdEndRenderPass buf
  endCommandBuffer buf



withOneTimeCommand :: Device -> CommandPool -> Queue -> (IO CommandBuffer -> (CommandBuffer -> IO ()) -> (CommandBuffer -> IO ()) -> IO ()) -> (CommandBuffer -> IO ()) -> IO ()
withOneTimeCommand device cPool gQueue wrapper wrapped = wrapper startCBuf endCBuf goCBuf
  where
    startCBuf = do
      let allocInfo = CommandBufferAllocateInfo cPool COMMAND_BUFFER_LEVEL_PRIMARY 1
      cBufs <- allocateCommandBuffers device allocInfo
      let cBuf = cBufs ! 0
      let beginInfo = CommandBufferBeginInfo () COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT Nothing
      beginCommandBuffer cBuf beginInfo
      return cBuf
    endCBuf cBuf = do
      endCommandBuffer cBuf
      let submitInfo = SomeStruct $ SubmitInfo () empty empty (fromList [commandBufferHandle cBuf]) empty
      queueSubmit gQueue (fromList [submitInfo]) NULL_HANDLE
      queueWaitIdle gQueue
      freeCommandBuffers device cPool (fromList [cBuf])
    goCBuf = wrapped