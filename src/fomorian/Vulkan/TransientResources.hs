{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.TransientResources (
  PipelineEtc(..),
  TransientResources(..),
  ImageFrameResources(..),
  withTransientResources,
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
  recordCommandBuffers
  ) where

import Control.Exception
import Data.Bits
import Data.ByteString (readFile)
import Data.Foldable
import Data.Vector ((!), Vector, empty, fromList)
import Data.Word (Word16, Word32)
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Linear (V2 (..), V3 (..), V4(..), M44, identity, (!*!), mkTransformation, mkTransformationMat, Quaternion, axisAngle, lookAt, perspective, ortho, transpose)
import System.FilePath
import Vulkan.CStruct.Extends
import Vulkan.Core10 as VKCORE
import Vulkan.Core10.MemoryManagement as VKMEM
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN
import Vulkan.Zero

-- | Holds all resources that should then be unloaded later
data TransientResources = TransientResources
  { 
    vertexBuffer :: VBuffer,
    indexBuffer :: VBuffer
  }
  deriving (Show)

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
  v <- createLocalVertexBuffer d pd cPool gq vertexData allocator
  ix <- createLocalIndexBuffer d pd cPool gq indexData allocator
  return (TransientResources v ix)

unloadTransientResources :: Device -> TransientResources -> Maybe AllocationCallbacks -> IO ()
unloadTransientResources d (TransientResources vb ix) allocator = do
  destroyVertexBuffer d vb allocator
  destroyIndexBuffer d ix allocator





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
buildSimplePipeline :: Device -> Maybe AllocationCallbacks -> DescriptorSetLayout -> SwapchainCreateInfoKHR '[] -> IO PipelineEtc
buildSimplePipeline device allocator descriptorLayout swapchainInfo = do
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
  let initMultisampleState = PipelineMultisampleStateCreateInfo () zero SAMPLE_COUNT_1_BIT False 1.0 empty False False
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
  pipelineLayout <- createPipelineLayout device pipelineLayoutCreate Nothing
  let swapchainFormat = VKSWAPCHAIN.imageFormat swapchainInfo
  let attachmentDescription =
        AttachmentDescription
          zero
          swapchainFormat
          SAMPLE_COUNT_1_BIT
          ATTACHMENT_LOAD_OP_CLEAR
          ATTACHMENT_STORE_OP_STORE
          ATTACHMENT_LOAD_OP_DONT_CARE
          ATTACHMENT_STORE_OP_DONT_CARE
          IMAGE_LAYOUT_UNDEFINED
          IMAGE_LAYOUT_PRESENT_SRC_KHR
  let attachmentReference = AttachmentReference 0 IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  let subpassDescription =
        SubpassDescription
          zero
          PIPELINE_BIND_POINT_GRAPHICS
          empty -- inputAttachments
          (fromList [attachmentReference]) -- colorAttachments
          empty -- resolveAttachments
          Nothing -- depthStencilAttachment
          empty -- preserveAttachments
  let subpassDependency =
        SubpassDependency
          SUBPASS_EXTERNAL
          0
          PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
          zero
          ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          zero
  let renderPassCreateInfo =
        RenderPassCreateInfo
          ()
          zero
          (fromList [attachmentDescription])
          (fromList [subpassDescription])
          (fromList [subpassDependency])
  initRenderPass <- createRenderPass device renderPassCreateInfo Nothing
  let pipelineCreateInfo =
        GraphicsPipelineCreateInfo
          () -- next
          zero -- flags
          shaderStages -- stages
          (Just (SomeStruct $ vertexStageInfo)) -- vertexInputState
          (Just inputAssembly) -- inputAssemblyState
          Nothing -- tessellationState
          (Just (SomeStruct $ initViewportState)) -- viewportState
          (SomeStruct rasterizerState) -- rasterizationState
          (Just (SomeStruct initMultisampleState)) -- multisampleState
          Nothing -- depthStencilState
          (Just (SomeStruct colorBlendCreate)) -- colorBlendState
          Nothing -- dynamicState
          pipelineLayout -- layout
          initRenderPass -- renderPass
          0 -- subpass
          NULL_HANDLE -- basePipelineHandle
          (-1) -- basePipelineIndex
  (_, pipelines) <- createGraphicsPipelines device NULL_HANDLE (fromList [SomeStruct pipelineCreateInfo]) Nothing
  let onepipe = pipelines ! 0
  return $ PipelineEtc onepipe initRenderPass descriptorLayout pipelineLayout (fromList [vm, fm])

-- | Clean up the pipeline - usually called via 'bracket'
destroyPipelineEtc :: Device -> PipelineEtc -> IO ()
destroyPipelineEtc dev (PipelineEtc pipeline renderPassInstance descriptorLayoutInstance layoutInstance modules) = do
  mapM_ (\m -> destroyShaderModule dev m Nothing) modules
  destroyPipelineLayout dev layoutInstance Nothing
  destroyRenderPass dev renderPassInstance Nothing
  destroyPipeline dev pipeline Nothing








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




data OneVertex = OneVertex (V2 Float) (V3 Float)

instance Storable OneVertex where
  sizeOf (OneVertex a1 a2) = sizeOf a1 + sizeOf a2
  alignment _ = 4
  peek p = do
    a1 <- peek (castPtr @OneVertex @(V2 Float) p)
    a2 <- peekByteOff p 8
    return (OneVertex a1 a2)
  poke p (OneVertex a1 a2) = do
    poke (castPtr @OneVertex @(V2 Float) p) a1
    pokeByteOff p 8 a2

-- Vertex data for drawing
vertexData :: [OneVertex]
vertexData =
  [ (OneVertex (V2 (-0.5) (-0.5)) (V3 1.0 0.0 0.0)),
    (OneVertex (V2 0.5 (-0.5)) (V3 0.0 1.0 0.0)),
    (OneVertex (V2 0.5 0.5) (V3 1.0 1.0 1.0)),
    (OneVertex (V2 (-0.5) (0.5)) (V3 0.0 0.0 1.0))
  ]

vertexDataBinding :: VertexInputBindingDescription
vertexDataBinding = VertexInputBindingDescription 0 (4 * 5) VERTEX_INPUT_RATE_VERTEX

vertexInputAttrs :: [VertexInputAttributeDescription]
vertexInputAttrs =
  [ (VertexInputAttributeDescription 0 0 FORMAT_R32G32_SFLOAT 0),
    (VertexInputAttributeDescription 1 0 FORMAT_R32G32B32_SFLOAT 8)
  ]

indexData :: [Word16]
indexData = [0, 1, 2, 2, 3, 0]

newtype MemoryTypeMask = MemoryTypeMask Word32
  deriving (Eq, Show)

-- Look through the physical device memory types and find one that matches a bit in the memoryTypeBits
-- and
findMemoryType :: PhysicalDevice -> MemoryTypeMask -> MemoryPropertyFlags -> IO Word32
findMemoryType pd (MemoryTypeMask typeBits) memFlags = do
  memProps <- getPhysicalDeviceMemoryProperties pd
  let typeIndices = [0 .. (fromIntegral $ memoryTypeCount memProps - 1)]
  let bitsMatch b1 b2 = (b1 .&. b2) /= zeroBits
  let matchesTypeBits = \i -> testBit typeBits i
  let matchesProperties = \i ->
        let memType = (memoryTypes memProps) ! i
         in bitsMatch (propertyFlags memType) memFlags
  let fullMatch = \i -> matchesTypeBits i && matchesProperties i
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
  mem <- allocateMemory d allocInfo Nothing
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
  let allocInfo = CommandBufferAllocateInfo cp COMMAND_BUFFER_LEVEL_PRIMARY 1
  cBufs <- allocateCommandBuffers d allocInfo
  let cBuf = cBufs ! 0
  let beginInfo = CommandBufferBeginInfo () COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT Nothing
  beginCommandBuffer cBuf beginInfo
  cmdCopyBuffer cBuf srcBuf dstBuf (fromList [BufferCopy 0 0 bufSize])
  endCommandBuffer cBuf
  let submitInfo = SomeStruct $ SubmitInfo () empty empty (fromList [commandBufferHandle cBuf]) empty
  queueSubmit gq (fromList [submitInfo]) NULL_HANDLE
  queueWaitIdle gq
  freeCommandBuffers d cp (fromList [cBuf])
  return ()


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
  let createInfo = DescriptorSetLayoutCreateInfo () zero (fromList [dBinding])
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


updateUniformBuffer :: Device -> UBuffer -> Float -> Extent2D -> IO ()
updateUniformBuffer device (UBuffer _ mem) elapsedTime (Extent2D width height) = do
  -- our shaders use premultiply so matrices need to be transposed
  let modelMatrix = transpose $ mkTransformation (axisAngle (V3 0 0 1) elapsedTime) (V3 (sin elapsedTime) 0 (0.1))
  let viewMatrix = transpose $ lookAt (V3 2 2 2) (V3 0 0 0) (V3 0 0 1)
  let scaleMatrix sx sy sz = V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1)
  let aspect = (fromIntegral width) / (fromIntegral height)
  let projMatrix = transpose $ (perspective (45 * 3.14159 / 180.0) aspect 0.1 10) !*! (scaleMatrix 1 (-1) 1)
  let newUniforms = UBO modelMatrix viewMatrix projMatrix
  withMappedMemory device mem 0 (fromIntegral $ sizeOf newUniforms) zero bracket $ \ptr -> poke (castPtr ptr) newUniforms
  return ()


makeDescriptorPool :: Device -> Int -> Maybe AllocationCallbacks -> IO DescriptorPool
makeDescriptorPool device count allocator = do
  let descriptorPoolSize = DescriptorPoolSize DESCRIPTOR_TYPE_UNIFORM_BUFFER (fromIntegral count)
  let dCreateInfo = DescriptorPoolCreateInfo () zero (fromIntegral count) (fromList [descriptorPoolSize])
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

syncDescriptorSets :: Device -> Vector UBuffer -> Vector DescriptorSet -> IO ()
syncDescriptorSets device dBufs dSets = mapM_ sync1 $ zip (toList dBufs) (toList dSets)
  where
    sync1 ((UBuffer dBuf _), dSet) = do
      let bufferInfo = DescriptorBufferInfo dBuf 0 (fromIntegral $ sizeOf (undefined :: UniformBufferObject))
      let writeDescriptor = SomeStruct $ WriteDescriptorSet () dSet 0 0 1 DESCRIPTOR_TYPE_UNIFORM_BUFFER empty (fromList [bufferInfo]) empty
      updateDescriptorSets device (fromList [writeDescriptor]) empty

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
  let clearTo = fromList [Color (Float32 0 0 0 1)]
  cmdBeginRenderPass buf (RenderPassBeginInfo () pass fb renderarea clearTo) SUBPASS_CONTENTS_INLINE
  cmdBindPipeline buf PIPELINE_BIND_POINT_GRAPHICS pipe
  let (VBuffer vBuf _) = vertexBuffer tRes
  let vBufs = [vBuf]
  let offsets = [0]
  cmdBindVertexBuffers buf 0 (fromList vBufs) (fromList offsets)
  let (VBuffer ixBuf _) = indexBuffer tRes
  cmdBindIndexBuffer buf ixBuf 0 INDEX_TYPE_UINT16
  cmdBindDescriptorSets buf PIPELINE_BIND_POINT_GRAPHICS pLayout 0 (fromList [dSet]) empty
  cmdDrawIndexed buf (fromIntegral $ length indexData) 1 0 0 0
  cmdEndRenderPass buf
  endCommandBuffer buf
