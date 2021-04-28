{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fomorian.Vulkan.SwapChainEtc where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.ByteString (ByteString, readFile)
import Data.Foldable
import Data.IORef
import Data.Vector ((!), (//), Vector, empty, findIndex, fromList)
import Data.Word (Word16, Word32, Word64)
import Fomorian.Windowing
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import GHC.Int
import qualified Graphics.UI.GLFW as GLFW
import Linear (V2 (..), V3 (..))
import System.FilePath
import Vulkan.CStruct.Extends
import Vulkan.Core10 as VKCORE
import Vulkan.Core10.DeviceInitialization as VKDI
import Vulkan.Core10.MemoryManagement as VKMEM
import Vulkan.Exception
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Extensions.VK_KHR_surface as VKSURFACE
import Vulkan.Extensions.VK_KHR_swapchain as VKSWAPCHAIN
import Vulkan.Extensions.VK_KHR_win32_surface
import Vulkan.Zero

-- | Holds a swapchain and the associated data handles.
data SwapchainEtc = SwapchainEtc
  { theSwapchain :: SwapchainKHR,
    swapchainImages :: Vector Image,
    swapchainImageViews :: Vector ImageView,
    swapchainFramebuffers :: Vector Framebuffer,
    swapchainCommandBuffers :: Vector CommandBuffer,
    swapchainPipeline :: PipelineEtc,
    swapchainCreateInfo :: SwapchainCreateInfoKHR '[]
  }
  deriving (Show)

data TransientResources = TransientResources
  { vertexBuffer :: VBuffer
  }
  deriving (Show)

data DeviceEtc = DeviceEtc
  { physicalHandle :: PhysicalDevice,
    graphicsQueueIndex :: Word32,
    presentQueueIndex :: Word32
  }
  deriving (Eq, Show)


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




-- | Get all the info for a given surface: capabilities, formats, present modes.
--   Returns a swapchain create info struct with a NULL oldswapchain value.
generateSwapchainInfo :: DeviceEtc -> SurfaceKHR -> IO (SwapchainCreateInfoKHR '[])
generateSwapchainInfo (DeviceEtc d gq pq) s = do
  capabilities <- getPhysicalDeviceSurfaceCapabilitiesKHR d s
  (SUCCESS, formats) <- getPhysicalDeviceSurfaceFormatsKHR d s
  (SUCCESS, pmodes) <- getPhysicalDeviceSurfacePresentModesKHR d s
  let desiredSwapChainImages = chooseSwapChainImageCount capabilities
  let presentFormat = choosePresentationFormat formats
  let swapChainSize = chooseSwapChainImageSize capabilities
  let (usageBits, transformBits) = checkSwapChainUsageAndTransform capabilities
  let presentingMode = choosePresentMode pmodes
  let whatImageSharingMode = if (gq == pq) then SHARING_MODE_EXCLUSIVE else SHARING_MODE_CONCURRENT
  let whatQueueFamilyIndices = if (gq == pq) then empty else (fromList [gq, pq])
  return $
    SwapchainCreateInfoKHR
      () -- pNext
      zero -- swapchain create flags
      s -- surface
      desiredSwapChainImages -- minImageCount
      (VKSURFACE.format presentFormat) -- imageFormat
      (VKSURFACE.colorSpace presentFormat) -- imageColorSpace
      swapChainSize -- imageExtent
      1 -- imageArrayLayers
      usageBits -- imageUsage
      whatImageSharingMode -- imageSharingMode
      whatQueueFamilyIndices -- queueFamilyIndices if sharing is concurrent
      transformBits -- preTransform
      COMPOSITE_ALPHA_OPAQUE_BIT_KHR -- compositeAlpha
      presentingMode -- presentMode
      True -- clipped
      NULL_HANDLE

-- | Creates a swapchain and passes and IORef to the wrapped user function. We provide an IORef because
--   sometimes you have to recreate the swapchain in-place without restarting the program.
withSwapchainEtc :: (MonadIO io) => Device -> SwapchainCreateInfoKHR '[] -> CommandPool -> TransientResources -> Maybe AllocationCallbacks -> (io (IORef SwapchainEtc) -> (IORef SwapchainEtc -> io ()) -> r) -> r
withSwapchainEtc device createInfo cpool tr allocator wrapper = wrapper startSwapchainRef endSwapchainRef
  where
    startSwapchainRef :: (MonadIO io) => io (IORef SwapchainEtc)
    startSwapchainRef =
      do
        swETC <- createSwapchainEtc device cpool tr createInfo allocator
        liftIO $ newIORef swETC
    endSwapchainRef :: (MonadIO io) => IORef SwapchainEtc -> io ()
    endSwapchainRef ref =
      do
        swETC <- liftIO $ readIORef ref
        destroySwapchainEtc device cpool allocator swETC

createSwapchainEtc :: (MonadIO io) => Device -> CommandPool -> TransientResources -> SwapchainCreateInfoKHR '[] -> Maybe AllocationCallbacks -> io SwapchainEtc
createSwapchainEtc device cpool tRes createInfo allocator = do
  newSwapchain <- createSwapchainKHR device createInfo allocator
  (_, newImages) <- getSwapchainImagesKHR device newSwapchain
  newImageViews <- createImageViews device createInfo newImages
  newPipe <- liftIO $ buildSimplePipeline device createInfo
  framebuffers <- makeFramebuffers device newPipe createInfo newImageViews
  cmdBuffers <- makeCommandBuffers device cpool framebuffers
  liftIO $ recordCommandBuffers cmdBuffers framebuffers tRes createInfo newPipe
  return (SwapchainEtc newSwapchain newImages newImageViews framebuffers cmdBuffers newPipe createInfo)

destroySwapchainEtc :: (MonadIO io) => Device -> CommandPool -> Maybe AllocationCallbacks -> SwapchainEtc -> io ()
destroySwapchainEtc device cpool allocator swETC = do
  let (SwapchainEtc sc _ imageViews framebuffers commandbuffers pipe _) = swETC
  mapM (\fb -> destroyFramebuffer device fb Nothing) framebuffers
  freeCommandBuffers device cpool commandbuffers
  liftIO $ destroyPipelineEtc device pipe
  mapM (\iv -> destroyImageView device iv Nothing) imageViews
  destroySwapchainKHR device sc allocator

recreateSwapChainEtc :: (MonadIO io) => Device -> CommandPool -> TransientResources -> SwapchainEtc -> DeviceEtc -> SurfaceKHR -> Maybe AllocationCallbacks -> io SwapchainEtc
recreateSwapChainEtc device cpool tRes oldswETC chosen surfaceK allocator = do
  destroySwapchainEtc device cpool allocator oldswETC
  newCreateInfo <- liftIO $ generateSwapchainInfo chosen surfaceK
  createSwapchainEtc device cpool tRes newCreateInfo allocator


createImageViews :: (MonadIO io) => Device -> SwapchainCreateInfoKHR '[] -> Vector Image -> io (Vector ImageView)
createImageViews device swapchainInfo images =
  let idSw = COMPONENT_SWIZZLE_IDENTITY
      mkImageCreateInfo i =
        ImageViewCreateInfo
          ()
          zero
          i
          IMAGE_VIEW_TYPE_2D
          (imageFormat swapchainInfo)
          (ComponentMapping idSw idSw idSw idSw)
          (ImageSubresourceRange IMAGE_ASPECT_COLOR_BIT 0 1 0 1)
      mkView img = createImageView device (mkImageCreateInfo img) Nothing
   in traverse mkView images

makeFramebuffers :: (MonadIO io) => Device -> PipelineEtc -> SwapchainCreateInfoKHR '[] -> Vector ImageView -> io (Vector Framebuffer)
makeFramebuffers device pipe sce imageViews = do
  let (Extent2D w h) = (VKSWAPCHAIN.imageExtent $ sce)
  let rPass = rendPass pipe
  let mkFramebuffers iv = createFramebuffer device (FramebufferCreateInfo () zero rPass (fromList [iv]) w h 1) Nothing
  fbs <- mapM mkFramebuffers imageViews
  return fbs

makeCommandBuffers :: (MonadIO io) => Device -> CommandPool -> Vector Framebuffer -> io (Vector CommandBuffer)
makeCommandBuffers device cmdpool fb = do
  let count = fromIntegral $ length fb
  let allocInfo = CommandBufferAllocateInfo cmdpool COMMAND_BUFFER_LEVEL_PRIMARY count
  buffers <- allocateCommandBuffers device allocInfo
  return buffers

recreateSwapchainEtc :: Device -> IORef SwapchainEtc -> IO ()
recreateSwapchainEtc device swref = do
  swapchainStuff <- readIORef swref
  (SUCCESS, newImages) <- getSwapchainImagesKHR device (theSwapchain swapchainStuff)
  let newswapchainStuff = swapchainStuff {swapchainImages = newImages}
  writeIORef swref newswapchainStuff
  return ()

-- | Given some surface picks a suitable number of surfaces. This will:
--   - Try to use one more than the minimum number of images.
--   - If that exceeds the maximum allowed, uses the maximum allowed.
chooseSwapChainImageCount :: SurfaceCapabilitiesKHR -> Word32
chooseSwapChainImageCount s =
  let minImages = (VKSURFACE.minImageCount s)
      maxImages = (VKSURFACE.maxImageCount s)
      desiredImages = minImages + 1
   in -- if 'maxImages' is 0 there is no upper limit so we don't need to clamp
      if (maxImages == 0)
        then desiredImages
        else min desiredImages maxImages

-- | Picks a format. Tries to find an R8G8B8_UNORM format, but if that's
--   not found just picks the first format in the list.
choosePresentationFormat :: Vector SurfaceFormatKHR -> SurfaceFormatKHR
choosePresentationFormat fs =
  let desiredFormat = FORMAT_R8G8B8A8_SRGB
      desiredColorspace = COLOR_SPACE_SRGB_NONLINEAR_KHR
      formatCount = length fs
      hasFormat =
        ( \f ->
            (VKSURFACE.format f == desiredFormat)
              && (VKSURFACE.colorSpace f == desiredColorspace)
        )
   in -- the driver can throw up it's hands (if it had hands) and say "idc what format you use"
      -- so we check for that first
      if (formatCount == 1 && ((VKSURFACE.format (fs ! 0)) == FORMAT_UNDEFINED))
        then (SurfaceFormatKHR desiredFormat desiredColorspace)
        else case (find hasFormat fs) of
          -- if we found a good format use it, otherwise settle for the first format
          Just f' -> f'
          Nothing -> fs ! 0

chooseSwapChainImageSize :: SurfaceCapabilitiesKHR -> Extent2D
chooseSwapChainImageSize s =
  let (Extent2D w _h) = currentExtent s
   in -- use whatever currentExtent is. If currentExtent is -1 we have to choose our own extent
      if (w /= maxBound)
        then currentExtent s
        else
          let (Extent2D minW minH) = minImageExtent s
              (Extent2D maxW maxH) = maxImageExtent s
              chooseW = max minW (min maxW 640)
              chooseH = max minH (min maxH 480)
           in (Extent2D chooseW chooseH)

checkSwapChainUsageAndTransform :: SurfaceCapabilitiesKHR -> (ImageUsageFlagBits, SurfaceTransformFlagBitsKHR)
checkSwapChainUsageAndTransform s =
  let hasBits = supportedUsageFlags s
      needBits =
        [ IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
          IMAGE_USAGE_TRANSFER_DST_BIT
        ]
      checkBit = \bitX -> if (hasBits .&. bitX) == zero then ["usage not supported: " ++ show bitX] else []
      checks = concat $ map checkBit needBits
      allBits = foldl1 (.|.) needBits
   in --transformBits = supportedTransforms s
      if (length checks > 0)
        then error $ "Cannot create swapchain. " ++ (concat checks)
        else (allBits, SURFACE_TRANSFORM_IDENTITY_BIT_KHR)

choosePresentMode :: Vector PresentModeKHR -> PresentModeKHR
choosePresentMode pmodes =
  -- use fifo mode if found, otherwise error
  case (find (== PRESENT_MODE_FIFO_KHR) pmodes) of
    Nothing -> error "No FIFO presentation mode found"
    Just m -> m

data PipelineEtc = PipelineEtc
  { pipelineInstance :: Pipeline,
    rendPass :: RenderPass,
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

-- | make a createinfo for a given shader module
shaderStageBoilerplate :: ShaderModule -> ByteString -> SomeStruct (PipelineShaderStageCreateInfo)
shaderStageBoilerplate sm entry =
  SomeStruct $
    PipelineShaderStageCreateInfo () zero SHADER_STAGE_VERTEX_BIT sm entry Nothing

-- | Put together a simple pipeline:
--   - Use shader "tut" which has "main" entry points
--   - triangle list
--   - front face culling
--   - one color attachment
--   - one simple renderpass
buildSimplePipeline :: Device -> SwapchainCreateInfoKHR '[] -> IO PipelineEtc
buildSimplePipeline device swapchainInfo = do
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
  let rasterizerState = PipelineRasterizationStateCreateInfo () zero False False POLYGON_MODE_FILL CULL_MODE_BACK_BIT FRONT_FACE_CLOCKWISE False 0 0 0 1.0
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
  -- layout is empty for now
  let pipelineLayoutCreate = PipelineLayoutCreateInfo zero empty empty
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
  return $ PipelineEtc onepipe initRenderPass pipelineLayout (fromList [vm, fm])

-- | Clean up the pipeline - usually called via 'bracket'
destroyPipelineEtc :: Device -> PipelineEtc -> IO ()
destroyPipelineEtc dev (PipelineEtc pipeline renderPassInstance layoutInstance modules) = do
  mapM_ (\m -> destroyShaderModule dev m Nothing) modules
  destroyPipelineLayout dev layoutInstance Nothing
  destroyRenderPass dev renderPassInstance Nothing
  destroyPipeline dev pipeline Nothing



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

data VBuffer = VBuffer Buffer DeviceMemory
  deriving (Eq, Show)

data StagingBuffer = StagingBuffer Buffer DeviceMemory

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

destroyVertexBuffer :: Device -> VBuffer -> Maybe AllocationCallbacks -> IO ()
destroyVertexBuffer d (VBuffer b mem) allocator = do
  destroyBuffer d b allocator
  freeMemory d mem allocator

destroyStagingBuffer :: Device -> StagingBuffer -> Maybe AllocationCallbacks -> IO ()
destroyStagingBuffer d (StagingBuffer b mem) allocator = do
  destroyBuffer d b allocator
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

withTransientResources ::
  Device ->
  PhysicalDevice ->
  CommandPool ->
  Queue ->
  Maybe AllocationCallbacks ->
  (IO TransientResources -> (TransientResources -> IO ()) -> r) ->
  r
withTransientResources d pd cPool gq allocator wrapper = wrapper loadResources unloadResources
  where
    loadResources = do
      v <- createLocalVertexBuffer d pd cPool gq vertexData allocator
      return (TransientResources v)
    unloadResources (TransientResources vb) = destroyVertexBuffer d vb allocator

-- | Record the same set of commands (pre-defined in 'recordCommands') to all the commandbuffers passed
--   in. Needs the framebuffers associated with each command buffer.
recordCommandBuffers :: Vector CommandBuffer -> Vector Framebuffer -> TransientResources -> SwapchainCreateInfoKHR '[] -> PipelineEtc -> IO ()
recordCommandBuffers cbs fbs tRes sci pipeETC = do
  let buffers = zip (toList cbs) (toList fbs)
  let goBuffers (cb, fb) = recordCommands cb tRes sci (rendPass pipeETC) (pipelineInstance pipeETC) fb
  mapM_ goBuffers buffers
  return ()

-- | Simple recording of a command buffer to draw using the given renderpass, pipeline, and framebuffer.
recordCommands :: CommandBuffer -> TransientResources -> SwapchainCreateInfoKHR '[] -> RenderPass -> Pipeline -> Framebuffer -> IO ()
recordCommands buf tRes sce pass pipe fb = do
  beginCommandBuffer buf (CommandBufferBeginInfo () zero Nothing)
  let renderarea = Rect2D (Offset2D 0 0) (VKSWAPCHAIN.imageExtent $ sce)
  let clearTo = fromList [Color (Float32 0 0 0 1)]
  cmdBeginRenderPass buf (RenderPassBeginInfo () pass fb renderarea clearTo) SUBPASS_CONTENTS_INLINE
  cmdBindPipeline buf PIPELINE_BIND_POINT_GRAPHICS pipe
  let (VBuffer vBuf _) = vertexBuffer tRes
  let vBufs = [vBuf]
  let offsets = [0]
  cmdBindVertexBuffers buf 0 (fromList vBufs) (fromList offsets)
  cmdDraw buf (fromIntegral $ length vertexData) 1 0 0
  cmdEndRenderPass buf
  endCommandBuffer buf
