module Fomorian.Vulkan.Resources.DeviceMemoryAllocator where

import Control.Exception (handle)

import Data.Bits
import Data.Map.Strict as M hiding ((!), size)
import Data.Foldable (asum)

import Data.Vector ((!))

import Data.Word (Word32)

import Vulkan.Core10 as VKCORE
    ( PhysicalDevice,
      Device,
      getPhysicalDeviceMemoryProperties,
      allocateMemory,
      freeMemory,
      MemoryHeap(MemoryHeap),
      MemoryType(MemoryType),
      PhysicalDeviceMemoryProperties(memoryTypes, memoryHeaps),
      DeviceSize,
      DeviceMemory,
      MemoryAllocateInfo(MemoryAllocateInfo) )
import qualified Vulkan.Core10 as VK
import Vulkan.Exception (VulkanException(..)) -- for catching memory allocation failures

import Fomorian.Vulkan.Resources.DeviceMemoryTypes
import Fomorian.SimpleMemoryArena

type ArenaKey = Int
type MemoryTypeIndex = Word32

-- | Vulkan-specific wrapper around memory allocations
data MemoryAllocation s = MemoryAllocation DeviceMemory MemoryTypeIndex ArenaKey (MemoryBlock s)
  deriving (Eq,Show)

-- | Vulkan-specific wrapper around memory arena
data DeviceArena = DeviceArena DeviceMemory ArenaKey (SimpleMemoryArena DeviceSize)
  deriving (Eq,Show)

--
-- arena group, which holds multiple (zero or more) arenas
--

-- | DeviceArenaGroup is a set of DeviceArenas, all for the same memory type
data DeviceArenaGroup = DeviceArenaGroup MemoryTypeIndex (M.Map ArenaKey DeviceArena)
  deriving (Eq, Show)

addArenaToGroup :: DeviceArenaGroup -> DeviceArena -> DeviceArenaGroup
addArenaToGroup (DeviceArenaGroup memix das) da@(DeviceArena _ k _) = DeviceArenaGroup memix (M.insert k da das)

freeAllArenas :: Device -> DeviceArenaGroup -> IO ()
freeAllArenas device (DeviceArenaGroup _ arenaMap) = do
  mapM_ freeArena arenaMap
    where
      freeArena (DeviceArena memHandle _ _) = freeMemory device memHandle Nothing

allocateFromCurrentArenas :: DeviceSize -> MemoryAlignment DeviceSize -> DeviceArenaGroup -> Maybe (MemoryAllocation DeviceSize, DeviceArenaGroup)
allocateFromCurrentArenas sz alignment dag@(DeviceArenaGroup memix arenaMap) =
  -- attempt to alloc from all arenas and use asum to find the first Just value. Laziness means we don't
  -- actually evaluate any arenas after the first successful one.
  case asum $ M.map (allocFromDeviceArena sz alignment memix) arenaMap of
    Just (memAlloc, arena') -> Just (memAlloc, addArenaToGroup dag arena')
    Nothing -> Nothing

-- Create a new arena with memory allocated using Vulkan's 'allocateMemory' and try to get a block from it. This can still fail, but
-- even if it fails the allocate will still have happened, so we can return 
allocateFromNewArena :: DeviceSize -> MemoryAlignment DeviceSize -> DeviceArenaGroup -> Device -> DeviceSize -> IO (Maybe (MemoryAllocation DeviceSize), DeviceArenaGroup)
allocateFromNewArena size alignment dag@(DeviceArenaGroup memix arenaMap) device newArenaSize = do
  let allocateInfo = MemoryAllocateInfo () newArenaSize memix
  -- if allocation fails a VulkanException will be thrown, so handle that
  handle (\(VulkanException _) -> return (Nothing, dag)) $ do
    result <- allocateMemory device allocateInfo Nothing
    -- choose a key one higher than the current max key for the new arena
    let newArenaKey = 1 + M.foldlWithKey' (\x k _ -> max x k) 0 arenaMap
    let newArenaConfig = SimpleMemoryArenaConfig newArenaSize 4
    let newArena = DeviceArena result newArenaKey (mkSimpleMemoryArena newArenaConfig)
    -- try to allocate from this new arena
    case allocFromDeviceArena size alignment memix newArena of
      Just (memAlloc, arena') -> return (Just memAlloc, addArenaToGroup dag arena')
      -- well THAT didn't work. keep the arena for other allocations
      Nothing -> return (Nothing, dag)

freeFromCurrentArenas :: MemoryAllocation DeviceSize -> Device -> DeviceArenaGroup -> IO (Maybe DeviceArenaGroup)
freeFromCurrentArenas alloc@(MemoryAllocation _ _ arenaKey _) _device dag@(DeviceArenaGroup _ arenaMap) =
  case M.lookup arenaKey arenaMap of
    Nothing -> return Nothing  -- couldn't find the arena for this allocation
    Just arena -> case freeFromDeviceArena alloc arena of
                    -- couldn't find the block in this arena
                    Nothing -> return Nothing
                    -- success, put updated arena back into the arena group structure
                    Just arena' -> do
                      return $ Just $ addArenaToGroup dag arena'


--
-- Main allocator
--


newtype AllocatorConfig =
  AllocatorConfig  {
    chooseArenaSize :: MemoryTypeIndex -> DeviceSize
  }

data MemoryAllocatorState s =
  MemoryAllocatorState {
    memDevice :: Device,
    physicalDevice :: PhysicalDevice,
    memoryProps :: PhysicalDeviceMemoryProperties,
    configuration :: AllocatorConfig,
    priorityMap :: MemoryPriorityMap s,
    arenaGroups :: M.Map MemoryTypeIndex DeviceArenaGroup
  }

mkMemoryAllocator :: Device -> PhysicalDevice -> AllocatorConfig -> IO (MemoryAllocatorState AbstractMemoryType)
mkMemoryAllocator dev phys config = do
  props <- getPhysicalDeviceMemoryProperties phys
  return $ MemoryAllocatorState dev phys props config (compileMemoryPriorities props) M.empty

cleanupMemoryAllocator :: MemoryAllocatorState AbstractMemoryType -> IO ()
cleanupMemoryAllocator allocator = do
  -- free arenas in all groups
  let d = memDevice allocator
  mapM_ (freeAllArenas d) (arenaGroups allocator)

isHostVisible :: MemoryAllocatorState AbstractMemoryType -> MemoryAllocation DeviceSize -> Bool
isHostVisible allocator (MemoryAllocation _ memTypeIndex _ _) =
  let memInfo = (memoryTypes $ memoryProps allocator) ! fromIntegral memTypeIndex
  in VK.propertyFlags memInfo .&. VK.MEMORY_PROPERTY_HOST_VISIBLE_BIT /= zeroBits
   

allocateDeviceMemory :: MemoryAllocatorState AbstractMemoryType -> DeviceSize -> MemoryAlignment DeviceSize -> AbstractMemoryType -> Word32 -> IO (Maybe (MemoryAllocation DeviceSize), MemoryAllocatorState AbstractMemoryType)
allocateDeviceMemory allocator memSize memAlignment memType allowedMemoryTypeBits = do
  let orderedMemoryTypes = lookupMemoryPriority (priorityMap allocator) memType
  let allowedMemoryTypes = Prelude.filter (\i -> (allowedMemoryTypeBits .&. bit (fromIntegral i)) /= zeroBits) orderedMemoryTypes
  let useMemoryTypeIndex = head allowedMemoryTypes
  allocWithMemoryTypeIndex allocator memSize memAlignment useMemoryTypeIndex

-- | Attemps to allocate a block for the given memory type index. First tries to get a block from the ArenaGroup if one exists.
--   If this fails, it creates a new arena, adds it to the arena group, and attempts to allocate from the new arena. If THAT fails it just
--   return Nothing. NOTE: Even on failure memory has been allocated as a side effect so you always need to update to the returned allocator even on failure.
allocWithMemoryTypeIndex :: MemoryAllocatorState AbstractMemoryType -> DeviceSize -> MemoryAlignment DeviceSize -> Word32 -> IO (Maybe (MemoryAllocation DeviceSize), MemoryAllocatorState AbstractMemoryType)
allocWithMemoryTypeIndex allocator size alignment memTypeIndex = do
  -- get the group for this index, defaulting to an empty group
  let arenaGroup = M.findWithDefault (DeviceArenaGroup memTypeIndex M.empty) memTypeIndex (arenaGroups allocator)
  -- we'll have to update the group in several spots in the code so just write it here once
  let updateGroup = \a g' -> a { arenaGroups = M.insert memTypeIndex g' (arenaGroups a) }
  -- try to alloc from the group
  case allocateFromCurrentArenas size alignment arenaGroup of
    -- success, update the arena group in our allocator record
    Just (alloc, arenaGroup') -> return (Just alloc, updateGroup allocator arenaGroup')
    Nothing -> do
      -- try to create a new arena and allocate from that
      let arenaSize = chooseArenaSize (configuration allocator) memTypeIndex
      (alloc, group') <- allocateFromNewArena size alignment arenaGroup (memDevice allocator) arenaSize
      return (alloc, updateGroup allocator group')

freeDeviceMemory :: MemoryAllocatorState AbstractMemoryType -> MemoryAllocation DeviceSize -> IO (Maybe (MemoryAllocatorState AbstractMemoryType))
freeDeviceMemory allocator alloc@(MemoryAllocation _memHandle memTypeIndex _aKey _block) = do
  case M.lookup memTypeIndex (arenaGroups allocator) of
    Nothing -> return Nothing
    Just arenaGroup -> do freeResult <- freeFromCurrentArenas alloc (memDevice allocator) arenaGroup
                          case freeResult of
                            Nothing -> return Nothing
                            Just arenaGroup' -> return $ Just $ allocator { arenaGroups = M.insert memTypeIndex arenaGroup' (arenaGroups allocator)}



allocFromDeviceArena :: DeviceSize -> MemoryAlignment DeviceSize -> MemoryTypeIndex -> DeviceArena -> Maybe (MemoryAllocation DeviceSize, DeviceArena)
allocFromDeviceArena size alignment memIndex (DeviceArena memHandle aKey sArena) =
  case allocBlock size alignment sArena of
    Nothing                  -> Nothing
    Just (sArena', newBlock) ->
      let newArena = DeviceArena memHandle aKey sArena'
          deviceBlock = MemoryAllocation memHandle memIndex aKey newBlock
      in
        Just (deviceBlock, newArena)

freeFromDeviceArena :: MemoryAllocation DeviceSize -> DeviceArena -> Maybe DeviceArena
freeFromDeviceArena (MemoryAllocation _ _ _ block) (DeviceArena h k sArena) =
  case returnBlock block sArena of
    Nothing -> Nothing
    Just arena' -> Just $ DeviceArena h k arena'

-- | A generic function to choose arena sizes for various memory types. This defaults to 128MB or 1/4 of the total heap size, whichever is smaller
genericChooseArenaSize :: PhysicalDeviceMemoryProperties -> MemoryTypeIndex -> DeviceSize
genericChooseArenaSize props memTypeIndex =
  let (MemoryType _memFlags heapIx) = memoryTypes props ! fromIntegral memTypeIndex
      (MemoryHeap heapSize _heapFlags) = memoryHeaps props ! fromIntegral heapIx
      defaultMemorySize = 134217728 :: DeviceSize  -- 128M
  in
    min defaultMemorySize (heapSize `div` 4)