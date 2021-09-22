-- | Code for a basic arena used for dynamically allocating chunks of memory. The arena starts as one large free block which is
--   divided up as needed to hand out for memory allocations. When the memory is freed (or "returned") the memory block is
--   converted back into a free block and merged with adjacent free blocks. Alignment and padding are supported. There is no
--   support for compaction.
module Fomorian.SimpleMemoryArena (
  mkSimpleMemoryArena,
  allocBlock,
  returnBlock,
  MemoryBlock,
  blockOffset,
  blockSize,
  SimpleMemoryArena,
  SimpleMemoryArenaConfig(..),
  ArenaStats,
  MemoryAlignment (..),
  getArenaStats,
  freeSpace,
  usedSpace,
  blockCount
  ) where


import Data.Vector ((!), Vector, fromList)
import qualified Data.Vector as V

data MemoryBlockState = UsedBlock | FreeBlock
  deriving (Eq, Show)

-- | Each chunk of memory is either allocated or free. The type parameter @s@ is the numeric datatype used for size and offset.
data MemoryBlock s =
  MemoryBlock {
    isFree :: MemoryBlockState,
    blockOffset :: s,
    blockSize :: s
  }
  deriving (Eq, Show)

data SimpleMemoryArenaConfig s =
  SimpleMemoryArenaConfig {
    totalSize :: s,
    blockPadding :: s
  }
  deriving (Eq, Show)

data SimpleMemoryArena s =
  SimpleMemoryArena {
    memoryConfig :: SimpleMemoryArenaConfig s,
    blocks :: Vector (MemoryBlock s)
  }
  deriving (Eq)

-- | Used to specify memory alignment. We use this to distinguish between an offset/size value and an alignment value
newtype MemoryAlignment s = MemAlign s

-- | Use a compact representation of the arena for display
instance (Show s) => Show (SimpleMemoryArena s) where
  show arena = "SimpleMemoryArena { config = " ++ show (memoryConfig arena) ++ " blocks = [" ++ compactShow (V.toList $ blocks arena) ++ "]"
    where
      compactShow [] = ""
      compactShow (b:bs) =
        let blockType = case (isFree b) of
                          UsedBlock -> "Used"
                          FreeBlock -> "Free"
        in "(" ++ blockType ++ " " ++ show (blockOffset b) ++ " " ++ show (blockSize b) ++ ")" ++ compactShow bs

-- | Use this to create the initial arena. All the space in the arena is marked free initially.
mkSimpleMemoryArena :: (Integral s) => SimpleMemoryArenaConfig s -> SimpleMemoryArena s
mkSimpleMemoryArena config = 
  SimpleMemoryArena {
      memoryConfig = config,
      -- start with a single free memory block
      blocks = (fromList [
                  MemoryBlock {
                    isFree = FreeBlock,
                    blockOffset = 0,
                    blockSize = (totalSize config)
                  }
                ])
    }

roundValueUpToAlignment :: (Integral s) => s -> s -> s
roundValueUpToAlignment x alignment = roundValueDownToAlignment (x + alignment - 1) alignment

roundValueDownToAlignment :: (Integral s) => s -> s -> s
roundValueDownToAlignment x alignment = (x `div` alignment) * alignment

-- | Given a free block, split it into a used block and (possibly) a freed block with the remaining free memory.
--   If the allocation uses up all the free space of this block it is just converted into a used block instead of splitting.
splitFreeBlock :: (Integral s) => s -> MemoryAlignment s -> MemoryBlock s -> Maybe [MemoryBlock s]
-- can't allocate from a used block!
splitFreeBlock _         _                    (MemoryBlock UsedBlock _ _)                 = Nothing
splitFreeBlock allocSize (MemAlign alignment) (MemoryBlock FreeBlock origOffset origSize) =
  let alignedAllocSize = roundValueUpToAlignment allocSize alignment
  in
    if (alignedAllocSize > origSize)
    -- this free block isn't big enough to hold the allocation!
    then Nothing
    else if (alignedAllocSize == origSize)
    -- allocation is the exact size of the free block, so just convert it to a used block
    then Just $ [MemoryBlock UsedBlock origOffset origSize]
    -- alloc a used block at the start of the free block, and put the remaining space in a free block after the used block
    else let b1 = MemoryBlock UsedBlock origOffset alignedAllocSize
             b2 = MemoryBlock FreeBlock (origOffset + alignedAllocSize) (origSize - alignedAllocSize)
         in Just $ [b1,b2]


-- | Apply the given function on successive elements until one succeeds (return a 'Just') and returns the index and result. If no element
--   produces a 'Just' then this returns 'Nothing'
findIndexFirstJust :: (a -> Maybe b) -> Vector a -> Maybe (Int, b)
findIndexFirstJust justTest elems = checkElements justTest elems 0
  where
    checkElements fj v ix =
      if ix >= length v
      then Nothing
      else case fj (v ! ix) of
        Nothing -> checkElements fj v (ix+1)
        Just result -> Just (ix, result)

-- | Find an area of free memory in this arena and allocate it. Returns the updated arena and a 'MemoryBlock' of the newly-allocated region.
--   If it can no free space large enough, return 'Nothing'
allocBlock :: (Integral s) => s -> MemoryAlignment s -> SimpleMemoryArena s -> Maybe (SimpleMemoryArena s, MemoryBlock s)
allocBlock findSize alignment (SimpleMemoryArena arenaConfig arenaBlocks) =
  -- try to split each block until we find one that succeeds
  case findIndexFirstJust (splitFreeBlock findSize alignment) arenaBlocks of
    Nothing -> Nothing
    Just (ix,splitResult) ->
      let beforeBlocks = V.take ix arenaBlocks
          afterBlocks = V.drop (ix+1) arenaBlocks
          newArena = SimpleMemoryArena arenaConfig (V.concat [beforeBlocks, fromList splitResult, afterBlocks])
          newBlock = head splitResult
      in Just (newArena, newBlock)
                   
mergeBlocks :: (Integral s) => MemoryBlock s -> MemoryBlock s -> MemoryBlock s
mergeBlocks (MemoryBlock u o s) (MemoryBlock _ _ s') = MemoryBlock u o (s+s')


-- | Take a previously-allocated block and return it to free space. Returns 'Nothing' if there
--   was an error (most likely the block was already freed or allocated from a different arena).
returnBlock :: (Integral s) => MemoryBlock s -> SimpleMemoryArena s -> Maybe (SimpleMemoryArena s)
returnBlock block (SimpleMemoryArena arenaConfig arenaBlocks) =
  case (V.findIndex (== block) arenaBlocks) of
    Nothing -> Nothing
    Just ix -> let beforeBlocks = V.take ix arenaBlocks
                   afterBlocks = V.drop (ix+1) arenaBlocks
                   newBlock = MemoryBlock FreeBlock (blockOffset block) (blockSize block)
               in Just $ SimpleMemoryArena arenaConfig (coalesceBlocks beforeBlocks newBlock afterBlocks)
      where
        coalesceBlocks :: (Integral s) => Vector (MemoryBlock s) -> MemoryBlock s -> Vector (MemoryBlock s) -> Vector (MemoryBlock s)
        coalesceBlocks pre b post =
          -- try to merge with adjacent blocks in post and pre vectors!
          let (b', post') = if (length post > 0 && (isFree (V.head post) == FreeBlock))
                            then (mergeBlocks b (V.head post), V.tail post)
                            else (b, post)
              (b'', pre') = if (length pre > 0 && (isFree (V.last pre) == FreeBlock))
                            then (mergeBlocks b' (V.last pre), V.take ((length pre) - 1) pre)
                            else (b', pre)
          in V.concat [pre',V.singleton b'',post']

-- | Some stats so you can check what's going on in the arena.
data ArenaStats s = 
  ArenaStats {
    freeSpace :: s,
    usedSpace :: s,
    blockCount :: Int
  }
  deriving (Eq, Show)

-- | Get some useful information about the arena
getArenaStats :: (Integral s) => SimpleMemoryArena s -> ArenaStats s
getArenaStats arena =
  let allBlocks = V.toList (blocks arena)
      isFreeBlock = \b -> isFree b == FreeBlock
      isUsedBlock = \b -> isFree b == UsedBlock
      sumSomeBlocks = \f -> sum $ fmap blockSize (filter f allBlocks)
  in
    ArenaStats (sumSomeBlocks isFreeBlock) (sumSomeBlocks isUsedBlock) (length allBlocks)