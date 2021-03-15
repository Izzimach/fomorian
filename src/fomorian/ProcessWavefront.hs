{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module Fomorian.ProcessWavefront 
  (OBJBufferRecord, 
   OBJBufferFormat, 
   loadWavefrontOBJFile)
    where


import qualified Data.Either as E
import Data.Maybe (fromMaybe)

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Vector ((!?))
import Data.Row
import qualified Data.Row.Records as Rec

import Foreign.Ptr
import Foreign.Storable

import Linear

import Codec.Wavefront


--
-- empty wavefront object
--

emptyOBJ :: WavefrontOBJ
emptyOBJ = WavefrontOBJ V.empty V.empty V.empty V.empty V.empty V.empty V.empty

{- |
   A wavefront file can specify a vertex using different index values for
   location, texture, and normal. However OpenGL cannot. So we have to
   identify all distinct combinations of (location/texture/normal) indices
   and then build a buffer holding all the distinct combinations. The
   OpenGL index buffer will then reference this vertex data.
  
   Missing indices (Nothing) will get filled with 0's
-}  
newtype OBJVertex = VD FaceIndex deriving (Eq, Show)

instance Ord OBJVertex where
  -- compare elements left-to-right, give 'Nothing' a value of 0
  compare (VD x) (VD y) = let (FaceIndex l0 t0 n0) = x
                              (FaceIndex l1 t1 n1) = y
                          in case compare l0 l1 of
                               LT -> LT
                               GT -> GT
                               EQ -> let t0' = fromMaybe 0 t0
                                         t1' = fromMaybe 0 t1
                                     in case compare t0' t1' of
                                       LT -> LT
                                       GT -> GT
                                       EQ -> let n0' = fromMaybe 0 n0
                                                 n1' = fromMaybe 0 n1
                                             in compare n0' n1'


-- Normally a Face Element has at least three FaceIndex elements plus
-- optional additional elements. This is awkward to work with so instead
-- we convert it to a list of FaceIndex elements

faceElementToList :: Element Face -> [OBJVertex]
faceElementToList ef =
  let (Face a b c ds) = elValue ef
  in fmap VD (a:b:c:ds)


-- |Build a list of unique FaceIndex values used in a WavefrontOBJ
buildOBJVertexSet :: WavefrontOBJ -> S.Set OBJVertex
buildOBJVertexSet wobj = S.fromList . (concatMap faceElementToList) . objFaces $ wobj

-- |Maps from a specific OBJVertex (which is actually just a FaceIndex) 
-- |to an index into the hardware vertex buffer. Used for building the index buffer.
newtype OBJFaceIndexLookup = VI (M.Map OBJVertex Int) deriving (Eq, Show)

-- |Builds a map to map from OBJVertex values to an index into the 
-- |final vertex buffer.
buildFaceIndexLookup :: S.Set OBJVertex -> OBJFaceIndexLookup
buildFaceIndexLookup faceverts = VI $ M.fromList $ zip (S.toList faceverts) [0..]

type OBJBufferFormat = ("pos3" .== (V3 Float) .+ "texCoord" .== (V2 Float) .+ "normal" .== (V3 Float))
newtype OBJBufferRecord = OBJBufferRecord (Rec OBJBufferFormat)

instance Storable OBJBufferRecord where
  sizeOf _ = let x = sizeOf (undefined :: Float)
             in 8 * x
  alignment = sizeOf
  peek p =
    do
      let b = castPtr p :: Ptr Float
      p0 <- peekElemOff b 0 
      p1 <- peekElemOff b 1
      p2 <- peekElemOff b 2
      t0 <- peekElemOff b 3
      t1 <- peekElemOff b 4
      n0 <- peekElemOff b 5
      n1 <- peekElemOff b 6
      n2 <- peekElemOff b 7
      return $ OBJBufferRecord (#pos3 .== (V3 p0 p1 p2) .+ 
                                #texCoord .== (V2 t0 t1) .+
                                #normal .== (V3 n0 n1 n2))

  poke p(OBJBufferRecord r) =
    do
      let b = castPtr p :: Ptr Float
      let (V3 v0 v1 v2) = r .! #pos3
      let (V2 t0 t1) = r .! #texCoord
      let (V3 n0 n1 n2) = r .! #normal
      pokeElemOff b 0 v0
      pokeElemOff b 1 v1
      pokeElemOff b 2 v2
      pokeElemOff b 3 t0
      pokeElemOff b 4 t1
      pokeElemOff b 5 n0
      pokeElemOff b 6 n1
      pokeElemOff b 7 n2


{-| 
   Generate actual record for a given OBJVertex
 
   this is kind of a mess since the texture and normal data may be
   nonexistant ('Nothing' values) and the lookups are 1-indexed instead of 0-indexed
-}
genOBJVertexRecord :: WavefrontOBJ -> OBJVertex -> OBJBufferRecord
genOBJVertexRecord obj (VD v) =
  let (FaceIndex l t n) = v
      locLookup = (objLocations obj) !? (l - 1)
      -- use monad Maybe to shortcut Nothing values in t or n
      texLookup = do t' <- t; objTexCoords obj !? (t' - 1)
      normLookup = do n' <- n; objNormals obj !? (n' - 1)

      loc = maybe  (V3 0 0 0) (V3 <$> locX <*> locY <*> locZ) locLookup
      tex = maybe  (V2 0 0)   (V2 <$> texcoordR <*> texcoordS) texLookup
      norm = maybe (V3 0 0 1) (V3 <$> norX <*> norY <*> norZ) normLookup
  in
      OBJBufferRecord $ (#pos3 .== loc) .+ (#texCoord .== tex) .+ (#normal .== norm)

-- |Generate a full list of vertex buffer data
genOBJVertexData :: WavefrontOBJ -> S.Set OBJVertex -> [OBJBufferRecord]
genOBJVertexData obj faceverts = fmap (genOBJVertexRecord obj) (S.toList faceverts)

-- |Generate index list from Face data
genOBJFaceIndexData :: WavefrontOBJ -> OBJFaceIndexLookup -> [Int]
genOBJFaceIndexData obj (faceindexlookup) = concatMap (faceixs faceindexlookup) (objFaces obj)
  where
    faceixs (VI vertlookup) face = 
      let verts = faceElementToList face
          --
          -- if there are more than 3 vertices on this face we need to
          -- generate a triangle fan. Given vertices:
          --  [a b c d e f]
          -- the triangle fan would be made up of triangles:
          --  (a,b,c) (a,c,d) (a,d,e) (a,e,f)
          vertexfan = zipWith3 (\a b c -> [a,b,c])
                               (repeat $ head verts)
                               (drop 1 verts)
                               (drop 2 verts)
      in fmap (\x -> M.findWithDefault 0 x vertlookup) (concat vertexfan)

-- |Given a WavefrontOBJ generate the vertex buffer data and index buffer data
genWavefrontBuffers :: WavefrontOBJ -> ([OBJBufferRecord],[Int])
genWavefrontBuffers obj = let uniqueVerts = buildOBJVertexSet obj
                              faceIndexLookup = buildFaceIndexLookup uniqueVerts
                          in
                            (genOBJVertexData obj uniqueVerts,
                             genOBJFaceIndexData obj faceIndexLookup)

loadWavefrontOBJFile :: String -> IO (Either String ([OBJBufferRecord],[Int]))
loadWavefrontOBJFile f = do
  x <- fromFile f
  case x of
    Left s -> return $ Left s
    Right obj -> return $ Right (genWavefrontBuffers obj)


{-|
   In ghci you can run 'x <- loadTestOBJ' to put the wavefront data into x
   This will produce an empty object if the file load fails.
-}
loadTestOBJ :: IO WavefrontOBJ
loadTestOBJ = do
  x <- fromFile "resources/geometry/testcube.obj"
  return $ E.fromRight emptyOBJ x

