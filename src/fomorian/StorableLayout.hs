{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Contains a data type to describe data layout of vertex attribute arrays. The data type here is in a Neutral format
--   that can be transformed into a backend-specific format. Also includes some plumbing to let you derive
--   the data layout for haskell records automatically using generics. For instance, you could build a vertex
--   format using a record
--
--   > data PosUV = PosUV { position :: V3 Float, uv :: V2 Float }
--
--   and use auto record layout to generate the data layout. You can also derive Storable as well, and then use
--   this record to build vertex buffers.
module Fomorian.StorableLayout where

import Data.Row.Records hiding (Map)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Singletons
import Data.Singletons.TH

import GHC.TypeLits
import GHC.Generics

import Linear (V2(..), V3(..), V4(..))

import Type.Reflection

import Foreign.Storable (Storable)

data NeutralElementalType =
    NeutralFloat64
  | NeutralFloat32
  | NeutralInt32
  | NeutralInt8
  deriving (Eq,Show)

neutralElementSize :: NeutralElementalType -> Int
neutralElementSize NeutralFloat64 = 8
neutralElementSize NeutralFloat32 = 4
neutralElementSize NeutralInt32   = 4
neutralElementSize NeutralInt8    = 1

genSingletons [''NeutralElementalType]

-- A type family to convert types into types of kind 'NeutralElementalType'. These will get converted from type-level
-- things back into term-level values via 'fromSing'.
-- Note that 'Integer' and tuples are not handled since neither has a 'Storable' instance

type family ElementalTypeOf f :: NeutralElementalType
type instance ElementalTypeOf Double = 'NeutralFloat64
type instance ElementalTypeOf Float = 'NeutralFloat32
type instance ElementalTypeOf Int = 'NeutralInt32
type instance ElementalTypeOf Word8 = 'NeutralInt8
type instance ElementalTypeOf (Linear.V2 x) = ElementalTypeOf x
type instance ElementalTypeOf (Linear.V3 x) = ElementalTypeOf x
type instance ElementalTypeOf (Linear.V4 x) = ElementalTypeOf x

-- we'll use 'Nat' to track elemental component counts

type family ElementalCount f :: Nat
type instance ElementalCount Double = 1
type instance ElementalCount Float = 1
type instance ElementalCount Int = 1
type instance ElementalCount Word8 = 1
type instance ElementalCount (Linear.V2 x) = 2 * ElementalCount x
type instance ElementalCount (Linear.V3 x) = 3 * ElementalCount x
type instance ElementalCount (Linear.V4 x) = 4 * ElementalCount x




-- | fomorian representation of a Vertex Attribute. Convert to your backend-specific representation.
data DataLayoutElement e =
  DataLayoutElement {
    numComponents :: Int,
    dataType :: e,
    offset :: Int  -- offset in bytes
  }
  deriving (Eq, Show)

-- | A map of elements for the given data layout. Holds a label/name for each element and a description of the
--   data for that element. Also holds the stride of the overall data.
data DataLayoutMap = 
  DataLayoutMap
  {
    stride :: Int,
    elements :: Map Text (DataLayoutElement NeutralElementalType)
  }
  deriving (Eq, Show)

instance Semigroup DataLayoutMap where
  a <> b = let totalStride = (stride a + stride b)
               addOffset (DataLayoutElement nC dT offs) = DataLayoutElement nC dT (offs + stride a)
               a' = elements a
               b' = M.map addOffset (elements b)
           in DataLayoutMap totalStride (M.union a' b')
           
instance Monoid DataLayoutMap where
  mempty = DataLayoutMap 0 M.empty


type ElementalField a = (KnownNat (ElementalCount a), SingI (ElementalTypeOf a), Storable a)

-- | Given some type proxy for some elemental data type, produce the DataLayoutElement for it.
singleLayoutElement :: forall e. ElementalField e => Proxy e -> DataLayoutElement NeutralElementalType
singleLayoutElement _ =
                  DataLayoutElement
                  {
                    -- we use type families to figure out the elemental type and count as type-level terms, then
                    -- reify into values using 'natVal' and 'fromSing'
                    numComponents = fromIntegral $ natVal (Proxy @(ElementalCount e)),
                    dataType      =              fromSing (sing  @(ElementalTypeOf e)),
                    offset        = 0
                  }

-- | given the proxy for a single element produce a layout map. You have to provide the name of the element since
--   there's no information in the type to deduce a name.
singleLayoutMap :: forall e. ElementalField e => Proxy e -> Text -> DataLayoutMap
singleLayoutMap p elementName =
  let element = singleLayoutElement p
      totalSize = numComponents element * neutralElementSize (dataType element)
  in
    DataLayoutMap totalSize (M.singleton elementName element)


--
-- Derive data layout via Generics
--

-- | Given a haskell record, generates a 'DataLayoutMap' using Generics. Assumes data is stored in the order fields are declared.
--   This should be the same order as Storable derived using derive-storable but there should be a test to check that.
autoRecordLayout :: (Generic a, RecordLayoutG (Rep a)) => a -> DataLayoutMap
autoRecordLayout = recordLayoutG . from


-- generic instance that just processes a single field of a record. This means we only 
-- need an instance for the generic 'K1', anything else should fail. If a generic instance
-- that isn't K1 shows up then the datatype probably isn't a record of elemental fields.
class SingleElementG f where
  singleElementG :: f p -> DataLayoutElement NeutralElementalType

instance (ElementalField c) => SingleElementG (K1 i c) where
  singleElementG (K1 _) = singleLayoutElement (Proxy @c)


-- instance to handle records with multiple fields (products)
class RecordLayoutG f where
  recordLayoutG :: f p -> DataLayoutMap

-- the field selector constructor @M1 S@ contains the field name, so we pull it out and then
-- generate the element data using 'SingleElementG' on the subtree. Then we combine those into
-- a single-element 'DataLayoutMap'
instance (SingleElementG f, KnownSymbol lbl) => RecordLayoutG (M1 S ('MetaSel ('Just lbl) b c d) f) where
  recordLayoutG (M1 x) =
    let element = singleElementG x
        elementName = T.pack $ symbolVal (Proxy @lbl)
        elementStride = numComponents element * neutralElementSize (dataType element)
    in DataLayoutMap elementStride (M.singleton elementName element)

-- we ignore metadata that isn't a field selector
instance (RecordLayoutG f) => RecordLayoutG (M1 D t f) where
  recordLayoutG (M1 x) = recordLayoutG x

instance (RecordLayoutG f) => RecordLayoutG (M1 C t f) where
  recordLayoutG (M1 x) = recordLayoutG x

-- we assume that left -> right ordering follows the generic storable (GStorable) layout. Should make a test for this in case it changes at some point...
instance (RecordLayoutG a, RecordLayoutG b) => RecordLayoutG (a :*: b) where
  recordLayoutG (x :*: y) = recordLayoutG x <> recordLayoutG y

