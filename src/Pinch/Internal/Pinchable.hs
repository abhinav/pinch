{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      :  Pinch.Internal.Pinchable
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Provides the core @Pinchable@ typeclass and the @GPinchable@ typeclass used
-- to derive instances automatically.
--
module Pinch.Internal.Pinchable
    ( Pinchable(..)

    , (.=)
    , (?=)
    , struct
    , union
    , FieldPair

    , (.:)
    , (.:?)

    , GPinchable(..)
    , genericPinch
    , genericUnpinch

    , Parser
    , runParser
    , parserCatch
    ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Data.ByteString (ByteString)
import Data.Hashable   (Hashable)
import Data.Int        (Int16, Int32, Int64, Int8)
import Data.List       (foldl')
import Data.Text       (Text)
import Data.Typeable   ((:~:) (..))
import Data.Vector     (Vector)
import GHC.Generics    (Generic, Rep)

import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as HM
import qualified Data.HashSet            as HS
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector             as V
import qualified GHC.Generics            as G

import Pinch.Internal.Pinchable.Parser
import Pinch.Internal.TType
import Pinch.Internal.Value

import qualified Pinch.Internal.FoldList as FL

-- | Implementation of 'pinch' based on 'GPinchable'.
genericPinch
    :: (Generic a, GPinchable (Rep a)) => a -> Value (GTag (Rep a))
genericPinch = gPinch . G.from

-- | Implementation of 'unpinch' based on 'GPinchable'.
genericUnpinch
    :: (Generic a, GPinchable (Rep a)) => Value (GTag (Rep a)) -> Parser a
genericUnpinch = fmap G.to . gUnpinch


-- | GPinchable is used to impelment support for automatically deriving
-- instances of Pinchable via generics.
class IsTType (GTag f) => GPinchable (f :: * -> *) where
    -- | 'TType' tag to use for objects of this type.
    type GTag f

    -- | Converts a generic representation of a value into a 'Value'.
    gPinch :: f a -> Value (GTag f)

    -- | Converts a 'Value' back into the generic representation of the
    -- object.
    gUnpinch :: Value (GTag f) -> Parser (f a)


-- | The Pinchable type class is implemented by types that can be sent or
-- received over the wire as Thrift payloads.
class IsTType (Tag a) => Pinchable a where
    -- | 'TType' tag for this type.
    --
    -- For most custom types, this will be 'TStruct', 'TUnion', or
    -- 'TException'. For enums, it will be 'TEnum'. If the instance
    -- automatically derived with use of @Generic@, this is not required
    -- because it is automatically determined by use of @Field@ or
    -- @Enumeration@.
    type Tag a
    type Tag a = GTag (Rep a)

    -- | Convert an @a@ into a 'Value'.
    --
    -- For structs, 'struct', '.=', and '?=' may be used to construct
    -- 'Value' objects tagged with 'TStruct'.
    pinch :: a -> Value (Tag a)

    -- | Read a 'Value' back into an @a@.
    --
    -- For structs, '.:' and '.:?' may be used to retrieve field values.
    unpinch :: Value (Tag a) -> Parser a

    default pinch
        :: (Generic a, GPinchable (Rep a)) => a -> Value (GTag (Rep a))
    pinch = genericPinch

    default unpinch
        :: (Generic a, GPinchable (Rep a))
        => Value (GTag (Rep a)) -> Parser a
    unpinch = genericUnpinch


------------------------------------------------------------------------------

-- | A pair of field identifier and maybe a value stored in the field. If the
-- value is absent, the field will be ignored.
type FieldPair = (Int16, Maybe SomeValue)

-- | Construct a 'FieldPair' from a field identifier and a 'Pinchable' value.
(.=) :: Pinchable a => Int16 -> a -> FieldPair
fid .= value = (fid, Just $ SomeValue (pinch value))

-- | Construct a 'FieldPair' from a field identifier and an optional
-- 'Pinchable' value.
(?=) :: Pinchable a => Int16 -> Maybe a -> FieldPair
fid ?= value = (fid, SomeValue . pinch <$> value)

-- | Construct a 'Value' tagged with a 'TStruct' from the given key-value
-- pairs. Optional fields whose values were omitted will be ignored.
--
-- > struct [1 .= ("Hello" :: Text), 2 .= (42 :: Int16)]
struct :: [FieldPair] -> Value TStruct
struct = VStruct . foldl' go HM.empty
  where
    go m (_, Nothing) = m
    go m (k,  Just v) = HM.insert k v m

-- | Constructs a 'Value' tagged with 'TUnion'.
--
-- > union 1 ("foo" :: ByteString)
--
union :: Pinchable a => Int16 -> a -> Value TUnion
union k v = VStruct (HM.singleton k (SomeValue $ pinch v))

-- | Given a field ID and a @Value TStruct@, get the value stored in the
-- struct under that field ID. The lookup fails if the field is absent or if
-- it's not the same type as expected by this call's context.
(.:) :: forall a. Pinchable a => Value TStruct -> Int16 -> Parser a
(VStruct items) .: fieldId = do
    SomeValue someValue <- note ("Field " ++ show fieldId ++ " is absent.")
               $ fieldId `HM.lookup` items
    (value :: Value (Tag a)) <-
        note ("Field " ++ show fieldId ++ " has the incorrect type. " ++
              "Expected '" ++ show (ttype :: TType (Tag a)) ++ "' but " ++
              "got '" ++ show (valueTType someValue) ++ "'")
          $ castValue someValue
    unpinch value
  where
    note msg m = case m of
        Nothing -> fail msg
        Just v -> return v

-- | Given a field ID and a @Value TStruct@, get the optional value stored in
-- the struct under the given field ID. The value returned is @Nothing@ if it
-- was absent or the wrong type. The lookup fails only if the value retrieved
-- fails to 'unpinch'.
(.:?) :: forall a. Pinchable a
      => Value TStruct -> Int16 -> Parser (Maybe a)
(VStruct items) .:? fieldId =
    case value of
        Nothing -> return Nothing
        Just v  -> Just <$> unpinch v
  where
    value :: Maybe (Value (Tag a))
    value = fieldId `HM.lookup` items >>= \(SomeValue v) -> castValue v

------------------------------------------------------------------------------

-- | Helper to 'unpinch' values by matching TTypes.
checkedUnpinch
    :: forall a b. (Pinchable a, IsTType b)
    => Value b -> Parser a
checkedUnpinch = case ttypeEqT of
    Nothing -> const . fail $
        "Type mismatch. Expected " ++ show ttypeA ++ ". Got " ++ show ttypeB
    Just (Refl :: Tag a :~: b) -> unpinch
  where
    ttypeA = ttype :: TType (Tag a)
    ttypeB = ttype :: TType b

-- | Helper to 'pinch' maps.
pinchMap
    :: (Pinchable k, Pinchable v)
    => (forall r. (r -> k -> v -> r) -> r -> m k v -> r)
          -- ^ @foldlWithKey@
    -> m k v
    -> Value TMap
pinchMap foldlWithKey = VMap . FL.map go . FL.fromMap foldlWithKey
  where
    go (!k, !v) = MapItem (pinch k) (pinch v)

unpinchMap
    :: (Pinchable k, Pinchable v)
    => (k -> v -> m -> m) -> m -> Value a -> Parser m
unpinchMap mapInsert mapEmpty (VMap xs) =
    FL.foldl' (\m (!k, !v) -> mapInsert k v m) mapEmpty <$> FL.mapM go xs
  where
    go (MapItem k v) = (,) <$> checkedUnpinch k <*> checkedUnpinch v
unpinchMap _ mapEmpty VNullMap = return mapEmpty
unpinchMap _ _ x = fail $ "Failed to read map. Got " ++ show x

instance IsTType a => Pinchable (Value a) where
    type Tag (Value a) = a
    pinch = id
    unpinch = return

instance Pinchable ByteString where
    type Tag ByteString = TBinary
    pinch = VBinary
    unpinch (VBinary b) = return b
    unpinch x = fail $ "Failed to read binary. Got " ++ show x

instance Pinchable BL.ByteString where
    type Tag BL.ByteString = TBinary
    pinch = VBinary . BL.toStrict
    unpinch (VBinary b) = return (BL.fromStrict b)
    unpinch x = fail $ "Failed to read binary. Got " ++ show x

instance Pinchable Text where
    type Tag Text = TBinary
    pinch = VBinary . TE.encodeUtf8
    unpinch (VBinary b) = return . TE.decodeUtf8 $ b
    unpinch x = fail $ "Failed to read string. Got " ++ show x

instance Pinchable TL.Text where
    type Tag TL.Text = TBinary
    pinch = VBinary . BL.toStrict . TLE.encodeUtf8
    unpinch (VBinary b) = return . TL.fromStrict . TE.decodeUtf8 $ b
    unpinch x = fail $ "Failed to read string. Got " ++ show x

instance Pinchable Bool where
    type Tag Bool = TBool
    pinch = VBool
    unpinch (VBool x) = return x
    unpinch x = fail $ "Failed to read boolean. Got " ++ show x

instance Pinchable Int8 where
    type Tag Int8 = TByte
    pinch = VByte
    unpinch (VByte x) = return x
    unpinch x = fail $ "Failed to read byte. Got " ++ show x

instance Pinchable Double where
    type Tag Double = TDouble
    pinch = VDouble
    unpinch (VDouble x) = return x
    unpinch x = fail $ "Failed to read double. Got " ++ show x

instance Pinchable Int16 where
    type Tag Int16 = TInt16
    pinch = VInt16
    unpinch (VInt16 x) = return x
    unpinch x = fail $ "Failed to read i16. Got " ++ show x

instance Pinchable Int32 where
    type Tag Int32 = TInt32
    pinch = VInt32
    unpinch (VInt32 x) = return x
    unpinch x = fail $ "Failed to read i32. Got " ++ show x

instance Pinchable Int64 where
    type Tag Int64 = TInt64
    pinch = VInt64
    unpinch (VInt64 x) = return x
    unpinch x = fail $ "Failed to read i64. Got " ++ show x

instance Pinchable a => Pinchable (Vector a) where
    type Tag (Vector a) = TList

    pinch = VList . FL.map pinch . FL.fromFoldable

    unpinch (VList xs) =
        V.fromList . FL.toList <$> FL.mapM checkedUnpinch xs
    unpinch x = fail $ "Failed to read list. Got " ++ show x

instance Pinchable a => Pinchable [a] where
    type Tag [a] = TList

    pinch = VList . FL.map pinch . FL.fromFoldable

    unpinch (VList xs) = FL.toList <$> FL.mapM checkedUnpinch xs
    unpinch x = fail $ "Failed to read list. Got " ++ show x

instance
  ( Eq k
  , Hashable k
  , Pinchable k
  , Pinchable v
  ) => Pinchable (HM.HashMap k v) where
    type Tag (HM.HashMap k v) = TMap
    pinch = pinchMap HM.foldlWithKey'
    unpinch = unpinchMap HM.insert HM.empty

instance (Ord k, Pinchable k, Pinchable v) => Pinchable (M.Map k v) where
    type Tag (M.Map k v) = TMap
    pinch = pinchMap M.foldlWithKey'
    unpinch = unpinchMap M.insert M.empty

instance (Eq a, Hashable a, Pinchable a) => Pinchable (HS.HashSet a) where
    type Tag (HS.HashSet a) = TSet
    pinch = VSet . FL.map pinch . FL.fromFoldable

    unpinch (VSet xs) =
        FL.foldl' (\s !a -> HS.insert a s) HS.empty
        <$> FL.mapM checkedUnpinch xs
    unpinch x = fail $ "Failed to read set. Got " ++ show x

instance (Ord a, Pinchable a) => Pinchable (S.Set a) where
    type Tag (S.Set a) = TSet
    pinch = VSet . FL.map pinch . FL.fromFoldable

    unpinch (VSet xs) =
        FL.foldl' (\s !a -> S.insert a s) S.empty
        <$> FL.mapM checkedUnpinch xs
    unpinch x = fail $ "Failed to read set. Got " ++ show x
