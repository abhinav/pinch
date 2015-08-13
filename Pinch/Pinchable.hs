{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      :  Pinch.Pinchable
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Types that can be serialized into Thrift payloads implement the 'Pinchable'
-- typeclass.
--
module Pinch.Pinchable
    (

    -- * Writing instances

    -- | Instances of 'Pinchable' will usually be constructed by composing
    -- together existing instances and using the '.=', '.:', etc. helpers.

    -- ** Structs and exceptions
    -- $struct

    -- ** Unions
    -- $union

    -- ** Enums
    -- $enum

    -- * Pinchable

      Pinchable(..)

    -- ** Helpers

    -- *** @pinch@

    , (.=)
    , (?=)
    , struct
    , FieldPair

    -- *** @unpinch@

    , (.:)
    , (.:?)


    ) where


#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Data.ByteString     (ByteString)
import Data.Hashable       (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Int            (Int16, Int32, Int64)
import Data.Text           (Text)
import Data.Typeable       ((:~:) (..), Typeable, eqT)
import Data.Vector         (Vector)
import Data.Word           (Word8)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import qualified Data.Text.Encoding  as TE
import qualified Data.Vector         as V

import Pinch.Internal.TType
import Pinch.Internal.Value

-- TODO helper to pinch/unpinch enums
-- TODO helper to pinch/unpinch unions

-- | The Pinchable type class is implemented by types that can be sent over
-- the wire as Thrift payloads.
class IsTType (Tag a) => Pinchable a where
    -- | 'TType' tag for this type. For most custom types, this will most
    -- likely be 'TStruct'. For enums, it will be 'TInt32'.
    type Tag a :: *

    -- | Convert an @a@ into a 'Value'.
    --
    -- For structs, 'struct', '.=', and '?=' may be used to construct
    -- 'Value' objects tagged with 'TStruct'.
    pinch :: a -> Value (Tag a)

    -- | Read a 'Value' back into an @a@.
    --
    -- For structs, '.:' and '.:?' may be used to retrieve field values.
    unpinch :: Value (Tag a) -> Either String a


instance IsTType a => Pinchable (Value a) where
    type Tag (Value a) = a
    pinch = id
    unpinch = Right


-- | Helper to 'unpinch' values by matching TTypes.
checkedUnpinch
    :: forall a b. (Pinchable a, Typeable b)
    => Value b -> Either String a
checkedUnpinch = case eqT of
    Nothing -> const $ Left "Type mismatch"
    Just (Refl :: Tag a :~: b) -> unpinch


-- | Helper to 'pinch' maps.
pinchMap
    :: forall k v kval vval m.
        ( Pinchable k
        , Pinchable v
        , kval ~ Value (Tag k)
        , vval ~ Value (Tag v)
        )
    => ((k -> v -> HashMap kval vval -> HashMap kval vval)
           -> HashMap kval vval -> m -> HashMap kval vval)
          -- ^ @foldrWithKey@
    -> m  -- ^ map that implements @foldrWithKey@
    -> Value TMap
pinchMap folder = VMap . folder go HM.empty
  where
    go k v = HM.insert (pinch k) (pinch v)


instance Pinchable ByteString where
    type Tag ByteString = TBinary
    pinch = VBinary
    unpinch = Right . vbinary

instance Pinchable Text where
    type Tag Text = TBinary
    pinch = VBinary . TE.encodeUtf8
    unpinch = Right . TE.decodeUtf8 . vbinary

instance Pinchable Bool where
    type Tag Bool = TBool
    pinch = VBool
    unpinch = Right . vbool

instance Pinchable Word8 where
    type Tag Word8 = TByte
    pinch = VByte
    unpinch = Right . vbyte

instance Pinchable Double where
    type Tag Double = TDouble
    pinch = VDouble
    unpinch = Right . vdouble

instance Pinchable Int16 where
    type Tag Int16 = TInt16
    pinch = VInt16
    unpinch = Right . vint16

instance Pinchable Int32 where
    type Tag Int32 = TInt32
    pinch = VInt32
    unpinch = Right . vint32

instance Pinchable Int64 where
    type Tag Int64 = TInt64
    pinch = VInt64
    unpinch = Right . vint64

instance Pinchable a => Pinchable (Vector a) where
    type Tag (Vector a) = TList

    pinch = VList . V.map pinch

    unpinch (VList xs) = V.mapM checkedUnpinch xs
    unpinch x = Left $ "Failed to read list. Got " ++ show x

instance Pinchable a => Pinchable [a] where
    type Tag [a] = TList

    pinch = VList . V.fromList . map pinch

    unpinch (VList xs) = mapM checkedUnpinch $ V.toList xs
    unpinch x = Left $ "Failed to read list. Got " ++ show x

instance
  ( Eq k
  , Hashable k
  , Pinchable k
  , Pinchable v
  ) => Pinchable (HM.HashMap k v) where
    type Tag (HM.HashMap k v) = TMap

    pinch = pinchMap HM.foldrWithKey

    unpinch (VMap xs) =
        fmap HM.fromList . mapM go $ HM.toList xs
      where go (k, v) = (,) <$> checkedUnpinch k <*> checkedUnpinch v
    unpinch x = Left $ "Failed to read map. Got " ++ show x

instance (Ord k, Pinchable k, Pinchable v) => Pinchable (M.Map k v) where
    type Tag (M.Map k v) = TMap

    pinch = pinchMap M.foldrWithKey

    unpinch (VMap xs) =
        fmap M.fromList . mapM go $ HM.toList xs
      where go (k, v) = (,) <$> checkedUnpinch k <*> checkedUnpinch v
    unpinch x = Left $ "Failed to read map. Got " ++ show x

instance (Eq a, Hashable a, Pinchable a) => Pinchable (HS.HashSet a) where
    type Tag (HS.HashSet a) = TSet

    pinch = VSet . HS.map pinch

    unpinch (VSet xs) =
        fmap HS.fromList . mapM checkedUnpinch $ HS.toList xs
    unpinch x = Left $ "Failed to read set. Got " ++ show x

instance (Ord a, Pinchable a) => Pinchable (S.Set a) where
    type Tag (S.Set a) = TSet

    pinch = VSet . S.foldr (HS.insert . pinch) HS.empty

    unpinch (VSet xs) =
        fmap S.fromList . mapM checkedUnpinch $ HS.toList xs
    unpinch x = Left $ "Failed to read set. Got " ++ show x

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
struct :: [FieldPair] -> Value TStruct
struct = VStruct . foldr go HM.empty
  where
    go (_, Nothing) m = m
    go (k, Just v) m = HM.insert k v m


-- | Given a field ID and a @Value TStruct@, get the value stored in the
-- struct under that field ID. The lookup fails if the field is absent or if
-- it's not the same type as expected by this call's context.
(.:) :: forall a. Pinchable a => Value TStruct -> Int16 -> Either String a
(VStruct items) .: fieldId = do
    someValue <- note ("Field " ++ show fieldId ++ " is absent.")
               $ fieldId `HM.lookup` items
    (value :: Value (Tag a)) <-
        note ("Field " ++ show fieldId ++ " has the incorrect type. " ++
              "Expected '" ++ show (ttype :: TType (Tag a)) ++ "' but " ++
              "got '" ++ showSomeValueTType someValue ++ "'")
          $ castValue someValue
    unpinch value
  where
    showSomeValueTType (SomeValue v) = show (valueTType v)
    note msg m = case m of
        Nothing -> Left msg
        Just v -> Right v

-- | Given a field ID and a @Value TStruct@, get the optional value stored in
-- the struct under the given field ID. The value returned is @Nothing@ if it
-- was absent or the wrong type. The lookup fails only if the value retrieved
-- fails to 'unpinch'.
(.:?) :: forall a. Pinchable a
      => Value TStruct -> Int16 -> Either String (Maybe a)
(VStruct items) .:? fieldId =
    case value of
        Nothing -> return Nothing
        Just v  -> Just <$> unpinch v
  where
    value :: Maybe (Value (Tag a))
    value = fieldId `HM.lookup` items >>= castValue


-- $struct
--
-- Given a Thrift struct,
--
-- > struct Post {
-- >   1: optional string subject
-- >   2: required string body
-- > }
--
-- And a corresponding Haskell data type, the 'Pinchable' instance for it will
-- be,
--
-- @
-- instance 'Pinchable' Post where
--     type 'Tag' Post = 'TStruct'
--
--     pinch (Post subject body) =
--         'struct' [ 1 '?=' subject
--                , 2 '.=' body
--                ]
--
--     unpinch value =
--         Post \<$\> value '.:?' 1
--              \<*\> value '.:'  2
-- @
--

-- $union
--
-- Given a Thrift union,
--
-- > union PostBody {
-- >   1: string markdown
-- >   2: binary rtf
-- > }
--
-- And a corresponding Haskell data type, the 'Pinchable' instance for it will
-- be,
--
-- > instance Pinchable PostBody where
-- >     type Tag PostBody = TStruct
-- >
-- >     pinch (PostBodyMarkdown markdownBody) = struct [1 .= markdownBody]
-- >     pinch (PostBodyRtf rtfBody) = struct [2 .= rtfBody]
-- >
-- >     unpinch v = PostBodyMarkdown <$> v .: 1
-- >             <|> PostBodyRtf      <$> v .: 2

-- $enum
--
-- For an enum,
--
-- > enum Role {
-- >   DISABLED = 0,
-- >   USER,
-- >   ADMIN,
-- > }
--
-- And a corresponding Haskell data type, the 'Pinchable' instance for it will
-- be,
--
-- > instance Pinchable Role where
-- >     type Tag Role = TInt32
-- >
-- >     pinch RoleDisabled = pinch 0
-- >     pinch RoleUser     = pinch 1
-- >     pinch RoleAdmin    = pinch 2
-- >
-- >     unpinch v = do
-- >        value <- unpinch v
-- >        case (value :: Int32) of
-- >            0 -> Right RoleDisabled
-- >            1 -> Right RoleUser
-- >            2 -> Right RoleAdmin
-- >            _ -> Left $ "Unknown role: " ++ show value
--
