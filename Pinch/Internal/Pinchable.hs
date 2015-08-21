{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Pinch.Internal.Pinchable
    ( GPinchable(..)
    , Pinchable(..)

    , (.=)
    , (?=)
    , struct
    , FieldPair
    , (.:)
    , (.:?)
    ) where

import Data.Int     (Int16)
import GHC.Generics (Generic, Rep)

import qualified Data.HashMap.Strict as HM
import qualified GHC.Generics        as G

import Pinch.Internal.TType
import Pinch.Internal.Value

genericPinch
    :: (Generic a, GPinchable (Rep a)) => a -> Value (GTag (Rep a))
genericPinch = gPinch . G.from

genericUnpinch
    :: (Generic a, GPinchable (Rep a))
    => Value (GTag (Rep a)) -> Either String a
genericUnpinch = fmap G.to . gUnpinch

class IsTType (GTag f) => GPinchable (f :: * -> *) where
    type GTag f
    gPinch :: f a -> Value (GTag f)
    gUnpinch :: Value (GTag f) -> Either String (f a)


-- | The Pinchable type class is implemented by types that can be sent or
-- received over the wire as Thrift payloads.
class IsTType (Tag a) => Pinchable a where
    -- | 'TType' tag for this type. For most custom types, this will most
    -- likely be 'TStruct'. For enums, it will be 'TInt32'.
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
    unpinch :: Value (Tag a) -> Either String a

    default pinch
        :: (Generic a, GPinchable (Rep a)) => a -> Value (GTag (Rep a))
    pinch = genericPinch

    default unpinch
        :: (Generic a, GPinchable (Rep a))
        => Value (GTag (Rep a)) -> Either String a
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
