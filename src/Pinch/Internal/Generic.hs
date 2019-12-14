{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE OverlappingInstances       #-}
#define OVERLAP
#else
#define OVERLAP {-# OVERLAPPABLE #-}
#endif
-- |
-- Module      :  Pinch.Internal.Generic
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Implements support for automatically deriving Pinchable instances for types
-- that implement @Generic@ and follow a specific pattern.
--
module Pinch.Internal.Generic
    ( Field(..)
    , getField
    , putField
    , field

    , Enumeration(..)
    , enum

    , Void(..)
    ) where


#if __GLASGOW_HASKELL__ < 709
import Data.Foldable    (Foldable)
import Data.Traversable (Traversable)
#endif

import Data.Binary         (Binary)
import Data.Semigroup
import Control.Applicative
import Control.DeepSeq     (NFData)
import Data.Proxy          (Proxy (..))
import Data.Typeable       (Typeable)
import GHC.Generics
import GHC.TypeLits

import qualified Data.HashMap.Strict as HM

import Pinch.Internal.Pinchable
import Pinch.Internal.TType
import Pinch.Internal.Value     (Value (..))


-- | Implemented by TType tags whose values know how to combine.
class Combinable t where
    combine :: Value t -> Value t -> Value t

instance Combinable TStruct where
    combine (VStruct as) (VStruct bs) = VStruct $ as `HM.union` bs


instance OVERLAP GPinchable a => GPinchable (M1 i c a) where
    type GTag (M1 i c a) = GTag a
    gPinch = gPinch . unM1
    gUnpinch = fmap M1 . gUnpinch


-- Adds the name of the data type to the error message.
instance (Datatype d, GPinchable a) => GPinchable (D1 d a) where
    type GTag (D1 d a) = GTag a
    gPinch = gPinch . unM1
    gUnpinch v =
        parserCatch (gUnpinch v)
            (\msg -> fail $ "Failed to read '" ++ name ++ "': " ++ msg)
            (return . M1)
      where
        name = datatypeName (undefined :: D1 d a b)

instance
  ( GPinchable a
  , GPinchable b
  , GTag a ~ GTag b
  , Combinable (GTag a)
  ) => GPinchable (a :*: b) where
    type GTag (a :*: b) = GTag a
    gPinch (a :*: b) = gPinch a `combine` gPinch b
    gUnpinch m = (:*:) <$> gUnpinch m <*> gUnpinch m

instance
  ( GPinchable a
  , GPinchable b
  , GTag a ~ GTag b
  ) => GPinchable (a :+: b) where
    type GTag (a :+: b) = GTag a
    gPinch (L1 a) = gPinch a
    gPinch (R1 b) = gPinch b
    gUnpinch m = L1 <$> gUnpinch m <|> R1 <$> gUnpinch m

------------------------------------------------------------------------------

-- | Fields of data types that represent structs, unions, and exceptions
-- should be wrapped inside 'Field' and tagged with the field identifier.
--
-- > data Foo = Foo (Field 1 Text) (Field 2 (Maybe Int32)) deriving Generic
-- > instance Pinchable Foo
--
-- > data A = A (Field 1 Int32) | B (Field 2 Text) deriving Generic
-- > instance Pinchable Foo
--
-- Fields which hold @Maybe@ values are treated as optional. All fields values
-- must be 'Pinchable' to automatically derive a @Pinchable@ instance for the
-- new data type.
newtype Field (n :: Nat) a = Field a
  deriving
    (Bounded, Eq, Enum, Foldable, Functor, Generic, Semigroup, Monoid, NFData, Ord, Show,
     Traversable, Typeable, Binary)

-- | Gets the current value of a field.
--
-- > let Foo a' _ = {- ... -}
-- >     a = getField a'
getField :: Field n a -> a
getField (Field a) = a

-- | Puts a value inside a field.
--
-- > Foo (putField "Hello") (putField (Just 42))
putField :: a -> Field n a
putField = Field

-- | A lens on @Field@ wrappers for use with the lens library.
--
-- > person & name . field .~ "new value"
--
field :: Functor f => (a -> f b) -> Field n a -> f (Field n b)
field f (Field a) = Field <$> f a

instance OVERLAP (Pinchable a, KnownNat n)
  => GPinchable (K1 i (Field n a)) where
    type GTag (K1 i (Field n a)) = TStruct
    gPinch (K1 (Field a)) = struct [n .= a]
      where
        n = fromIntegral $ natVal (Proxy :: Proxy n)

    gUnpinch m = K1 . Field <$> m .: n
      where
        n = fromIntegral $ natVal (Proxy :: Proxy n)

instance
  (Pinchable a, KnownNat n)
  => GPinchable (K1 i (Field n (Maybe a))) where
    type GTag (K1 i (Field n (Maybe a))) = TStruct
    gPinch (K1 (Field a)) = struct [n ?= a]
      where
        n = fromIntegral $ natVal (Proxy :: Proxy n)

    gUnpinch m = K1 . Field <$> m .:? n
      where
        n = fromIntegral $ natVal (Proxy :: Proxy n)

------------------------------------------------------------------------------

-- | Data types that represent Thrift enums must have one constructor for each
-- enum item accepting an 'Enumeration' object tagged with the corresponding
-- enum value.
--
-- > data Role = RoleUser (Enumeration 1) | RoleAdmin (Enumeration 2)
-- >   deriving Generic
-- > instance Pinchable Role
data Enumeration (n :: Nat) = Enumeration
  deriving
    (Eq, Generic, Ord, Show, Typeable)

instance NFData (Enumeration n)
instance Binary (Enumeration k)

-- | Convenience function to construct 'Enumeration' objects.
--
-- > let role = RoleUser enum
enum :: Enumeration n
enum = Enumeration

instance KnownNat n => GPinchable (K1 i (Enumeration n)) where
    type GTag (K1 i (Enumeration n)) = TEnum
    gPinch (K1 Enumeration) = VInt32 . fromIntegral $ natVal (Proxy :: Proxy n)
    gUnpinch (VInt32 i)
        | i == val  = return (K1 Enumeration)
        | otherwise = fail $ "Couldn't match enum value " ++ show i
      where
        val = fromIntegral $ natVal (Proxy :: Proxy n)
    gUnpinch x = fail $ "Failed to read enum. Got " ++ show x

------------------------------------------------------------------------------

-- | Represents a @void@ result for methods.
--
-- This should be used as an element in a response union along with 'Field'
-- tags.
--
-- For a method,
--
-- > void setValue(..) throws
-- >   (1: ValueAlreadyExists alreadyExists,
-- >    2: InternalError internalError)
--
-- Something similar to the following can be used.
--
-- > data SetValueResponse
-- >   = SetValueAlreadyExists (Field 1 ValueAlreadyExists)
-- >   | SetValueInternalError (Field 2 InternalError)
-- >   | SetValueSuccess Void
-- >   deriving (Generic)
-- >
-- > instance Pinchable SetValueResponse
data Void = Void
  deriving
    (Eq, Generic, Ord, Show, Typeable)

instance GPinchable (K1 i Void) where
    type GTag (K1 i Void) = TStruct

    gPinch (K1 Void) = struct []

    -- If the map isn't empty, there's probably an exception in there.
    gUnpinch (VStruct m) | HM.null m = return $ K1 Void
    gUnpinch x = fail $
        "Failed to read response. Expected void, got: " ++ show x
