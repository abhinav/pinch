{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      :  Pinch.Generic
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Implements support for automatically deriving Pinchable instances for types
-- that implement @Generic@ and follow a specific pattern.
--
module Pinch.Generic
    ( Field(..)
    , getField
    , putField
    , field

    , Enumeration
    , enum
    ) where

import Control.Applicative
import Data.Proxy          (Proxy (..))
import Data.Typeable       (Typeable)
import GHC.Generics
import GHC.TypeLits

import qualified Data.HashMap.Strict as HM

import Pinch.Internal.Pinchable
import Pinch.Internal.TType
import Pinch.Internal.Value     (Value (..))


class Combinable t where
    combine :: Value t -> Value t -> Value t

instance Combinable TStruct where
    combine (VStruct as) (VStruct bs) = VStruct $ as `HM.union` bs


instance {-# OVERLAPPABLE #-} GPinchable a => GPinchable (M1 i c a) where
    type GTag (M1 i c a) = GTag a
    gPinch = gPinch . unM1
    gUnpinch = fmap M1 . gUnpinch

instance (Datatype d, GPinchable a) => GPinchable (D1 d a) where
    type GTag (D1 d a) = GTag a
    gPinch = gPinch . unM1
    gUnpinch v = case gUnpinch v of
        Left msg -> Left $ "Failed to read '" ++ name ++ "'. " ++ msg
        Right a -> Right $ M1 a
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
-- should be warpped inside 'Field' and tagged with the field identifier.
--
-- > data Foo = Bar (Field 1 Text) (Field 2 (Maybe Int32)) deriving Generic
--
-- Fields which hold @Maybe@ values are treated as optional. All fields must
-- be 'PInchable' to automatically derive a @Pinchable@ instance for the new
-- data type.
newtype Field (n :: Nat) a = Field a
  deriving
    (Bounded, Eq, Enum, Foldable, Functor, Monoid, Ord, Show, Traversable,
     Typeable)

-- | Gets the current value of a field.
getField :: Field n a -> a
getField (Field a) = a

-- | Puts a value inside a field.
putField :: a -> Field n a
putField = Field

-- | A lens on @Field@ wrappers for use with the lens library.
field :: Functor f => (a -> f b) -> Field n a -> f (Field n b)
field f (Field a) = Field <$> f a

instance
  {-# OVERLAPPABLE #-} (Pinchable a, KnownNat n)
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

data Enumeration (n :: Nat) = Enumeration
  deriving
    (Eq, Ord, Show, Typeable)

enum :: Enumeration n
enum = Enumeration

instance KnownNat n => GPinchable (K1 i (Enumeration n)) where
    type GTag (K1 i (Enumeration n)) = TEnum
    gPinch (K1 Enumeration) = VInt32 . fromIntegral $ natVal (Proxy :: Proxy n)
    gUnpinch (VInt32 i)
        | i == val  = return (K1 Enumeration)
        | otherwise = Left $ "Couldn't match enum value " ++ show i
      where
        val = fromIntegral $ natVal (Proxy :: Proxy n)
    gUnpinch x = Left $ "Failed to read enum. Got " ++ show x
