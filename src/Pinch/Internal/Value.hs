{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      :  Pinch.Internal.Value
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- This module defines an intermediate representation for Thrift values and
-- functions to work with the intermediate representation.
module Pinch.Internal.Value
    ( Value(..)
    , MapItem(..)
    , SomeValue(..)
    , castValue
    , valueTType
    ) where

import Control.DeepSeq     (NFData (..))
import Data.ByteString     (ByteString)
import Data.Hashable       (Hashable (..))
import Data.HashMap.Strict (HashMap)
import Data.Int            (Int16, Int32, Int64, Int8)
import Data.List           (intercalate)
import Data.Typeable       ((:~:) (..), Typeable)

import qualified Data.Foldable       as F
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

import Pinch.Internal.FoldList (FoldList)
import Pinch.Internal.TType

-- | A single item in a map
data MapItem k v = MapItem !(Value k) !(Value v)
  deriving (Eq, Typeable)

instance NFData (MapItem k v) where
    rnf (MapItem k v) = rnf k `seq` rnf v `seq` ()

instance Hashable (MapItem k v) where
    hashWithSalt s (MapItem k v) = s `hashWithSalt` k `hashWithSalt` v

instance Show (MapItem k v) where
    show (MapItem k v) = show k ++ ": " ++ show v

-- | @Value@ maps directly to serialized representation of Thrift types. It
-- contains about as much information as what gets sent over the wire.
-- @Value@ objects are tagged with different 'TType' values to indicate the
-- type of the value.
--
-- Typical usage will not involve accessing the constructors for this type.
-- 'Pinch.Pinchable.Pinchable' must be used to construct 'Value' objects or
-- convert them back to original types.
data Value a where
    VBool   :: !Bool                      -> Value TBool
    VByte   :: !Int8                      -> Value TByte
    VDouble :: !Double                    -> Value TDouble
    VInt16  :: !Int16                     -> Value TInt16
    VInt32  :: !Int32                     -> Value TInt32
    VInt64  :: !Int64                     -> Value TInt64
    VBinary :: !ByteString                -> Value TBinary
    VStruct :: !(HashMap Int16 SomeValue) -> Value TStruct

    VMap  :: forall k v. (IsTType k, IsTType v)
          => !(FoldList (MapItem k v)) -> Value TMap
    VNullMap :: Value TMap
    VSet  :: forall a. IsTType a => !(FoldList (Value a)) -> Value TSet
    VList :: forall a. IsTType a => !(FoldList (Value a)) -> Value TList
  deriving Typeable

instance Show (Value a) where
    show (VBool   x) = show x
    show (VByte   x) = show x
    show (VDouble x) = show x
    show (VInt16  x) = "i16(" ++ show x ++ ")"
    show (VInt32  x) = "i32(" ++ show x ++ ")"
    show (VInt64  x) = "i64(" ++ show x ++ ")"
    show (VBinary x) = show x

    show (VStruct x) = "{" ++ s ++ "}"
      where
        s = intercalate ", " (M.foldlWithKey' go [] x)
        go xs i (SomeValue val) = (show i ++ ": " ++ show val):xs

    show (VMap x) = show x
    show VNullMap = "[]"
    show (VSet  x) = show x
    show (VList x) = show x

instance Eq (Value a) where
    VBool   a == VBool   b = a == b
    VByte   a == VByte   b = a == b
    VDouble a == VDouble b = a == b
    VInt16  a == VInt16  b = a == b
    VInt32  a == VInt32  b = a == b
    VInt64  a == VInt64  b = a == b
    VBinary a == VBinary b = a == b
    VStruct a == VStruct b = a == b

    VList as == VList bs = areEqual1 as bs
    VMap as == VMap  bs = areEqual2 (toMap as) (toMap bs)
      where
        toMap = M.toList . F.foldl' (\m (MapItem k v) -> M.insert k v m) M.empty
    VNullMap == VMap xs  = null xs
    VMap xs  == VNullMap = null xs
    VSet as  == VSet bs  = areEqual1 (toSet as) (toSet bs)
    _ == _ = False

toSet :: forall f x. (F.Foldable f, Hashable x, Eq x) => f x -> S.HashSet x
toSet = F.foldl' (flip S.insert) S.empty

instance NFData (Value a) where
    rnf (VBool   a) = rnf a
    rnf (VByte   a) = rnf a
    rnf (VDouble a) = rnf a
    rnf (VInt16  a) = rnf a
    rnf (VInt32  a) = rnf a
    rnf (VInt64  a) = rnf a
    rnf (VBinary a) = rnf a
    rnf (VStruct a) = rnf a
    rnf (VMap   as) = rnf as
    rnf VNullMap    = ()
    rnf (VSet   as) = rnf as
    rnf (VList  as) = rnf as

-- | 'SomeValue' holds any value, regardless of type. This may be used when
-- the type of the value is not necessarily known at compile time. Typically,
-- this will be pattern matched on and code that depends on the value's
-- 'TType' will go inside the scope of the match.
data SomeValue where
    SomeValue :: (IsTType a) => !(Value a) -> SomeValue
  deriving Typeable

deriving instance Show SomeValue

instance Eq SomeValue where
    SomeValue a == SomeValue b = areEqual a b

instance NFData SomeValue where
    rnf (SomeValue a) = rnf a

-- | Safely attempt to cast a Value into another.
castValue :: forall a b. (IsTType a, IsTType b) => Value a -> Maybe (Value b)
castValue v = case ttypeEqT :: Maybe (a :~: b) of
    Just Refl -> Just v
    Nothing -> Nothing
{-# INLINE castValue #-}

-- | Get the 'TType' of a 'Value'.
valueTType :: IsTType a => Value a -> TType a
valueTType _ = ttype
{-# INLINE valueTType #-}

areEqual
    :: forall a b. (IsTType a, IsTType b) => Value a -> Value b -> Bool
areEqual l r = case ttypeEqT :: Maybe (a :~: b) of
    Just Refl -> l == r
    Nothing -> False
{-# INLINE areEqual #-}

areEqual1
    :: forall a b f. (IsTType a, IsTType b, Foldable f, Eq (f (Value a)))
    => f (Value a) -> f (Value b) -> Bool
areEqual1 l r = case ttypeEqT of
    Just (Refl :: a :~: b) -> l == r
    Nothing -> False
{-# INLINE areEqual1 #-}

areEqual2
    :: forall k1 v1 k2 v2.
    ( IsTType k1, IsTType v1, IsTType k2, IsTType v2
    ) => [(Value k1, Value v1)] -> [(Value k2, Value v2)] -> Bool
areEqual2 l r = case ttypeEqT of
    Just (Refl :: k1 :~: k2) -> case ttypeEqT of
        Just (Refl :: v1 :~: v2) -> l == r
        Nothing -> False
    Nothing -> False
{-# INLINE areEqual2 #-}

instance Hashable (Value a) where
    hashWithSalt s a = case a of
      VBinary x -> s `hashWithSalt` (0 :: Int) `hashWithSalt` x
      VBool   x -> s `hashWithSalt` (1 :: Int) `hashWithSalt` x
      VByte   x -> s `hashWithSalt` (2 :: Int) `hashWithSalt` x
      VDouble x -> s `hashWithSalt` (3 :: Int) `hashWithSalt` x
      VInt16  x -> s `hashWithSalt` (4 :: Int) `hashWithSalt` x
      VInt32  x -> s `hashWithSalt` (5 :: Int) `hashWithSalt` x
      VInt64  x -> s `hashWithSalt` (6 :: Int) `hashWithSalt` x
      VList   x -> s `hashWithSalt` (7 :: Int) `hashWithSalt` x
      VMap    x -> s `hashWithSalt` (8 :: Int) `hashWithSalt` x
      VNullMap  -> s `hashWithSalt` (8 :: Int)
      VSet    x -> s `hashWithSalt` (9 :: Int) `hashWithSalt` x

      VStruct fields ->
        M.foldlWithKey' (\s' k v -> s' `hashWithSalt` k `hashWithSalt` v)
                        (s `hashWithSalt` (10 :: Int))
                        fields


instance Hashable SomeValue where
    hashWithSalt s (SomeValue v) = hashWithSalt s v
