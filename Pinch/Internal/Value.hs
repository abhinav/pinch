{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
module Pinch.Internal.Value
    ( Value(..)
    , SomeValue(..)
    , someValueTType
    ) where

import Data.ByteString     (ByteString)
import Data.Hashable       (Hashable (..))
import Data.HashMap.Strict (HashMap)
import Data.HashSet        (HashSet)
import Data.Int            (Int16, Int32, Int64)
import Data.Proxy          (Proxy (..))
import Data.Typeable       ((:~:) (..), Typeable, eqT)
import Data.Vector         (Vector)
import Data.Word           (Word8)

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.Vector         as V

import Pinch.Internal.TType


-- | Over-the-wire representation of Thrift data types.
data Value a where
    VBool   :: { vbool   ::       !Bool } -> Value TBool
    VByte   :: { vbyte   ::      !Word8 } -> Value TByte
    VDouble :: { vdouble ::     !Double } -> Value TDouble
    VInt16  :: { vint16  ::      !Int16 } -> Value TInt16
    VInt32  :: { vint32  ::      !Int32 } -> Value TInt32
    VInt64  :: { vint64  ::      !Int64 } -> Value TInt64
    VBinary :: { vbinary :: !ByteString } -> Value TBinary
    VStruct :: { vstruct :: !(HashMap Int16 SomeValue) } -> Value TStruct

    VMap  :: forall k v. (IsTType k, IsTType v)
          => !(TType k) -> !(TType v)
          -> !(HashMap (Value k) (Value v)) -> Value TMap
    VSet  :: forall a. IsTType a
          => !(TType a) -> !(HashSet (Value a)) -> Value TSet
    VList :: forall a. IsTType a
          => !(TType a) -> !(Vector (Value a)) -> Value TList
  deriving Typeable

deriving instance Show (Value a)

instance Eq (Value a) where
    VBool   a == VBool   b = a == b
    VByte   a == VByte   b = a == b
    VDouble a == VDouble b = a == b
    VInt16  a == VInt16  b = a == b
    VInt32  a == VInt32  b = a == b
    VInt64  a == VInt64  b = a == b
    VBinary a == VBinary b = a == b
    VStruct a == VStruct b = a == b

    VMap ak av as == VMap bk bv bs = areEqualSeq2 ak av as bk bv bs
    VSet    at as == VSet    bt bs = areEqualSeq at as bt bs
    VList   at as == VList   bt bs = areEqualSeq at as bt bs

    _ == _ = False


-- | Container that holds any value, regardless of type.
data SomeValue where
    SomeValue :: (IsTType a) => !(Value a) -> SomeValue
  deriving Typeable

deriving instance Show SomeValue

instance Eq SomeValue where
    SomeValue a == SomeValue b = areEqual a b

-- | Retrieves the TType for 'SomeValue'.
someValueTType :: SomeValue -> SomeTType
someValueTType (SomeValue v) = SomeTType (getTType v)
  where
    getTType :: forall a. IsTType a => Value a -> TType a
    getTType _ = ttype (Proxy :: Proxy a)


-- | Helper to compare two types that are not known to be equal at compile
-- time.
areEqual
    :: forall a b. (Typeable a, Typeable b, Eq a) => a -> b -> Bool
areEqual x y = case eqT of
    Nothing -> False
    Just (Refl :: a :~: b) -> x == y

areEqualSeq
    :: forall a b f.
        ( Eq (f (Value a))
        , Eq (f (Value b))
        , IsTType a
        , IsTType b
        )
    => TType a -> f (Value a) -> TType b -> f (Value b) -> Bool
areEqualSeq at as bt bs = case eqT of
    Nothing -> False
    Just (Refl :: a :~: b) -> at == bt && as == bs


areEqualSeq2
    :: forall ak av bk bv f.
        ( Eq (f (Value ak) (Value av))
        , Eq (f (Value bk) (Value bv))
        , IsTType ak, IsTType av
        , IsTType bk, IsTType bv
        )
    => TType ak -> TType av -> f (Value ak) (Value av)
    -> TType bk -> TType bv -> f (Value bk) (Value bv)
    -> Bool
areEqualSeq2 ak av as bk bv bs = case eqT of
    Nothing -> False
    Just (Refl :: ak :~: bk) -> case eqT of
        Nothing -> False
        Just (Refl :: av :~: bv) ->
            ak == bk && av == bv && as == bs


instance Hashable (Value a) where
    hashWithSalt s a = case a of
      VBinary x -> s `hashWithSalt` (0 :: Int) `hashWithSalt` x
      VBool   x -> s `hashWithSalt` (1 :: Int) `hashWithSalt` x
      VByte   x -> s `hashWithSalt` (2 :: Int) `hashWithSalt` x
      VDouble x -> s `hashWithSalt` (3 :: Int) `hashWithSalt` x
      VInt16  x -> s `hashWithSalt` (4 :: Int) `hashWithSalt` x
      VInt32  x -> s `hashWithSalt` (5 :: Int) `hashWithSalt` x
      VInt64  x -> s `hashWithSalt` (6 :: Int) `hashWithSalt` x
      VList t xs ->
        V.foldr'
          (flip hashWithSalt)
          (s `hashWithSalt` (7 :: Int) `hashWithSalt` t) xs
      VMap x y xs ->
        M.foldrWithKey
          (\k v s' -> s' `hashWithSalt` k `hashWithSalt` v)
          (s `hashWithSalt` (8 :: Int) `hashWithSalt` x `hashWithSalt` y) xs
      VSet t xs ->
        S.foldr
          (flip hashWithSalt)
          (s `hashWithSalt` (9 :: Int) `hashWithSalt` t) xs
      VStruct fields ->
        M.foldrWithKey (\k v s' -> s' `hashWithSalt` k `hashWithSalt` v)
                       (s `hashWithSalt` (10 :: Int))
                       fields


instance Hashable SomeValue where
    hashWithSalt s (SomeValue v) = hashWithSalt s v
