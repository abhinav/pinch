{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      :  Pinch.Internal.TType
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Defines the different types Thrift supports at the protocol level.
--
module Pinch.Internal.TType
    (
    -- * TType

      TType(..)
    , IsTType(..)
    , SomeTType(..)

    , ttypeEquality
    , ttypeEqT

    -- * Tags

    , TBool
    , TByte
    , TDouble
    , TInt16
    , TInt32
    , TEnum
    , TInt64
    , TBinary
    , TText
    , TStruct
    , TUnion
    , TException
    , TMap
    , TSet
    , TList
    ) where

import Data.Hashable (Hashable (..))
import Data.Typeable ((:~:) (..), Typeable)

-- | > bool
data TBool   deriving (Typeable)

-- | > byte
data TByte   deriving (Typeable)

-- | > double
data TDouble deriving (Typeable)

-- | > i16
data TInt16  deriving (Typeable)

-- | > i32
data TInt32  deriving (Typeable)

-- | > enum
type TEnum = TInt32

-- | > i64
data TInt64  deriving (Typeable)

-- | > binary
data TBinary deriving (Typeable)

-- | > string
type TText = TBinary

-- | > struct
data TStruct deriving (Typeable)

-- | > union
type TUnion = TStruct

-- | > exception
type TException = TStruct

-- | > map<k, v>
data TMap    deriving (Typeable)

-- | > set<x>
data TSet    deriving (Typeable)

-- | > list<x>
data TList   deriving (Typeable)


-- | Represents the type of a Thrift value.
--
-- Objects of this type are tagged with one of the TType tags, so this type
-- also acts as a singleton on the TTypes. It allows writing code that can
-- enforce properties about the TType of values at compile time.
data TType a where
    TBool   :: TType TBool    -- 2
    TByte   :: TType TByte    -- 3
    TDouble :: TType TDouble  -- 4
    TInt16  :: TType TInt16   -- 6
    TInt32  :: TType TInt32   -- 8
    TInt64  :: TType TInt64   -- 10
    TBinary :: TType TBinary  -- 11
    TStruct :: TType TStruct  -- 12
    TMap    :: TType TMap     -- 13
    TSet    :: TType TSet     -- 14
    TList   :: TType TList    -- 15
  deriving (Typeable)

deriving instance Show (TType a)
deriving instance Eq (TType a)

instance Hashable (TType a) where
    hashWithSalt s TBool   = s `hashWithSalt` (0  :: Int)
    hashWithSalt s TByte   = s `hashWithSalt` (1  :: Int)
    hashWithSalt s TDouble = s `hashWithSalt` (2  :: Int)
    hashWithSalt s TInt16  = s `hashWithSalt` (3  :: Int)
    hashWithSalt s TInt32  = s `hashWithSalt` (4  :: Int)
    hashWithSalt s TInt64  = s `hashWithSalt` (5  :: Int)
    hashWithSalt s TBinary = s `hashWithSalt` (6  :: Int)
    hashWithSalt s TStruct = s `hashWithSalt` (7  :: Int)
    hashWithSalt s TMap    = s `hashWithSalt` (8  :: Int)
    hashWithSalt s TSet    = s `hashWithSalt` (9  :: Int)
    hashWithSalt s TList   = s `hashWithSalt` (10 :: Int)


-- | Typeclass used to map type-leve TTypes into 'TType' objects. All TType
-- tags are instances of this class.
class Typeable a => IsTType a where
    -- | Based on the context in which this is used, it will automatically
    -- return the corresponding 'TType' object.
    ttype :: TType a


instance IsTType TBool   where ttype = TBool
instance IsTType TByte   where ttype = TByte
instance IsTType TDouble where ttype = TDouble
instance IsTType TInt16  where ttype = TInt16
instance IsTType TInt32  where ttype = TInt32
instance IsTType TInt64  where ttype = TInt64
instance IsTType TBinary where ttype = TBinary
instance IsTType TStruct where ttype = TStruct
instance IsTType TMap    where ttype = TMap
instance IsTType TSet    where ttype = TSet
instance IsTType TList   where ttype = TList


-- | Used when the 'TType' for something is not known at compile time.
-- Typically, this will be pattern matched inside a case statement and code
-- that depends on the TType will be go there.
data SomeTType where
    SomeTType :: forall a. IsTType a => TType a -> SomeTType
  deriving Typeable

deriving instance Show SomeTType

-- | Witness the equality of two ttypes.
ttypeEquality :: TType a -> TType b -> Maybe (a :~: b)
ttypeEquality TBool   TBool   = Just Refl
ttypeEquality TByte   TByte   = Just Refl
ttypeEquality TDouble TDouble = Just Refl
ttypeEquality TInt16  TInt16  = Just Refl
ttypeEquality TInt32  TInt32  = Just Refl
ttypeEquality TInt64  TInt64  = Just Refl
ttypeEquality TBinary TBinary = Just Refl
ttypeEquality TStruct TStruct = Just Refl
ttypeEquality TMap    TMap    = Just Refl
ttypeEquality TSet    TSet    = Just Refl
ttypeEquality TList   TList   = Just Refl
ttypeEquality _       _       = Nothing
{-# INLINE ttypeEquality #-}

-- | Witness the equality of two TTypes.
--
-- Implicit version of @ttypeEquality@.
ttypeEqT :: forall a b. (IsTType a, IsTType b) => Maybe (a :~: b)
ttypeEqT = ttypeEquality ttype ttype
{-# INLINE ttypeEqT #-}
