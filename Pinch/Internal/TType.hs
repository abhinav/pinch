{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Pinch.Internal.TType
    ( TType(..)
    , IsTType(..)
    , SomeTType(..)

    -- * Tags
    , TBool
    , TByte
    , TDouble
    , TInt16
    , TInt32
    , TInt64
    , TBinary
    , TStruct
    , TMap
    , TSet
    , TList
    ) where

import Data.Hashable (Hashable (..))
import Data.Typeable (Typeable)

data TBool   deriving (Typeable)
data TByte   deriving (Typeable)
data TDouble deriving (Typeable)
data TInt16  deriving (Typeable)
data TInt32  deriving (Typeable)
data TInt64  deriving (Typeable)
data TBinary deriving (Typeable)
data TStruct deriving (Typeable)
data TMap    deriving (Typeable)
data TSet    deriving (Typeable)
data TList   deriving (Typeable)


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


-- | Type class used to map type-leve TTypes into 'TType' objects.
class Typeable a => IsTType a where
    -- | Base on the context in which this is used, it will automatically
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
--
-- Typically, this will be pattern matched inside a case statement and code
-- that depends on the TType will be go there.
data SomeTType where
    SomeTType :: forall a. IsTType a => TType a -> SomeTType
  deriving Typeable

deriving instance Show SomeTType
