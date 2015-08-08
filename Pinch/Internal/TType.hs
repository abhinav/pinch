{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Pinch.Internal.TType
    ( TType(..)
    , IsTType(..)
    , SomeTType(..)

    , fromCode
    , toCode

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
import Data.Proxy    (Proxy)
import Data.Typeable (Typeable)
import Data.Word     (Word8)

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
    TBool   :: TType TBool    -- ^ 2
    TByte   :: TType TByte    -- ^ 3
    TDouble :: TType TDouble  -- ^ 4
    TInt16  :: TType TInt16   -- ^ 6
    TInt32  :: TType TInt32   -- ^ 8
    TInt64  :: TType TInt64   -- ^ 10
    TBinary :: TType TBinary  -- ^ 11
    TStruct :: TType TStruct  -- ^ 12
    TMap    :: TType TMap     -- ^ 13
    TSet    :: TType TSet     -- ^ 14
    TList   :: TType TList    -- ^ 15
  deriving (Typeable)

deriving instance Show (TType a)
deriving instance Eq (TType a)


class Typeable a => IsTType a where
    ttype :: Proxy a -> TType a


instance IsTType TBool   where ttype _ = TBool
instance IsTType TByte   where ttype _ = TByte
instance IsTType TDouble where ttype _ = TDouble
instance IsTType TInt16  where ttype _ = TInt16
instance IsTType TInt32  where ttype _ = TInt32
instance IsTType TInt64  where ttype _ = TInt64
instance IsTType TBinary where ttype _ = TBinary
instance IsTType TStruct where ttype _ = TStruct
instance IsTType TMap    where ttype _ = TMap
instance IsTType TSet    where ttype _ = TSet
instance IsTType TList   where ttype _ = TList

data SomeTType where
    SomeTType :: forall a. IsTType a => TType a -> SomeTType


fromCode :: Word8 -> Maybe SomeTType
fromCode 2  = Just $ SomeTType TBool
fromCode 3  = Just $ SomeTType TByte
fromCode 4  = Just $ SomeTType TDouble
fromCode 6  = Just $ SomeTType TInt16
fromCode 8  = Just $ SomeTType TInt32
fromCode 10 = Just $ SomeTType TInt64
fromCode 11 = Just $ SomeTType TBinary
fromCode 12 = Just $ SomeTType TStruct
fromCode 13 = Just $ SomeTType TMap
fromCode 14 = Just $ SomeTType TSet
fromCode 15 = Just $ SomeTType TList
fromCode _  = Nothing


toCode :: TType a -> Word8
toCode TBool   = 2
toCode TByte   = 3
toCode TDouble = 4
toCode TInt16  = 6
toCode TInt32  = 8
toCode TInt64  = 10
toCode TBinary = 11
toCode TStruct = 12
toCode TMap    = 13
toCode TSet    = 14
toCode TList   = 15


instance Hashable (TType a) where
    hashWithSalt s t = s `hashWithSalt` toCode t
