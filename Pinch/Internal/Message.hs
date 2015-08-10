{-# LANGUAGE DeriveDataTypeable #-}
module Pinch.Internal.Message
    ( Message(..)
    , MessageType(..)
    , messageCode
    , fromMessageCode
    ) where

import Data.Data     (Data)
import Data.Int      (Int32)
import Data.Text     (Text)
import Data.Typeable (Typeable)
import Data.Word     (Word8)

import Pinch.Internal.Value (Value)

data MessageType
    = CallMessage      -- ^ 1
    | ReplyMessage     -- ^ 2
    | ExceptionMessage -- ^ 3
    | OnewayMessage    -- ^ 4
  deriving (Show, Eq, Data, Typeable)

messageCode :: MessageType -> Word8
messageCode CallMessage      = 1
messageCode ReplyMessage     = 2
messageCode ExceptionMessage = 3
messageCode OnewayMessage    = 4

fromMessageCode :: Word8 -> Maybe MessageType
fromMessageCode 1 = Just CallMessage
fromMessageCode 2 = Just ReplyMessage
fromMessageCode 3 = Just ExceptionMessage
fromMessageCode 4 = Just OnewayMessage
fromMessageCode _ = Nothing

data Message a = Message
    { messageName :: !Text
    , messageType :: !MessageType
    , messageId   :: !Int32
    , messageBody :: !(Value a)
    }
  deriving (Show, Eq, Typeable)
