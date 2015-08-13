{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      :  Pinch.Internal.Message
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Message wrapper for Thrift payloads.
--
module Pinch.Internal.Message
    ( Message(..)
    , MessageType(..)
    ) where

import Data.Data     (Data)
import Data.Int      (Int32)
import Data.Text     (Text)
import Data.Typeable (Typeable)

import Pinch.Internal.Value (Value)

-- | Type of message being sent.
data MessageType
    = CallMessage
    -- ^ Call to a specific method.
    | ReplyMessage
    -- ^ Response of a call.
    | ExceptionMessage
    -- ^ Failed response of a call.
    | OnewayMessage
    -- ^ One-way call that expects no response.
  deriving (Show, Eq, Data, Typeable)


-- | Message envelope for Thrift payloads. This is parameterized over the
-- 'Pinch.TType.TType' of the 'Value' it contains.
data Message a = Message
    { messageName :: !Text
    -- ^ Name of the method to which this message is targeted.
    , messageType :: !MessageType
    -- ^ Type of the message.
    , messageId   :: !Int32
    -- ^ Sequence ID of the message. This is used to map responses to requests
    -- if the caller had multiple requests ongoing with the server.
    , messageBody :: !(Value a)
    -- ^ Contents of the message.
    }
  deriving (Show, Eq, Typeable)
