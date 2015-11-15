{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- |
-- Module      :  Pinch.Internal.Message
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Message wrapper for Thrift payloads. Normal Thrift requests sent over the
-- wire are wrapped inside a message envelope that contains information about
-- the method being called, the type of message, etc. This information is
-- essential for the RPC system to function.
module Pinch.Internal.Message
    ( Message(..)
    , MessageType(..)
    ) where

import Control.DeepSeq (NFData)
import Data.Data       (Data)
import Data.Int        (Int32)
import Data.Text       (Text)
import Data.Typeable   (Typeable)
import GHC.Generics    (Generic)

import Pinch.Internal.TType (TStruct)
import Pinch.Internal.Value (Value)

-- | Type of message being sent.
data MessageType
    = CallMessage
    -- ^ A call to a specific method.
    --
    -- The message body is the request arguments struct.
    | ReplyMessage
    -- ^ Response to a call.
    --
    -- The message body is the response union.
    | ExceptionMessage
    -- ^ Failure to make a call.
    --
    -- Note: This message type is /not/ used for exceptions that are defined
    -- under the @throws@ clause of a method. Those exceptions are part of the
    -- response union of the method and are received in a @ReplyMessage@. This
    -- message type is used to Thrift-level failures.
    | OnewayMessage
    -- ^ One-way call that expects no response.
  deriving (Show, Eq, Data, Typeable, Generic)

instance NFData MessageType


-- | Message envelope for Thrift payloads.
data Message = Message
    { messageName    :: !Text
    -- ^ Name of the method to which this message is targeted.
    , messageType    :: !MessageType
    -- ^ Type of the message.
    , messageId      :: !Int32
    -- ^ Sequence ID of the message.
    --
    -- If the clients expect to receive out-of-order responses, they may use
    -- the message ID to map responses back to their corresponding requests.
    -- If the client does not expect out-of-order responses, they are free to
    -- use the same message ID for all messages.
    --
    -- The server's contract regarding message IDs is that all responses must
    -- have the same message ID as their corresponding requests.
    , messagePayload :: !(Value TStruct)
    -- ^ Contents of the message.
    }
  deriving (Show, Eq, Typeable, Generic)

instance NFData Message
