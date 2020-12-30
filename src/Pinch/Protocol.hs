{-# LANGUAGE RankNTypes #-}
-- |
-- Module      :  Pinch.Protocol
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Protocols in Pinch only need to know how to serialize and deserialize
-- 'Value' objects. Types that want to be serialized into/from Thrift payloads
-- define how to convert them to/from 'Value' objects via
-- 'Pinch.Pinchable.Pinchable'.
module Pinch.Protocol
    ( Protocol(..)
    , deserializeValue
    , deserializeMessage
    ) where

import Data.ByteString (ByteString)
import Data.Serialize.Get (Get, runGet)

import Pinch.Internal.Builder (Builder)
import Pinch.Internal.Message (Message)
import Pinch.Internal.TType   (IsTType)
import Pinch.Internal.Value   (Value)

-- | Protocols define a specific way to convert values into binary and back.
data Protocol = Protocol
    { serializeValue   :: forall a. IsTType a => Value a -> Builder
    -- ^ Serializes a 'Value' into a ByteString builder.
    --
    -- Returns a @Builder@ and the total length of the serialized content.
    , serializeMessage :: Message -> Builder
    -- ^ Serializes a 'Message' and its payload into a ByteString builder.
    --
    -- Returns a @Builder@ and the total length of the serialized content.

    , deserializeValue'
        :: forall a. IsTType a => Get (Value a)
    -- ^ Reads a 'Value' from a ByteString and returns leftovers from parse.
    , deserializeMessage' :: Get Message
    -- ^ Reads a 'Message' and its payload.
    }


-- | Reads a 'Value' from a ByteString.
deserializeValue :: forall a. IsTType a
                 => Protocol -> ByteString -> Either String (Value a)
deserializeValue proto = runGet $ deserializeValue' proto

-- | Reads a 'Message' and its payload from a ByteString.
deserializeMessage :: Protocol -> ByteString -> Either String Message
deserializeMessage proto = runGet $ deserializeMessage' proto
