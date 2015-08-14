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
    ) where

import Data.ByteString         (ByteString)
import Data.ByteString.Builder (Builder)

import Pinch.Internal.Message (Message)
import Pinch.Internal.TType   (IsTType)
import Pinch.Internal.Value   (Value)

-- | Defines a grouping of functions that together compose a Thrift protocol.
data Protocol = Protocol
    { serializeValue   :: forall a. IsTType a => Value a -> Builder
    -- ^ Serializes a 'Value' into a ByteString builder.
    , serializeMessage :: forall a. IsTType a => Message a -> Builder
    -- ^ Serializes a 'Message' and its payload into a ByteString builder.

    , deserializeValue
        :: forall a. IsTType a => ByteString -> Either String (Value a)
    -- ^ Reads a 'Value' from a ByteString.
    , deserializeMessage
        :: forall a. IsTType a => ByteString -> Either String (Message a)
    -- ^ Reads a 'Message' and its payload from a ByteString.
    }
