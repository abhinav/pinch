{-# LANGUAGE RankNTypes #-}
module Pinch.Protocol
    ( Protocol(..)
    ) where

import Data.ByteString         (ByteString)
import Data.ByteString.Builder (Builder)

import Pinch.Internal.Message (Message)
import Pinch.Internal.TType   (IsTType)
import Pinch.Internal.Value   (Value)

data Protocol = Protocol
    { serializeValue   :: forall a. Value a -> Builder
    , serializeMessage :: forall a. Message a -> Builder

    , deserializeValue
        :: forall a. IsTType a => ByteString -> Either String (Value a)
    , deserializeMessage
        :: forall a. IsTType a => ByteString -> Either String (Message a)
    }
