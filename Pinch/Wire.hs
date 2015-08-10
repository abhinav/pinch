{-# LANGUAGE RankNTypes #-}
module Pinch.Wire
    ( Protocol(..)
    ) where
    -- TODO move to internal, don't export everything

import Data.ByteString         (ByteString)
import Data.ByteString.Builder (Builder)

import Pinch.Internal.Message (Message)
import Pinch.Internal.TType   (TType)
import Pinch.Internal.Value   (Value)

data Protocol = Protocol
    { serializeValue   :: forall a. Value a -> Builder
    , serializeMessage :: forall a. Message a -> Builder

    , deserializeValue
        :: forall a. TType a -> ByteString -> Either String (Value a)
    , deserializeMessage
        :: forall a. TType a -> ByteString -> Either String (Message a)
    } -- TODO might be able to drop TType a argument
