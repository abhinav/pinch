{-# LANGUAGE RankNTypes #-}
module Pinch.Wire where

import Data.ByteString         (ByteString)
import Data.ByteString.Builder (Builder)

import Pinch.Internal.TType (TType)
import Pinch.Internal.Value (Value)

data Protocol = Protocol
    { serialize   :: forall a. Value a -> Builder
    , deserialize :: forall a. TType a -> ByteString -> Either String (Value a)
    }
