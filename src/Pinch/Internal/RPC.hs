{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Pinch.Internal.RPC
  ( Channel(..)
  , createChannel
  , createChannel1
  ) where


import           Data.Hashable        (Hashable)
import           Data.Text            (Text)

import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T

import           Pinch.Internal.TType
import           Pinch.Internal.Value
import           Pinch.Protocol       (Protocol)
import           Pinch.Transport      (Connection, Transport)

-- | A bi-directional channel to read/write Thrift messages.
data Channel = Channel
  { cTransportIn  :: !Transport
  , cTransportOut :: !Transport
  , cProtocolIn   :: !Protocol
  , cProtocolOut  :: !Protocol
  }

-- | Creates a channel using the same transport/protocol for both directions.
createChannel :: Connection c => c -> (c -> IO Transport) -> Protocol -> IO Channel
createChannel c t p = do
  t' <- t c
  pure $ Channel t' t' p p

-- | Creates a channel.
createChannel1 :: (Transport, Protocol) -> (Transport, Protocol) -> Channel
createChannel1 (tIn, pIn) (tOut, pOut) = Channel tIn tOut pIn pOut
