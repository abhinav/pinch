{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Pinch.Internal.RPC
  ( Channel(..)
  , createChannel
  , createChannel1
  , readMessage
  , writeMessage

  , ReadResult(..)
  ) where

import           Pinch.Internal.Message
import           Pinch.Protocol       (Protocol, deserializeMessage', serializeMessage)
import           Pinch.Transport      (Connection, Transport, ReadResult(..))

import qualified Pinch.Transport      as Transport

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

readMessage :: Channel -> IO (ReadResult Message)
readMessage chan = Transport.readMessage (cTransportIn chan) $ deserializeMessage' (cProtocolIn chan)

writeMessage :: Channel -> Message -> IO ()
writeMessage chan msg = Transport.writeMessage (cTransportOut chan) $ serializeMessage (cProtocolOut chan) msg
