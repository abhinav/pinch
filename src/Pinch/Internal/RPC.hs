{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module Pinch.Internal.RPC
  ( Channel(..)
  , createChannel
  , createChannel1
  , readMessage
  , writeMessage
  , wrap

  , ReadResult(..)

  , ServiceName(..)
  , ThriftResult(..)

  , Unit(..)
  ) where

import           Data.Hashable            (Hashable (..))
import           Data.String              (IsString (..))
import           Data.Typeable            (Typeable)

import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T

import           Pinch.Internal.Message
import           Pinch.Internal.Pinchable (Pinchable (..), Tag)
import           Pinch.Internal.TType     (TStruct)
import           Pinch.Internal.Value     (Value (..))
import           Pinch.Protocol           (Protocol, deserializeMessage',
                                           serializeMessage)
import           Pinch.Transport          (Connection, ReadResult (..),
                                           Transport)

import qualified Pinch.Transport          as Transport

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

readMessage :: Channel -> IO (ReadResult (Message, Transport.HeaderData))
readMessage chan = Transport.readMessage (cTransportIn chan) $ deserializeMessage' (cProtocolIn chan)

writeMessage :: Channel -> Transport.HeaderData -> Message -> IO ()
writeMessage chan headers msg = Transport.writeMessage (cTransportOut chan) headers $ serializeMessage (cProtocolOut chan) msg


newtype ServiceName = ServiceName T.Text
  deriving (Typeable, Eq, Hashable)

instance IsString ServiceName where
  fromString = ServiceName . T.pack


-- | The Result datatype for a Thrift Service Method.
class (Pinchable a, Tag a ~ TStruct) => ThriftResult a where
  -- | The Haskell type returned when the Thrift call succeeds.
  type ResultType a

  -- | Tries to extract the result from a Thrift call. If the call threw any
  -- of the Thrift exceptions declared for this Thrift service method,
  -- the corresponding Haskell excpetions is thrown using `throwIO`.
  unwrap :: a -> IO (ResultType a)

  -- `wrapThrow` and `wrapPure` are the decomposition of `wrap` into a catch
  -- phase and a succeed phase.
  wrapThrown :: IO b -> IO (Either a b)
  wrapPure :: ResultType a -> a

-- | Runs the given computation. If it throws any of the exceptions
-- declared in the Thrift service definition, it is caught and converted
-- to the corresponding Haskell result datatype constructor.
wrap :: ThriftResult a => IO (ResultType a) -> IO a
wrap act = flip fmap (wrapThrown act) $ \case
  Left err -> err
  Right a -> wrapPure a

-- | Result datatype for void methods not throwing any exceptions.
data Unit = Unit

instance Pinchable Unit where
  type Tag Unit = TStruct
  pinch Unit = VStruct mempty
  unpinch (VStruct xs) | HM.null xs = pure Unit
  unpinch x            = fail $ "Failed to read void success. Got " ++ show x

instance ThriftResult Unit where
  type ResultType Unit = ()
  wrapThrown m = Right <$> m
  wrapPure () = Unit
  unwrap Unit = pure ()
