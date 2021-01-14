{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Pinch.Client
  (
    -- * Basic Thrift client
    Client
  , client
  , Channel
  , createChannel
  , createChannel1

  , ThriftCall(..)
  , ThriftClient(..)
  , callOrThrow

    -- * Multiplexing Client
  , MultiplexClient
  , multiplexClient


    -- * Errors
  , ApplicationException(..)
  , ExceptionType(..)
  , ThriftError(..)
  ) where

import           Control.Exception        (throwIO)

import qualified Data.Text                as T

import           Pinch.Internal.Exception
import           Pinch.Internal.Message
import           Pinch.Internal.Pinchable
import           Pinch.Internal.RPC
import           Pinch.Internal.TType

-- | A simple Thrift Client.
newtype Client = Client Channel

-- | Instantiates a new Thrift client.
client :: Channel -> Client
client = Client

-- | A call to a Thrift server resulting in the return datatype `a`.
data ThriftCall a where
  TCall :: (Pinchable req, Tag req ~ TStruct, Pinchable res, Tag res ~ TStruct)
    => !T.Text -> !req -> ThriftCall res
  TOneway :: (Pinchable req, Tag req ~ TStruct) => !T.Text -> !req -> ThriftCall ()

class ThriftClient c where
  -- | Calls a Thrift service and returns the result/error data structure.
  -- Application-level exceptions defined in the thrift service are returned
  -- as part of the result/error data structure.
  call :: c -> ThriftCall a -> IO a

instance ThriftClient Client where
  call (Client chan) tcall = do
    case tcall of
      TOneway m r -> do
        writeMessage chan $ Message m Oneway 0 (pinch r)
        pure ()
      TCall m r -> do
        writeMessage chan $ Message m Call 0 (pinch r)
        reply <- readMessage chan
        case reply of
          RREOF -> throwIO $ ThriftError $ "Reached EOF while awaiting reply"
          RRFailure err -> throwIO $ ThriftError $ "Could not read message: " <> T.pack err
          RRSuccess reply' -> case messageType reply' of
            Reply -> case runParser $ unpinch $ messagePayload reply' of
              Right x -> pure x
              Left err -> do
                throwIO $ ThriftError $ "Could not parse reply payload: " <> T.pack err
            Exception -> case runParser $ unpinch $ messagePayload reply' of
              Right (x :: ApplicationException) -> throwIO x
              Left err ->
                throwIO $ ThriftError $ "Could not parse application exception: " <> T.pack err
            t -> throwIO $ ThriftError $ "Expected reply or exception, got " <> T.pack (show t) <> "."

-- | Calls a Thrift service. If an application-level thrift exception as defined in the Thrift service definition
-- is returned by the server, it will be re-thrown using `throwIO`.
callOrThrow :: (ThriftClient c, ThriftResult a) => c -> ThriftCall a -> IO (ResultType a)
callOrThrow client' c = call client' c >>= unwrap

-- | A multiplexing thrift client.
data MultiplexClient = forall c . ThriftClient c => MultiplexClient c ServiceName

-- | Create a new multiplexing thrift client targeting the given service.
multiplexClient :: ThriftClient c => c -> ServiceName -> MultiplexClient
multiplexClient = MultiplexClient

instance ThriftClient MultiplexClient where
  call (MultiplexClient client' (ServiceName serviceName)) tcall = case tcall of
    TOneway r req -> call client' $ TOneway (serviceName <> ":" <> r) req
    TCall r req   -> call client' $ TCall (serviceName <> ":" <> r) req
