{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  , call
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
import           Pinch.Transport (HeaderData, emptyHeaderData)

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
    => !T.Text -> HeaderData -> !req -> ThriftCall res
  TOneway :: (Pinchable req, Tag req ~ TStruct) => !T.Text -> HeaderData -> !req -> ThriftCall ()

class ThriftClient c where
  -- | Calls a Thrift service and returns the result/error data structure.
  -- Application-level exceptions defined in the thrift service are returned
  -- as part of the result/error data structure.
  callWithHeaders :: c -> ThriftCall a -> IO (a, HeaderData)

instance ThriftClient Client where
  callWithHeaders (Client chan) tcall = do
    case tcall of
      TOneway m reqHeaders r -> do
        writeMessage chan reqHeaders $ Message m Oneway 0 (pinch r)
        pure ((), emptyHeaderData)
      TCall m reqHeaders r -> do
        writeMessage chan reqHeaders $ Message m Call 0 (pinch r)
        reply <- readMessage chan
        case reply of
          RREOF -> throwIO $ ThriftError $ "Reached EOF while awaiting reply"
          RRFailure err -> throwIO $ ThriftError $ "Could not read message: " <> T.pack err
          RRSuccess (reply', responseHeaders) -> case messageType reply' of
            Reply -> case runParser $ unpinch $ messagePayload reply' of
              Right x -> pure (x, responseHeaders)
              Left err -> do
                throwIO $ ThriftError $ "Could not parse reply payload: " <> T.pack err
            Exception -> case runParser $ unpinch $ messagePayload reply' of
              Right (x :: ApplicationException) -> throwIO x
              Left err ->
                throwIO $ ThriftError $ "Could not parse application exception: " <> T.pack err
            t -> throwIO $ ThriftError $ "Expected reply or exception, got " <> T.pack (show t) <> "."

call :: ThriftClient c => c -> ThriftCall a -> IO a
call c call' = fst <$> callWithHeaders c call'

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
  callWithHeaders (MultiplexClient client' (ServiceName serviceName)) tcall = case tcall of
    TOneway r reqHeaders req -> callWithHeaders client' $ TOneway (serviceName <> ":" <> r) reqHeaders req
    TCall r reqHeaders req   -> callWithHeaders client' $ TCall (serviceName <> ":" <> r) reqHeaders req
