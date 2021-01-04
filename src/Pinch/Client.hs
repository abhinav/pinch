{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Pinch.Client
  ( Client
  , ThriftCall(..)
  , ThriftError(..)
  , call
  , simpleClient
  ) where

import           Control.Exception        (Exception, throwIO)

import qualified Data.Text                as T

import           Pinch.Internal.Exception
import           Pinch.Internal.Message
import           Pinch.Internal.Pinchable
import           Pinch.Internal.RPC
import           Pinch.Internal.TType
import           Pinch.Internal.Value
import           Pinch.Protocol
import           Pinch.Transport

-- | A simple Thrift Client.
newtype Client = Client Channel

-- | A call to a Thrift server resulting in the return datatype `a`.
data ThriftCall a where
  TCall :: (Pinchable a, Tag a ~ TStruct) => !T.Text -> !(Value TStruct) -> ThriftCall a
  TOneway :: !T.Text -> !(Value TStruct) -> ThriftCall ()

-- | Calls a Thrift service and returns the result/error data structure.
-- Application-level exceptions defined in the thrift service are returned
-- as part of the result/error data structure.
call :: Client -> ThriftCall a -> IO a
call (Client (Channel tIn tOut pIn pOut)) tcall = do
  case tcall of
    TOneway m r -> do
      writeMessage tOut $ serializeMessage pOut $ Message m Oneway 0 r
      pure ()
    TCall m r -> do
      writeMessage tOut $ serializeMessage pOut $ Message m Call 0 r
      reply <- readMessage tIn $ deserializeMessage' pIn
      case reply of
        RREOF -> throwIO $ ThriftError $ "Reached EOF while awaiting reply"
        RRFailure err -> throwIO $ ThriftError $ "Could not read message: " <> T.pack err
        RRSuccess reply -> case messageType reply of
          Reply -> case runParser $ unpinch $ messagePayload reply of
            Right x -> pure x
            Left err -> do
              throwIO $ ThriftError $ "Could not parse reply payload: " <> T.pack err
          Exception -> case runParser $ unpinch $ messagePayload reply of
            Right (x :: ApplicationException) -> throwIO x
            Left err ->
              throwIO $ ThriftError $ "Could not parse application exception: " <> T.pack err
          t -> throwIO $ ThriftError $ "Expected reply or exception, got " <> T.pack (show t) <> "."

-- | Instantiates a new Thrift client.
simpleClient :: Channel -> Client
simpleClient = Client
