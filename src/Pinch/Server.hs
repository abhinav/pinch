{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module Pinch.Server
  ( ThriftServer(..)
  , ParseError (..)
  , Channel (..)
  , Context
  , ContextItem (..)
  , addToContext
  , lookupInContext

  , createServer
  , runConnection
  ) where


import           Control.Concurrent       (forkFinally)
import           Control.Exception        (Exception, SomeException, throwIO,
                                           try)
import           Control.Monad
import           Data.Dynamic             (Dynamic (..), fromDynamic, toDyn)
import           Data.Proxy               (Proxy (..))
import           Data.Typeable            (TypeRep, Typeable, typeOf, typeRep)

import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T

import           Pinch.Internal.Exception
import           Pinch.Internal.Message
import           Pinch.Internal.Pinchable
import           Pinch.Internal.RPC
import           Pinch.Internal.TType

import qualified Pinch.Protocol           as P
import qualified Pinch.Transport          as T

-- | A `Thrift` server. Takes the context and the request message as input and produces a reply message.
newtype ThriftServer = ThriftServer { unThriftServer :: Context -> Message -> IO Message }

data ParseError = ParseError T.Text
  deriving (Show, Eq)
instance Exception ParseError

-- | Allows passing context information to a `ThriftServer`.
-- The context is indexed by type.
newtype Context = Context (HM.HashMap TypeRep Dynamic)

class Typeable a => ContextItem a where

instance Semigroup Context where
  (Context a) <> (Context b) = Context $ a <> b

instance Monoid Context where
  mempty = Context mempty


-- | Adds a new item to the context. If an item with the same
-- type is already part of the context, it will be overwritten.
addToContext :: forall i . ContextItem i => i -> Context -> Context
addToContext i (Context m) =
  Context $ HM.insert (typeOf i) (toDyn i) m

-- | Lookup a value in the context.
lookupInContext :: forall i . ContextItem i => Context -> Maybe i
lookupInContext (Context m) = do
  x <- HM.lookup (typeRep (Proxy :: Proxy i)) m
  case fromDynamic @i x of
    Nothing -> error "Impossible!"
    Just y  -> pure y


-- | Creates a new thrift server processing requests with the function `f`.
createServer :: (Pinchable c, Pinchable r, Tag c ~ TStruct, Tag r ~ TStruct) => (Context -> T.Text -> c -> IO r) -> ThriftServer
createServer f = ThriftServer $ \ctx msg -> do
  case runParser $ unpinch $ messagePayload msg of
    Right args -> do
      ret <- f ctx (messageName msg) args
      pure $ Message
        { messageName = messageName msg
        , messageType = Reply
        , messageId   = messageId msg
        , messagePayload = pinch ret
        }
    Left err -> do
      pure $ msgAppEx msg $ ApplicationException ("Unable to parse service arguments: " <> T.pack err) InternalError

-- | Run a Thrift server for a single connection.
runConnection :: Context -> ThriftServer -> Channel -> IO ()
runConnection ctx srv chan = do
  msg <- T.readMessage (cTransportIn chan) $ P.deserializeMessage' (cProtocolIn chan)
  case msg of
    T.RREOF -> pure ()
    T.RRFailure err -> do
      throwIO $ ParseError $ T.pack err
    T.RRSuccess call -> do
      reply <- case messageType call of
        Call -> do
          r <- try $ unThriftServer srv ctx call
          case r of
            Left (e :: SomeException) -> pure $ msgAppEx call $
              ApplicationException ("Could not process request: " <> (T.pack $ show e)) InternalError
            Right x -> pure x
        t -> pure $ msgAppEx call $ ApplicationException ("Expected call, got " <> (T.pack $ show t)) InvalidMessageType
      T.writeMessage (cTransportOut chan) $ P.serializeMessage (cProtocolOut chan) reply
      runConnection ctx srv chan
  where

msgAppEx :: Message -> ApplicationException -> Message
msgAppEx req ex = Message
  { messageName = messageName req
  , messageType = Exception
  , messageId = messageId req
  , messagePayload = pinch ex
  }
