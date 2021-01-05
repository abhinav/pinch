{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module Pinch.Server
  ( ThriftServer (..)
  , Request (..)
  , ParseError (..)
  , Channel (..)
  , Context
  , ContextItem
  , addToContext
  , lookupInContext

  , createServer
  , Handler(..)
  , runConnection
  ) where

import           Control.Exception        (Exception, SomeException, throwIO,
                                           try, tryJust)
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

import qualified Pinch.Transport          as T

data Request out where
  RCall :: !Message -> Request Message
  ROneway :: !Message -> Request ()

-- | A `Thrift` server. Takes the context and the request message as input and produces a reply message.
newtype ThriftServer = ThriftServer { unThriftServer :: forall a . Context -> Request a -> IO a }

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

data Handler where
  CallHandler :: (Pinchable c, Tag c ~ TStruct, Pinchable r, Tag r ~ TStruct) => (Context -> c -> IO r) -> Handler
  OnewayHandler :: (Pinchable c, Tag c ~ TStruct) => (Context -> c -> IO ()) -> Handler

-- | Creates a new thrift server processing requests with the function `f`.
createServer :: (T.Text -> Maybe Handler) -> ThriftServer
createServer f = ThriftServer $ \ctx req ->
  case req of
    RCall msg ->
      case f $ messageName msg of
        Just (CallHandler f) ->
          case runParser $ unpinch $ messagePayload msg of
            Right args -> do
              ret <- f ctx args
              pure $ Message
                  { messageName = messageName msg
                  , messageType = Reply
                  , messageId   = messageId msg
                  , messagePayload = pinch ret
                  }
            Left err ->
              pure $ msgAppEx msg $ ApplicationException ("Unable to parse service arguments: " <> T.pack err) InternalError

        Just (OnewayHandler _) ->
          pure $ msgAppEx msg $ ApplicationException "Expected message type Call, got Oneway." InvalidMessageType
        Nothing ->
          pure $ msgAppEx msg $ ApplicationException "Unknown method name." WrongMethodName

    ROneway msg ->
      case f $ messageName msg of
        Just (OnewayHandler f) -> do
          case runParser $ unpinch $ messagePayload msg of
            Right args -> f ctx args
            Left _     -> pure ()
        _ -> pure ()


-- | Run a Thrift server for a single connection.
runConnection :: Context -> ThriftServer -> Channel -> IO ()
runConnection ctx srv chan = do
  msg <- readMessage chan
  case msg of
    T.RREOF -> pure ()
    T.RRFailure err -> do
      throwIO $ ParseError $ T.pack err
    T.RRSuccess call -> do
      case messageType call of
        Call -> do
          r <- try $ unThriftServer srv ctx (RCall call)
          case r of
            Left (e :: SomeException) -> writeMessage chan $ msgAppEx call $
              ApplicationException ("Could not process request: " <> (T.pack $ show e)) InternalError
            Right x -> writeMessage chan x
        Oneway -> do
          _ <- tryJust (\(_ :: SomeException) -> Just ()) $ unThriftServer srv ctx (ROneway call)
          -- no matter what happens, we can never send back an error
          -- because the client is not listening for oneway calls...
          -- If you want to log this, you should use a custom Thrift server to intercept
          -- all messages.
          pure ()
        t -> writeMessage chan $ msgAppEx call $ ApplicationException ("Expected call, got " <> (T.pack $ show t)) InvalidMessageType
      runConnection ctx srv chan

msgAppEx :: Message -> ApplicationException -> Message
msgAppEx req ex = Message
  { messageName = messageName req
  , messageType = Exception
  , messageId = messageId req
  , messagePayload = pinch ex
  }
