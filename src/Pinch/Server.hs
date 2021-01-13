{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module Pinch.Server
  (
    -- * Thrift Server creation

    ThriftServer (..)
  , createServer
  , Handler(..)
  , Request (..)

    -- * Running a Thrift Server
  , runConnection
  , ThriftError (..)
  , Channel (..)
  , createChannel
  , createChannel1

    -- * Thrift Server context

    -- | The context can be used to pass data from the environment to the thrift server functions.
    -- For example, you could pass the remote host name to the server to use it for logging purposes.
  , Context
  , ContextItem
  , addToContext
  , lookupInContext

    -- * Middlewares
  , multiplex
  , ServiceName (..)
  , onError

    -- * Helper functions

    -- | Functions mostly useful for defining custom `ThriftServer`s.
  , mapRequestMessage
  , getRequestMessage
  , mkApplicationExceptionReply
  ) where

import           Control.Exception        (Exception, SomeException, catchJust,
                                           fromException, throwIO, try)
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

-- | A single request to a thrift server.
data Request out where
  RCall :: !Message -> Request Message
  ROneway :: !Message -> Request ()

deriving instance Show (Request out)

-- | Map the message contained in the request.
mapRequestMessage :: (Message -> Message) -> Request o -> Request o
mapRequestMessage f (RCall m)   = RCall $ f m
mapRequestMessage f (ROneway m) = ROneway $ f m

-- | Extract the message contained in the request.
getRequestMessage :: Request o -> Message
getRequestMessage (RCall m)   = m
getRequestMessage (ROneway m) = m

-- | A `Thrift` server. Takes the context and the request as input and may produces a reply message.
newtype ThriftServer = ThriftServer { unThriftServer :: forall a . Context -> Request a -> IO a }

-- | Allows passing context information to a `ThriftServer`.
-- The context is indexed by type.
newtype Context = Context (HM.HashMap TypeRep Dynamic)

instance Semigroup Context where
  (Context a) <> (Context b) = Context $ a <> b

instance Monoid Context where
  mempty = Context mempty

class Typeable a => ContextItem a where

instance ContextItem ServiceName


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

-- | Create a handler for a request type.
data Handler where
  -- | Handle normal call requests. Must return a result.
  CallHandler :: (Pinchable c, Tag c ~ TStruct, Pinchable r, Tag r ~ TStruct) => (Context -> c -> IO r) -> Handler
  -- | Handle oneway requests. Cannot return any result.
  OnewayHandler :: (Pinchable c, Tag c ~ TStruct) => (Context -> c -> IO ()) -> Handler

-- | Creates a new thrift server processing requests with the function `f`.
--
-- By default, if processing a oneway call fails a haskell exception is thrown which will likely
-- terminate the guilty connection. You may use the `onError` combinator to handle this case
-- more gracefully.
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
              pure $ mkApplicationExceptionReply msg $
                ApplicationException ("Unable to parse service arguments: " <> T.pack err) InternalError

        Just (OnewayHandler _) ->
          pure $ mkApplicationExceptionReply msg $
            ApplicationException "Expected message type Oneway, got Call." InvalidMessageType
        Nothing ->
          pure $ mkApplicationExceptionReply msg $ ApplicationException "Unknown method name." WrongMethodName

    ROneway msg ->
      -- we cannot return errors to the client as it is a oneway call.
      -- Instead we just throw an exception, possible terminating
      -- the guilty connection.
      case f $ messageName msg of
        Just (OnewayHandler f) -> do
          case runParser $ unpinch $ messagePayload msg of
            Right args -> f ctx args
            Left err   ->
              throwIO $ ApplicationException ("Unable to parse service arguments: " <> T.pack err) InternalError
        Just (CallHandler _) ->
          throwIO $ ApplicationException "Expected message type Call, got Oneway." InvalidMessageType
        Nothing ->
          throwIO $ ApplicationException "Unknown method name." WrongMethodName

-- | Multiplex multiple services into a single `ThriftServer`.
--
-- The service name is added to the `Context` and may be retrieved using `lookupInContext @ServiceName ctx`.
multiplex :: [(ServiceName, ThriftServer)] -> ThriftServer
multiplex services = ThriftServer $ \ctx req -> do
  case req of
    RCall msg -> go ctx req (pure . mkApplicationExceptionReply msg)
    -- we cannot send the exception back, because it is a oneway call
    -- instead let's just throw it and crash the server
    ROneway _ -> go ctx req throwIO
  where
    srvMap = HM.fromList services

    go :: Context -> Request a -> (ApplicationException -> IO a) -> IO a
    go ctx req onError = do
      let (prefix, rem) = T.span (/= ':') (messageName $ getRequestMessage req)
      let prefix' = ServiceName prefix
      let ctx' = addToContext prefix' ctx
      case prefix' `HM.lookup` srvMap of
        _ | T.null rem -> onError $ ApplicationException "Invalid method name, expecting a dot." WrongMethodName
        Just srv -> do

          reply <- unThriftServer srv ctx' $ mapRequestMessage (\msg -> msg { messageName = T.tail rem }) req

          case req of
            ROneway _ -> pure ()
            RCall _   -> pure reply
        Nothing -> onError $ ApplicationException ("No service with name " <> prefix <> " available.") UnknownMethod

-- | Add error handlers to a `ThriftServer`. Exceptions are caught and not re-thrown, but you may do
-- so by calling `ioThrow` yourself.
onError
  :: Exception e
  => (e -> Maybe a) -- ^ Select exceptions to handle.
  -> (a -> IO Message) -- ^ Error handler for normal method calls.
  -> (a -> IO ()) -- ^ Error handler for oneway calls.
  -> ThriftServer -> ThriftServer
onError sel callError onewayError srv = ThriftServer $
  \ctx req ->
    catchJust sel
      (unThriftServer srv ctx req)
      (\e -> do
        case req of
          RCall _   -> callError e
          ROneway _ -> onewayError e
      )

-- | Run a Thrift server for a single connection.
runConnection :: Context -> ThriftServer -> Channel -> IO ()
runConnection ctx srv chan = do
  msg <- readMessage chan
  case msg of
    T.RREOF -> pure ()
    T.RRFailure err -> do
      throwIO $ ThriftError $ T.pack err
    T.RRSuccess call -> do
      case messageType call of
        Call -> do
          r <- try $ unThriftServer srv ctx (RCall call)
          case r of
            -- if it is already an ApplicationException, we just send it back
            Left (e :: SomeException)
              | Just appEx <- fromException e -> writeMessage chan $ mkApplicationExceptionReply call appEx
            Left (e :: SomeException) -> writeMessage chan $ mkApplicationExceptionReply call $
              ApplicationException ("Could not process request: " <> (T.pack $ show e)) InternalError
            Right x -> writeMessage chan x
        Oneway -> do
          -- no matter what happens, we can never send back an error because the client is not listening for replies
          -- when doing a oneway calls...
          -- Let's just crash the connection in this case, to avoid silently swallowing errors.
          -- `onError` can be used to handle this more gracefully.
          unThriftServer srv ctx (ROneway call)
        -- the client must never send Reply/Exception messages.
        t -> throwIO $
          ApplicationException ("Expected call, got " <> (T.pack $ show t)) InvalidMessageType
      runConnection ctx srv chan

-- | Builds an exception reply given the corresponding request message.
mkApplicationExceptionReply :: Message -> ApplicationException -> Message
mkApplicationExceptionReply req ex = Message
  { messageName = messageName req
  , messageType = Exception
  , messageId = messageId req
  , messagePayload = pinch ex
  }
