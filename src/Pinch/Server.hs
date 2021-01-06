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
  , onError
  , runConnection

  , ServiceName (..)
  , multiplex
  ) where

import           Control.Exception        (Exception, SomeException, throwIO,
                                           try, tryJust, catchJust)
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

mapRequest :: (Message -> Message) -> Request o -> Request o
mapRequest f (RCall m)   = RCall $ f m
mapRequest f (ROneway m) = ROneway $ f m

getRequestMessage :: Request o -> Message
getRequestMessage (RCall m)   = m
getRequestMessage (ROneway m) = m

-- | A `Thrift` server. Takes the context and the request message as input and produces a reply message.
newtype ThriftServer = ThriftServer { unThriftServer :: forall a . Context -> Request a -> IO a }

data ParseError = ParseError T.Text
  deriving (Show, Eq)
instance Exception ParseError

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

data Handler where
  CallHandler :: (Pinchable c, Tag c ~ TStruct, Pinchable r, Tag r ~ TStruct) => (Context -> c -> IO r) -> Handler
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
              pure $ msgAppEx msg $ ApplicationException ("Unable to parse service arguments: " <> T.pack err) InternalError

        Just (OnewayHandler _) ->
          pure $ msgAppEx msg $ ApplicationException "Expected message type Oneway, got Call." InvalidMessageType
        Nothing ->
          pure $ msgAppEx msg $ ApplicationException "Unknown method name." WrongMethodName

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

multiplex :: [(ServiceName, ThriftServer)] -> ThriftServer
multiplex services = ThriftServer $ \ctx req -> do
  case req of
    RCall msg   -> go ctx req (pure . msgAppEx msg)
    -- we cannot send the exception back, because it is a oneway call
    -- instead let's just throw it and crash the server
    ROneway msg -> go ctx req throwIO
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

          reply <- unThriftServer srv ctx' $ mapRequest (\msg -> msg { messageName = T.tail rem }) req

          case req of
            ROneway _ -> pure ()
            RCall _ -> pure reply
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
          RCall _ -> callError e
          ROneway _ -> onewayError e
      )

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
          -- no matter what happens, we can never send back an error because the client is not listening for replies
          -- when doing a oneway calls...
          -- Let's just crash the connection in this case, to avoid silently swallowing errors.
          -- `onError` can be used to handle this more gracefully.
          unThriftServer srv ctx (ROneway call)
        t -> writeMessage chan $ msgAppEx call $ ApplicationException ("Expected call, got " <> (T.pack $ show t)) InvalidMessageType
      runConnection ctx srv chan

msgAppEx :: Message -> ApplicationException -> Message
msgAppEx req ex = Message
  { messageName = messageName req
  , messageType = Exception
  , messageId = messageId req
  , messagePayload = pinch ex
  }
