{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pinch.ClientServerSpec (spec) where

import           Control.Concurrent       (forkFinally)
import           Control.Concurrent.Async (withAsync)
import           Control.Concurrent.MVar  (MVar, newEmptyMVar, putMVar,
                                           takeMVar)
import           Control.Exception        (bracketOnError, finally, throwIO)
import           Control.Monad            (forever, void)
import           Data.Int
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           Network.Run.TCP          (runTCPClient)
import           Network.Socket           as S
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified Data.Text                as T

import           Pinch
import           Pinch.Arbitrary          ()
import           Pinch.Client             hiding (client)
import qualified Pinch.Client             (client)
import           Pinch.Server
import           Pinch.Transport

echoServer :: ThriftServer
echoServer = createServer $ \_ -> Just $ CallHandler $ \_ (r :: Value TStruct) -> do
  pure r

data CalcRequest = CalcRequest
  { _inp1 :: Field 1 Int32
  , _inp2 :: Field 2 Int32
  , _op   :: Field 3 Op
  } deriving (Generic, Show)
instance Pinchable CalcRequest

data Op = Plus (Enumeration 1) | Minus (Enumeration 2) | Div (Enumeration 3)
  deriving (Generic, Show)
instance Pinchable Op

data CalcResult = CalcResult
  { result   :: Field 1 (Maybe Int32)
  , errorMsg :: Field 2 (Maybe Text)
  } deriving (Generic, Show, Eq)
instance Pinchable CalcResult

calcServer :: ThriftServer
calcServer = createServer $ \_ -> Just $ CallHandler $ \_ (CalcRequest (Field inp1) (Field inp2) (Field op)) -> do
  let ret = case op of
        Plus _ -> CalcResult (Field $ Just $ inp1 + inp2) (Field Nothing)
        Minus _ -> CalcResult (Field $ Just $ inp1 - inp2) (Field Nothing)
        Div _ | inp2 == 0 -> CalcResult (Field Nothing) (Field $ Just "div by zero")
        Div _ -> CalcResult (Field $ Just $ inp1 `div` inp2) (Field Nothing)
  pure ret

onewayServer :: IO (ThriftServer, MVar (Value TStruct))
onewayServer = do
  ref <- newEmptyMVar
  let srv = createServer $ \_ -> Just $ OnewayHandler $ \_ (r :: Value TStruct) -> putMVar ref r
  pure (srv, ref)

errorServer :: ThriftServer
errorServer = createServer $ \nm -> case nm of
  "app_ex" -> Just $ CallHandler $ \_ (_ :: Value TStruct) ->
    throwIO $ ApplicationException "Test" InternalError :: IO (Value TStruct)
  "hs_ex" -> Just $ CallHandler $ \_ (_ :: Value TStruct) -> error "nononono" :: IO (Value TStruct)
  _ -> Nothing

spec :: Spec
spec = do
  describe "Client/Server" $ do
    prop "echo test" $ withMaxSuccess 10 $ \(request :: Value TStruct) -> ioProperty $
      withLoopbackServer echoServer $ \client -> do
        reply <- call client $ TCall "" request
        pure $
          reply === request

    it "calculator" $ do
      withLoopbackServer calcServer $ \client -> do
        r1 <- call client $ mkCall 10 20 Plus
        r1 `shouldBe` CalcResult (Field $ Just 30) (Field Nothing)

        r2 <- call client $ mkCall 10 20 Minus
        r2 `shouldBe` CalcResult (Field $ Just $ -10) (Field Nothing)

        r3 <- call client $ mkCall 20 10 Div
        r3 `shouldBe` CalcResult (Field $ Just 2) (Field Nothing)

        r4 <- call client $ mkCall 10 0 Div
        r4 `shouldBe` CalcResult (Field Nothing) (Field $ Just "div by zero")

    it "oneway" $ do
      (srv, ref) <- onewayServer
      withLoopbackServer srv $ \client -> do
        let val = struct [1 .= True, 2 .= ("Hello" :: Text)]
        _ <- call client $ TOneway "test" val
        r1 <- takeMVar ref
        r1 `shouldBe` val

        _ <- call client (TCall "test" val :: ThriftCall (Value TStruct)) `shouldThrow` \e ->
          case e of
            ApplicationException _ ty -> ty == InvalidMessageType
        pure ()

    it "multiplex" $ do
      let srv = multiplex [("calc", calcServer), ("echo", echoServer)]
      withLoopbackServer srv $ \client -> do
        r1 <- call (multiplexClient client "calc") $ mkCall 10 20 Plus
        r1 `shouldBe` CalcResult (Field $ Just 30) (Field Nothing)

        let payload = struct [1 .= True, 2 .= ("Hello" :: Text)]
        r2 <- call (multiplexClient client "echo") $ TCall "" payload
        r2 `shouldBe` payload

    it "exceptions" $ do
      withLoopbackServer errorServer $ \client -> do
        let val = struct [1 .= True, 2 .= ("Hello" :: Text)]
        _ <- call client (TCall "app_ex" val :: ThriftCall (Value TStruct)) `shouldThrow` \e ->
          case e of
            ApplicationException msg ty -> msg == "Test" && ty == InternalError

        _ <- call client (TCall "hs_ex" val :: ThriftCall (Value TStruct)) `shouldThrow` \e ->
          case e of
            ApplicationException msg ty -> "nonono" `T.isInfixOf` msg && ty == InternalError

        _ <- call client (TCall "missing" val :: ThriftCall (Value TStruct)) `shouldThrow` \e ->
          case e of
            ApplicationException _ ty -> ty == WrongMethodName
        pure ()

  where
    mkCall inp1 inp2 op = TCall "calc" $ pinch $ CalcRequest (Field inp1) (Field inp2) (Field $ op $ Enumeration)


withLoopbackServer :: ThriftServer -> (Client -> IO a) -> IO a
withLoopbackServer srv cont = do
    addr <- resolve Stream (Just "127.0.0.1") "54093" True
    bracketOnError (open addr) close (\sock ->
      withAsync (loop sock `finally` close sock) $ \_ ->
        runTCPClient "127.0.0.1" "54093" $ \s -> do
          c <- Pinch.Client.client <$> createChannel s framedTransport binaryProtocol
          cont c
      )
  where
    open addr = bracketOnError (openServerSocket addr) close $ \sock -> do
        listen sock 1024
        return sock
    loop sock = forever $ bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $ forkFinally (runServer conn) (const $ gracefulClose conn 5000)
    runServer sock = do
      createChannel sock framedTransport binaryProtocol
        >>= runConnection mempty srv

    resolve :: SocketType -> Maybe HostName -> S.ServiceName -> Bool -> IO AddrInfo
    resolve socketType mhost port passive = 
        getAddrInfo (Just hints) mhost (Just port) >>= \case
          -- Data.List.head emits a warning in GHC 9.8+. Therefore, we're being more explicit
          -- about possible error cases.
          [addr] -> pure addr
          _      -> error "No addresses"
      where
        hints = defaultHints
          { addrSocketType = socketType
          , addrFlags = if passive then [AI_PASSIVE] else []
          }
    openServerSocket :: AddrInfo -> IO Socket
    openServerSocket addr = bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock $ setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      return sock
