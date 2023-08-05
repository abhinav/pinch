{-# LANGUAGE RankNTypes #-}

module Pinch.Transport
  ( Transport(..)
  , framedTransport
  , unframedTransport
  , Connection(..)
  , ReadResult(..)
  ) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll, recv)
import System.IO (Handle)

import qualified Data.ByteString as BS
import qualified Data.Serialize.Get as G

import qualified Pinch.Internal.Builder as B

class Connection c where
  -- | Gets up to n bytes. Returns an empty bytestring if EOF is reached.
  cGetSome :: c -> IO BS.ByteString
  -- | Writes the given bytestring.
  cPut :: c -> BS.ByteString -> IO ()

instance Connection Handle where
  cPut = BS.hPut
  cGetSome h = BS.hGetSome h 1024

instance Connection Socket where
  cPut = sendAll
  cGetSome s = recv s 4096

data ReadResult a
  = RRSuccess a
  | RRFailure String
  | RREOF
  deriving (Eq, Show)

-- | A bidirectional transport to read/write messages from/to.
data Transport
  = Transport
  { writeMessage :: B.Builder -> IO ()
  , readMessage  :: forall a . G.Get a -> IO (ReadResult a)
  }

-- | Creates a thrift framed transport. See also <https://github.com/apache/thrift/blob/master/doc/specs/thrift-rpc.md#framed-vs-unframed-transport>.
framedTransport :: Connection c => c -> IO Transport
framedTransport c = do
  readBuffer <- newIORef mempty
  pure $ Transport writeMsg (readMsg readBuffer) where
  writeMsg msg = do
    cPut c $ B.runBuilder $ B.int32BE (fromIntegral $ B.getSize msg)
    cPut c $ B.runBuilder msg

  readMsg readBuffer parser = do
    let 
      frameParser = do 
        size <- G.getInt32be
        G.isolate (fromIntegral size) parser
    
    initial <- readIORef readBuffer
    (leftovers, r) <- runGetWith (cGetSome c) frameParser initial
    writeIORef readBuffer $! leftovers
    pure r

-- | Creates a thrift unframed transport. See also <https://github.com/apache/thrift/blob/master/doc/specs/thrift-rpc.md#framed-vs-unframed-transport>.
unframedTransport :: Connection c => c -> IO Transport
unframedTransport c = do
  -- As we do not know how long messages are,
  -- we may read more data then the current message needs.
  -- We keep the leftovers in a buffer so that we may use them
  -- when reading the next message.
  readBuffer <- newIORef mempty
  pure $ Transport writeMsg (readMsg readBuffer)
  where
    writeMsg msg = cPut c $ B.runBuilder msg

    readMsg buf p = do
      bs <- readIORef buf
      bs' <- if BS.null bs then getSome else pure bs
      (leftOvers, r) <- runGetWith getSome p bs'
      writeIORef buf leftOvers
      pure $ r
    getSome = cGetSome c

-- | Runs a Get parser incrementally, reading more input as necessary until a successful parse
-- has been achieved.
runGetWith :: IO BS.ByteString -> G.Get a -> BS.ByteString -> IO (BS.ByteString, ReadResult a)
runGetWith getBs p initial = go (G.runGetPartial p initial)
  where
    go r = case r of
      G.Fail err bs -> do
        pure (bs, RRFailure err)
      G.Done a bs -> do
        pure (bs, RRSuccess a)
      G.Partial cont -> do
        bs <- getBs
        if BS.null bs
          then
            -- EOF
            pure (bs, RREOF)
          else
            go $ cont bs
