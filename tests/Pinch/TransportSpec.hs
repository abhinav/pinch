{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pinch.TransportSpec (spec) where

import Data.ByteString       (ByteString)
import Data.IORef            (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Serialize.Get as G

import Pinch.Arbitrary (SomeByteString(..))
import Pinch.Transport ( Transport(..), framedTransport, unframedTransport, headerTransport
                       , Connection(..), ReadResult(..), emptyHeaderData, HeaderData(..))

import qualified Pinch.Internal.Builder as B
import Data.Char (ord)
import Data.ByteString.Builder as BB

data MemoryConnection = MemoryConnection
  { contents :: IORef ByteString
  , _maxChunkSize :: Int -- ^ how many bytes to maximally return for one cGetSome call
  }

newMemoryConnection :: Int -> IO MemoryConnection
newMemoryConnection ch = MemoryConnection <$> newIORef mempty <*> pure ch

mGetContents :: MemoryConnection -> IO ByteString
mGetContents = readIORef . contents

instance Connection MemoryConnection where
  cGetSome (MemoryConnection ref ch) = do
    bytes <- readIORef ref
    let (left, right) = BS.splitAt ch bytes
    writeIORef ref right
    return left
  cPut (MemoryConnection ref _) builder = do
    modifyIORef ref (<> B.runBuilder builder)

eofSpec :: (forall c . Connection c => c -> IO Transport) -> Spec
eofSpec t = it "EOF handling" $ do
  buf <- newMemoryConnection 10
  transp <- t buf
  r <- readMessage transp (G.getInt8)
  r `shouldBe` RREOF

transportSpec :: (forall c . Connection c => c -> IO Transport) -> Spec
transportSpec t = do
  prop "can roundtrip bytestrings" $ \(Positive c, SomeByteString bytes) ->
    ioProperty $ do
      buf <- newMemoryConnection c
      transp <- t buf
      writeMessage transp emptyHeaderData (B.byteString bytes)
      actual <- readMessage transp (G.getBytes $ BS.length bytes)
      pure $ actual === RRSuccess (bytes, emptyHeaderData)

  eofSpec t

spec :: Spec
spec = do
  describe "framedTransport" $ do
    transportSpec framedTransport

    it "read case" $ do
      let payload = BS.pack [0x01, 0x05, 0x01, 0x08, 0xFF]
      buf <- newMemoryConnection 1
      transp <- framedTransport buf
      cPut buf $ B.byteString (BS.pack [0x00, 0x00, 0x00, 0x05])
      cPut buf $ B.byteString payload
      r <- readMessage transp (G.getBytes $ BS.length payload)
      r `shouldBe` RRSuccess (payload, emptyHeaderData)

    it "write case" $ do
      let payload = BS.pack [0x01, 0x05, 0x01, 0x08, 0xFF]
      buf <- newMemoryConnection 1
      transp <- framedTransport buf
      writeMessage transp emptyHeaderData (B.byteString payload)
      actual <- mGetContents buf
      actual `shouldBe` (BS.pack [0x00, 0x00, 0x00, 0x05] <> payload)


  describe "unframedTransport" $ do
    transportSpec unframedTransport

    prop "read cases" $ \(SomeByteString payload) ->
      ioProperty $ do
        buf <- newMemoryConnection 1
        transp <- unframedTransport buf
        cPut buf $ B.byteString payload
        r <- readMessage transp (G.getBytes $ BS.length payload)
        pure $ r === RRSuccess (payload, emptyHeaderData)

    prop "write cases" $ \(SomeByteString payload) ->
      ioProperty $ do
        buf <- newMemoryConnection 1
        transp <- unframedTransport buf
        writeMessage transp emptyHeaderData (B.byteString payload)
        actual <- mGetContents buf
        pure $ actual === payload

  describe "headerTransport" $ do
    eofSpec headerTransport

    describe "framed egress" $ do
      it "read case" $ do
        let payload =
              [ [ 0x80, 0x01, 0x00, 0x00 ]     -- binary protocol version 1
              , [ 0x01, 0x05, 0x01, 0x08, 0xFF ] -- data
              ]
        let payloadBS = BS.pack $ concat payload
        buf <- newMemoryConnection 1
        transp <- headerTransport buf
        cPut buf $ B.byteString $ BS.pack $
          [ 0x00, 0x00, 0x00, 0x09 ] -- size
          ++ concat payload
        cPut buf $ B.byteString payloadBS
        r <- readMessage transp (G.getBytes $ BS.length payloadBS)
        r `shouldBe` RRSuccess (payloadBS, emptyHeaderData)

    describe "header egress" $ do
      it "read case" $ do
        let payload = [0x01, 0x05, 0x01, 0x08, 0xFF]
        let payloadBS = BS.pack payload
        buf <- newMemoryConnection 1
        transp <- headerTransport buf
        let packet = testHeaderBS <> payloadBS
        cPut buf $ B.byteString packet
        r <- readMessage transp (G.getBytes $ length payload)
        r `shouldBe` RRSuccess (payloadBS, testHeader)

      it "write case" $ do
        let payload = [0x01, 0x05, 0x01, 0x08, 0xFF]
        let payloadBS = BS.pack payload
        buf <- newMemoryConnection 1
        transp <- headerTransport buf
        writeMessage transp testHeader (B.byteString payloadBS)
        actual <- mGetContents buf
        toHex actual `shouldBe` toHex (testHeaderBS <> payloadBS)

toHex :: BS.ByteString -> BSL.ByteString
toHex = BB.toLazyByteString . ("0x" <>) . byteStringHex

testHeaderBS :: ByteString
testHeaderBS = BS.pack $ concat
  [ [0x00, 0x00, 0x00, 0x1F]       --  0
  , [0x0f, 0xff]                   --  4 -- header magic
  , [0x12, 0x34]                   --  6 -- flags
  , [0x00, 0x00, 0x00, 0x42]       --  8 -- sequence number
  , [0x00, 0x04]                   -- 12 -- header size / 4
  , [0x80]                         -- 14 -- protocol id (0 as varint)
  , [0x80]                         -- 15 -- num transforms (0 as varint)
  , [0x81]                         -- 16 -- INFO_KEYVALUE number of headers
  , [0x81]                         -- 17 -- number of info entries
  , [0x84]                         -- 18 -- length of key
  , fromIntegral . ord <$> "test"  -- 19
  , [0x83]                         -- 23 -- length of value
  , fromIntegral . ord <$> "123"   -- 24
  , [0x00, 0x00, 0x00]             -- 27 -- padding
                                   -- 30
  ]

testHeader :: HeaderData
testHeader = HeaderData
  { hSequenceNumber = 0x42
  , hInfo = [("test", "123")]
  , hFlags = 0x1234
  }
