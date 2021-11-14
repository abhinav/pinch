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
import qualified Data.Serialize.Get as G

import Pinch.Arbitrary (SomeByteString(..))
import Pinch.Transport (Transport(..), framedTransport, unframedTransport, Connection(..), ReadResult(..))

import qualified Pinch.Internal.Builder as B

data MemoryConnection = MemoryConnection
  { contents :: IORef ByteString
  , _maxChunkSize :: Int -- ^ how many bytes to maximally return for one cGetSome call
  }

newMemoryConnection :: Int -> IO MemoryConnection
newMemoryConnection ch = MemoryConnection <$> newIORef mempty <*> pure ch

mGetContents :: MemoryConnection -> IO ByteString
mGetContents = readIORef . contents

instance Connection MemoryConnection where
  cGetSome (MemoryConnection ref ch) n = do
    bytes <- readIORef ref
    let (left, right) = BS.splitAt (min ch n) bytes
    writeIORef ref right
    return left
  cPut (MemoryConnection ref _) newBytes = do
    modifyIORef ref (<> newBytes)

transportSpec :: (forall c . Connection c => c -> IO Transport) -> Spec
transportSpec t = do
  prop "can roundtrip bytestrings" $ \(Positive c, SomeByteString bytes) ->
    ioProperty $ do
      buf <- newMemoryConnection c
      transp <- t buf
      writeMessage transp (B.byteString bytes)
      actual <- readMessage transp (G.getBytes $ BS.length bytes)
      pure $ actual === RRSuccess bytes

  it "EOF handling" $ do
    buf <- newMemoryConnection 10
    transp <- t buf
    r <- readMessage transp (G.getInt8)
    r `shouldBe` RREOF


spec :: Spec
spec = do
  describe "framedTransport" $ do
    transportSpec framedTransport
    
    it "read case" $ do
      let payload = BS.pack [0x01, 0x05, 0x01, 0x08, 0xFF]
      buf <- newMemoryConnection 1
      transp <- framedTransport buf
      cPut buf $ BS.pack [0x00, 0x00, 0x00, 0x05]
      cPut buf payload
      r <- readMessage transp (G.getBytes $ BS.length payload)
      r `shouldBe` RRSuccess payload

    it "write case" $ do
      let payload = BS.pack [0x01, 0x05, 0x01, 0x08, 0xFF]
      buf <- newMemoryConnection 1
      transp <- framedTransport buf
      writeMessage transp (B.byteString payload)
      actual <- mGetContents buf
      actual `shouldBe` (BS.pack [0x00, 0x00, 0x00, 0x05] <> payload)


  describe "unframedTransport" $ do
    transportSpec unframedTransport

    prop "read cases" $ \(SomeByteString payload) ->
      ioProperty $ do
        buf <- newMemoryConnection 1
        transp <- unframedTransport buf
        cPut buf payload
        r <- readMessage transp (G.getBytes $ BS.length payload)
        pure $ r === RRSuccess payload

    prop "write cases" $ \(SomeByteString payload) ->
      ioProperty $ do
        buf <- newMemoryConnection 1
        transp <- unframedTransport buf
        writeMessage transp (B.byteString payload)
        actual <- mGetContents buf
        pure $ actual === payload
