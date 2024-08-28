{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Pinch.Transport
  ( Transport(..)
  , framedTransport
  , unframedTransport
  , headerTransport
  , emptyHeaderData
  , HeaderData(..)
  , Connection(..)
  , ReadResult(..)
  ) where

import Control.DeepSeq (NFData)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll, recv)
import System.IO (Handle)

import qualified Control.Monad.Trans.Writer as W
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Serialize.Get as G

import qualified Pinch.Internal.Builder as B
import Data.Word
import Data.Bits (Bits(..))
import Control.Monad (when, replicateM, unless)
import GHC.IORef (IORef)
import Data.Traversable (for)
import GHC.List (foldl')
import Data.Foldable (for_)
import GHC.Generics (Generic)

class Connection c where
  -- | Returns available bytes, or an empty bytestring if EOF was reached.
  cGetSome :: c -> IO BS.ByteString
  -- | Writes the given builder.
  cPut :: c -> B.Builder -> IO ()

pattern KVInfoTag :: Integral a => a
pattern KVInfoTag = 1

instance Connection Handle where
  cPut c b = BS.hPut c (B.runBuilder b)
  cGetSome h = BS.hGetSome h 1024

instance Connection Socket where
  cPut c b = sendAll c (B.runBuilder b)
  cGetSome s = recv s 4096

data ReadResult a
  = RRSuccess a
  | RRFailure String
  | RREOF
  deriving (Eq, Show, Functor)

data HeaderData
  = HeaderData
  { hSequenceNumber :: !Word32
  , hFlags :: Word16
  , hInfo :: ![(BS.ByteString, BS.ByteString)]
  } deriving (Eq, Generic, Show, NFData)

emptyHeaderData :: HeaderData
emptyHeaderData = HeaderData 0 0 []

-- | A bidirectional transport to read/write messages from/to.
data Transport
  = Transport
  { writeMessage :: HeaderData -> B.Builder -> IO ()
  , readMessage  :: forall a . Show a => G.Get a -> IO (ReadResult (a, HeaderData))
  }

-- | Creates a thrift framed transport. See also <https://github.com/apache/thrift/blob/master/doc/specs/thrift-rpc.md#framed-vs-unframed-transport>.
framedTransport :: Connection c => c -> IO Transport
framedTransport c = do
  readBuffer <- newIORef mempty
  pure $ Transport (const writeMsg) (fmap (fmap (,emptyHeaderData)) <$> readMsg readBuffer)

  where
    writeMsg msg = do
      cPut c $ B.int32BE (fromIntegral $ B.getSize msg) <> msg
    readMsg readBuffer parser = do
      let
        frameParser = do
          size <- G.getInt32be
          G.isolateLazy (fromIntegral size) parser

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
  pure $ Transport (const writeMsg) (fmap (fmap (,emptyHeaderData)) <$> readMsg readBuffer)

  where
    writeMsg = cPut c

    readMsg buf p = do
      initial <- readIORef buf
      (leftovers, r) <- runGetWith (cGetSome c) p initial
      writeIORef buf $! leftovers
      pure r

binaryVersionMask :: Word32
binaryVersionMask = 0xffff0000

compactProtocolId :: Word32
compactProtocolId = 0x82

compactVersionMask :: Word32
compactVersionMask = 0x1f

compactProtocolVersion :: Word32
compactProtocolVersion = 1

headerMask :: Word32
headerMask = 0xffff0000

headerMagic16 :: Word16
headerMagic16 = 0x0fff

headerMagic32 :: Word32
headerMagic32 = 0x0fff0000

binaryVersion1Pattern :: Word32
binaryVersion1Pattern = 0x80010000

data Protocol
  = PBinary
  | PCompact

data Mode
  = MHeader !ParsedHeader
  | MFramed !Protocol
  | MUnframed !Protocol


-- If you're adding a transform, please make sure that they're applied
-- in the correct order, both for reading and writing messages.
data Transform
  = Transform
  { tRead :: BS.ByteString -> BS.ByteString
  , tWrite :: BS.ByteString -> BS.ByteString
  }

-- Creates one chunk of lazy output, converted to strict in O(1)
-- Judging by some benchmarks found [here](https://quixdb.github.io/squash-benchmark/)
-- It's rare that zlib produces a compression ratio < 2 (on data that isn't already compressed)
zlibCompressStrict :: BS.ByteString -> BS.ByteString
zlibCompressStrict bs = BSL.toStrict $ Zlib.compressWith params (BSL.fromStrict bs)
  where
    params = Zlib.defaultCompressParams{ Zlib.compressBufferSize = BS.length bs `div` 2 }

-- Creates one chunk of lazy output, converted to strict in O(1)
-- Judging by some benchmarks found [here](https://quixdb.github.io/squash-benchmark/)
-- It's rare that zlib produces a compression ratio > 3.5, unless its input is something
-- with a tiny amount of entropy, like xml.
zlibDecompressStrict :: BS.ByteString -> BS.ByteString
zlibDecompressStrict bs = BSL.toStrict $ Zlib.decompressWith params (BSL.fromStrict bs)
  where
    params = Zlib.defaultDecompressParams{ Zlib.decompressBufferSize = BS.length bs * 4 }

zlibTransform :: Transform
zlibTransform
  = Transform
  { tRead = zlibDecompressStrict
  , tWrite = zlibCompressStrict
  }

transformsById :: [(Word32, Transform)]
transformsById = [(1, zlibTransform)]

data ParsedHeader
  = ParsedHeader
  { phFlags :: !Word16 -- ^ I have no idea what these are for
  , phSize :: Word32
  , phProtocolId :: !Word16
  , phSequenceNumber :: !Word32
  , phTransforms :: ![(Word32, Transform)]
  , phInfo :: ![(BS.ByteString, BS.ByteString)]
  }

isBinaryProtocol :: Word32 -> Bool
isBinaryProtocol bytes = (bytes .&. binaryVersionMask) == binaryVersion1Pattern

isCompactProtocol :: Word32 -> Bool
isCompactProtocol bytes =
  bytes `unsafeShiftR` 24 == compactProtocolId
  && (((bytes `unsafeShiftR` 16) .&. compactVersionMask)
  == compactProtocolVersion)

-- This takes the 'magic bytes' and 'flags' all at once.
headerMagicMatches :: Word32 -> Bool
headerMagicMatches bytes = (bytes .&. headerMask) == headerMagic32

defaultMode :: Mode
defaultMode = MHeader ParsedHeader
  { phFlags = 0
  , phSize = 0
  , phInfo = []
  , phProtocolId = 0
  , phTransforms = []
  , phSequenceNumber = 0
  }

-- | Creates a thrift header transport, which can apparently use either
-- framed or unframed transports under the hood.
headerTransport :: Connection c => c -> IO Transport
headerTransport c = do
  readBuffer <- newIORef mempty
  mode <- newIORef defaultMode
  pure $ Transport (writeHeaderMsg c mode) (readHeaderMsg c readBuffer mode)

composeFunctions :: [a -> a] -> a -> a
composeFunctions = foldl' (flip (.)) id

writeHeaderMsg :: Connection c => c -> IORef Mode -> HeaderData -> B.Builder -> IO ()
writeHeaderMsg c modeRef headerData@HeaderData{..} msg = readIORef modeRef >>= \case
  MUnframed _ -> cPut c msg
  MFramed _ -> cPut c $ B.int32BE (fromIntegral $ B.getSize msg) <> msg
  MHeader parsedHeader@ParsedHeader{..} -> do
    let transform = composeFunctions $ tWrite . snd <$> phTransforms
    let payload = transform $ B.runBuilder msg
    let header = padHeader $ mkHeaderBuf parsedHeader headerData
    let headerSize = B.getSize header
    let frameSize = BS.length payload + headerSize + 10
    -- when (header > maxFrameSize) $ throwIO $ -- TODO
    cPut c $ W.execWriter $ do
      W.tell $ B.word32BE $ fromIntegral frameSize
      W.tell $ B.word16BE headerMagic16
      W.tell $ B.word16BE hFlags
      W.tell $ B.word32BE hSequenceNumber
      W.tell $ B.word16BE $ fromIntegral headerSize `div` 4
      W.tell header
      W.tell $ B.byteString payload

padHeader :: B.Builder -> B.Builder
padHeader b = b <> B.byteString (BS.replicate paddingNeeded 0)
  where
    paddingNeeded = (4 - (B.getSize b `mod` 4)) `mod` 4

mkHeaderBuf :: ParsedHeader -> HeaderData -> B.Builder
mkHeaderBuf ParsedHeader{..} HeaderData{..} = W.execWriter $ do
  W.tell $ writeVarInt phProtocolId
  W.tell $ writeVarInt $ length phTransforms
  for_ phTransforms $ W.tell . writeVarInt . fst
  unless (null hInfo) $ do
    W.tell $ writeVarInt (KVInfoTag :: Word32)
    W.tell $ writeVarInt $ length hInfo
    for_ hInfo writeInfoKV

writeInfoKV :: (BS.ByteString, BS.ByteString) -> W.Writer B.Builder ()
writeInfoKV (key, val) = W.tell $ writeVarString key <> writeVarString val

writeVarString :: BS.ByteString -> B.Builder
writeVarString bs = writeVarInt (fromIntegral @Int @Word32 $ BS.length bs) <> B.byteString bs

modeToHeaderData :: Mode -> HeaderData
modeToHeaderData mode = case mode of
  MHeader ParsedHeader{..} ->
    HeaderData
      { hSequenceNumber = phSequenceNumber
      , hFlags = phFlags
      , hInfo = phInfo
      }
  _ -> emptyHeaderData

readHeaderMsg :: forall a conn. (Show a, Connection conn) => conn -> IORef BS.ByteString -> IORef Mode -> G.Get a -> IO (ReadResult (a, HeaderData))
readHeaderMsg conn buf modeRef p = do
  initial <- readIORef buf
  (leftovers, res) <- runGetWith (cGetSome conn) frameParser initial
  writeIORef buf $! leftovers
  case res of
    RRSuccess (a, mode) -> do
      writeIORef modeRef $! mode
      pure $ RRSuccess (a, modeToHeaderData mode)
    RREOF -> pure RREOF
    RRFailure err -> pure $ RRFailure err

  where

  -- The thrift 'header' transport type inspects all inbound messages.
  -- It detects:
  -- * A transport type (which might be unframed, framed or header).
  -- * A prototol type (which might be binary, or compact).
  -- It replies to each message with a matching (transport type, protocol type).
  frameParser :: G.Get (a, Mode)
  frameParser = do
    -- TODO there's no isEOF functionality in cereal,
    -- or any catch functionality, so we can't recover
    -- if determining frame type fails, because we're at
    -- the end of a stream.

    frameStart <- G.bytesRead
    first4Bytes <- G.getWord32be
    if
      | isBinaryProtocol first4Bytes -> (,MUnframed PBinary) <$> p
      | isCompactProtocol first4Bytes -> (,MUnframed PCompact) <$> p
      | otherwise -> do
        -- All other options start with a frame size.
        let frameSize = first4Bytes
        G.isolateLazy (fromIntegral frameSize) $ do
          -- Either the version field of a compact/binary protocol message, or
          -- the magic value and flags of a header message.
          second4Bytes <- G.lookAhead G.getWord32be
          if
            | isBinaryProtocol second4Bytes -> (,MFramed PBinary) <$> p
            | isCompactProtocol second4Bytes -> (,MFramed PCompact) <$> p
            | headerMagicMatches second4Bytes -> do
                header@ParsedHeader{..} <- readHeader frameSize
                -- Some info fields might have been skipped in the header, in
                -- which case we need to skip *to* the end of the header
                -- 14 is the combined length of the 'length', 'magic', 'flags',
                -- and 'header size' fields
                let headerEnd = frameStart + 14 + fromIntegral phSize
                position <- G.bytesRead
                when (position > headerEnd) $ fail $ "Internal error reading header. position: " <> show position <> ", headerEnd: " <> show headerEnd
                let bytesToSkip = headerEnd - position
                when (position < headerEnd) $ G.skip bytesToSkip
                let addMode = (,MHeader header)
                case phTransforms of
                  [] -> addMode <$> p
                  transforms -> do
                    let transform = composeFunctions $ tRead . snd <$> transforms
                    -- In cereal, `getLazyByteString` actually calls `getByteString`, so we
                    -- may as well embrace the strictness, and get some performance benefits.
                    let dataBytes = fromIntegral frameSize - fromIntegral phSize - 14
                    bytes <- G.getByteString dataBytes
                    case G.runGet p $ transform bytes of
                      Left err -> fail err
                      Right a -> pure $ addMode a
            | otherwise -> fail $ "Could not detect client transport type. Second eight bytes: " <> convertToBase 16 (fromIntegral second4Bytes) <> " " <> show second4Bytes

convertToBase :: Word8 -> Integer -> String
convertToBase b n
  | n < 0              = '-' : convertToBase b (-n)
  | n < fromIntegral b = [(['0'..'9'] ++ ['A' .. 'Z']) !! fromIntegral n]
  | otherwise          = let (d, m) = n `divMod` fromIntegral b in convertToBase b d ++ convertToBase b m

readVarInt :: (Num a, Bits a) => Int -> G.Get a
readVarInt headerEnd = do
  limit <- (headerEnd -) <$> G.bytesRead
  -- Maximum bytes to fill a Word32 is 5
  case limit of
    0 -> fail "VarInt fully exceeds bounds of header"
    _ -> do
      firstByte <- G.getWord8
      let acc = fromIntegral $ firstByte .&. 0x7f
      case firstByte of
        0 -> pure 0 -- almost certainly end-of-header padding
        _ -> if firstByte .&. 0x80 > 0
          then pure acc
          else go (min limit 5 - 1) acc 0

  where
    go 0 _ _ = fail "VarInt exceeds bounds of header"
    go limit acc shiftAmt = do
      byte <- G.getWord8
      let acc' = acc .|. (fromIntegral (byte .&. 0x7f) `unsafeShiftL` shiftAmt)
      if byte .&. 0x80 > 0
        then pure acc'
        else go (limit - 1) acc' (shiftAmt + 7)

writeVarInt :: forall a. (Integral a, Bits a) => a -> B.Builder
writeVarInt n = go (B.word8 $ fromIntegral $ 0x80 .|. (n .&. 0x7f)) $ n `unsafeShiftR` 7

  where
  go :: B.Builder -> a -> B.Builder
  go acc n'
    | n' == 0 = acc
    | otherwise =
        go (B.word8 (fromIntegral n' .&. 0x7f) <> acc) (n' `unsafeShiftR` 7)

-- https://github.com/apache/thrift/blob/master/doc/specs/HeaderFormat.md
readHeader :: Word32 -> G.Get ParsedHeader
readHeader frameSize = do
  -- header magic
  _ <- G.skip 2
  phFlags <- G.getWord16be
  phSequenceNumber <- G.getWord32be
  -- The header size is measured in bytes/4
  phSize :: Word32 <- (* 4) . fromIntegral <$> G.getWord16be
  when (fromIntegral phSize > frameSize) $ fail "ParsedHeader size is larger than frame size"
  headerStartPos <- G.bytesRead
  let headerEnd = headerStartPos + fromIntegral phSize
  let limitedVarInt16 :: G.Get Word16 = readVarInt headerEnd
  let limitedVarInt32 :: G.Get Word32 = readVarInt headerEnd
  phProtocolId :: Word16 <- limitedVarInt16
  numTransforms :: Word16 <- limitedVarInt16
  -- The spec (if you can call it that), says transforms can carry data, but
  -- none of the other clients implement this.
  transformIds :: [Word32] <- replicateM (fromIntegral numTransforms) limitedVarInt32
  phTransforms <- for transformIds $ \transformId -> do
    case lookup transformId transformsById of
      Just transform -> pure (transformId, transform)
      Nothing -> fail "Unsupported transform ID"
  phInfo <- readInfoFields headerEnd
  pure ParsedHeader{..}

varString :: Int -> G.Get BS.ByteString
varString headerEnd = do
  n <- readVarInt headerEnd
  pos <- G.bytesRead
  when (n > headerEnd - pos) $ fail "Var string breaches header boundary"
  G.getByteString n

type KVs = [(BS.ByteString, BS.ByteString)]

readInfoFields :: Int -> G.Get KVs
readInfoFields headerEnd = concat . ($ []) <$> go
  where
  limitedVarInt = readVarInt headerEnd
  limitedVarString = varString headerEnd

  go :: G.Get ([KVs] -> [KVs])
  go = do
    position <- G.bytesRead
    if position < headerEnd
      then go'
      else pure id

  -- difference list
  go' :: G.Get ([KVs] -> [KVs])
  go' = do
    infoId :: Word32 <- readVarInt headerEnd
    case infoId of
      KVInfoTag -> do
        -- key/value pairs
        numPairs :: Word32 <- limitedVarInt
        kvs <- replicateM (fromIntegral numPairs) $ do
          key <- limitedVarString
          value <- limitedVarString
          pure (key, value)
        ((kvs :) .) <$> go
      0 -> -- header padding
        pure id
      _ ->
        -- Can't handle info field, or make progress, because we don't
        -- how much to skip to get to the next field.
        -- According to the spec, all info fields are ignorable, so
        -- we just return what we can.
        -- This branch can also be hit because there's padding in the
        -- info area, so that the header size can be expressed divided
        -- by four.
        pure id

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
