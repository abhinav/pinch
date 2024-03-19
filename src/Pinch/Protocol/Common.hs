module Pinch.Protocol.Common
  ( parseUtf8Incremental
  , parseIDLMethodName
  ) where

import Control.Monad   (unless, replicateM)
import Data.ByteString (ByteString)
import Data.Text       (Text)
import Data.Word       (Word8)

import qualified Data.ByteString          as B
import qualified Data.Char                as C
import qualified Data.Serialize.Get       as G
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE

data Utf8Result
  = Utf8Success !TE.StrictBuilder
  | Utf8Failure !TE.UnicodeException
  | Utf8Partial !(ByteString -> Utf8Result)

-- | Wrapper around 'decodeUtf8Chunk', which reports 'UnicodeException's
parseUtf8Incremental' :: ByteString -> Utf8Result
parseUtf8Incremental' initialBs = go mempty (TE.decodeUtf8Chunk initialBs)
  where
    go :: TE.StrictBuilder -> (TE.StrictBuilder, ByteString, Maybe TE.Utf8State) -> Utf8Result
    go acc (builder, leftovers, Nothing)
      | B.null leftovers = Utf8Success $ acc <> builder
      | otherwise = case TE.decodeUtf8' leftovers of
          Left err -> Utf8Failure err
          Right str ->
            -- impossible, unless the `text` library has a bug,
            -- but we deal with it anyway
            Utf8Partial $ \bs ->
              go (acc <> TE.textToStrictBuilder str) $ TE.decodeUtf8Chunk bs
    go acc (builder, leftovers, Just state)
      | B.null leftovers = Utf8Success (acc <> builder)
      | otherwise = Utf8Partial $ \bs ->
      go (acc <> builder) $ TE.decodeUtf8More state bs

parseUtf8Incremental :: Int -> Int -> G.Get Text
parseUtf8Incremental maxChunkSize n | n < maxChunkSize = do
  bs <- G.getBytes n
  case TE.decodeUtf8' bs of
    Left err -> fail $ "Couldn't parse thrift function name: " ++ show err
    Right a -> pure a
parseUtf8Incremental maxChunkSize nameLength = do
  bytes <- G.getBytes maxChunkSize
  go mempty bytes (nameLength - maxChunkSize) $ parseUtf8Incremental' bytes

  where
    go :: TE.StrictBuilder -> ByteString -> Int -> Utf8Result -> G.Get Text
    go _ _ _ (Utf8Failure err) = fail $ "Method name not a valid UTF8 string: " ++ show err
    go acc _ 0 (Utf8Success part) = pure $ TE.strictBuilderToText $ acc <> part
    go acc _ n (Utf8Success part) = let chunkSize = min n maxChunkSize in do
      chunk <- G.getBytes chunkSize
      go (acc <> part) chunk (n - chunkSize) $ parseUtf8Incremental' chunk
    go _ chunk 0 (Utf8Partial _) = fail $ "Method name has unfinished UTF8 sequence: " ++ show (B.takeEnd 4 chunk)
    go acc _ n (Utf8Partial cont) = let chunkSize = min n maxChunkSize in do
      chunk <- G.getBytes chunkSize
      go acc chunk (n - chunkSize) $ cont chunk


validIdentStart :: Char -> Bool
validIdentStart '_' = True
validIdentStart c = C.isAscii c && C.isLetter c

validIdentChar :: Char -> Bool
validIdentChar ':' = True -- for multiplexing
validIdentChar '.' = True
validIdentChar '_' = True
validIdentChar c = C.isAscii c && (C.isLetter c || C.isDigit c)

w2c :: Word8 -> Char
w2c = toEnum . fromIntegral

invalidIdentStart :: Char -> G.Get a
invalidIdentStart c = fail $ "Invalid identifier start: " ++ show c

-- The thrift standard doesn't specify any limitations on the names of methods
-- but the official compiler has a very restrictive grammar defined
-- [here](https://thrift.apache.org/docs/idl)
parseIDLMethodName :: Int -> Int -> G.Get Text
parseIDLMethodName _ 0 = fail "Identifier of length 0"
parseIDLMethodName _ n | n < 0 = fail "Identifier of length <0"
parseIDLMethodName maxChunkSize n | n < maxChunkSize = do
  bs <- G.getBytes n
  let Just (c, cs) = B.uncons bs
  let cChar = w2c c
  unless (validIdentStart cChar) $ invalidIdentStart cChar
  unless (B.all (validIdentChar . w2c) cs) $ fail $ "Invalid identifier" ++ show bs
  pure $ TE.decodeUtf8 bs
parseIDLMethodName maxChunkSize n = do
  [c] <- B.unpack <$> G.getBytes 1 :: G.Get [Word8]
  let cChar = w2c c
  unless (validIdentStart cChar) $ invalidIdentStart cChar
  let n1 = n - 1
  let numChunks = n1 `div` maxChunkSize
  chunks <- replicateM numChunks (parseChunk maxChunkSize)
  lastChunk <- parseChunk (n1 `mod` maxChunkSize)
  pure $ TE.decodeUtf8 $ B.concat $ B.singleton c : (chunks <> [lastChunk])

  where
    parseChunk :: Int -> G.Get ByteString
    parseChunk m = do
      bs <- G.getBytes m
      unless (B.all (validIdentChar . w2c) bs) $ fail $ "Invalid identifier fragment: " ++ show bs
      pure bs
