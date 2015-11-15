{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  Pinch.Protocol.Binary
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Implements the Thrift Binary Protocol as a 'Protocol'.
module Pinch.Protocol.Binary (binaryProtocol) where


#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Control.Monad
import Data.Bits           (shiftR, (.&.))
import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashSet        (HashSet)
import Data.Int            (Int16, Int8)
import Data.Vector         (Vector)

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.Text.Encoding  as TE
import qualified Data.Vector         as V

import Pinch.Internal.Builder (Build)
import Pinch.Internal.Message
import Pinch.Internal.Parser  (Parser, runParser)
import Pinch.Internal.TType
import Pinch.Internal.Value
import Pinch.Protocol         (Protocol (..))

import qualified Pinch.Internal.Builder as BB
import qualified Pinch.Internal.Parser  as P


-- | Provides an implementation of the Thrift Binary Protocol.
binaryProtocol :: Protocol
binaryProtocol = Protocol
    { serializeValue     = BB.run . binarySerialize
    , deserializeValue   = binaryDeserialize ttype
    , serializeMessage   = BB.run . binarySerializeMessage
    , deserializeMessage = binaryDeserializeMessage
    }

------------------------------------------------------------------------------

binarySerializeMessage :: Message -> Build
binarySerializeMessage msg = do
    binarySerialize . VBinary . TE.encodeUtf8 $ messageName msg
    BB.int8  $ messageCode (messageType msg)
    BB.int32 $ messageId msg
    binarySerialize (messagePayload msg)


binaryDeserializeMessage :: ByteString -> Either String Message
binaryDeserializeMessage = runParser binaryMessageParser

binaryMessageParser :: Parser Message
binaryMessageParser = do
    size <- P.int32
    if size < 0
        then parseStrict size
        else parseNonStrict size
  where
    -- versionAndType:4 name~4 seqid:4 payload
    -- versionAndType = version:2 0x00 type:1
    parseStrict versionAndType = do
        unless (version == 1) $
            fail $ "Unsupported version: " ++ show version
        Message
            <$> TE.decodeUtf8 <$> (P.int32 >>= P.take . fromIntegral)
            <*> typ
            <*> P.int32
            <*> binaryParser ttype
      where
        version = (0x7fff0000 .&. versionAndType) `shiftR` 16

        code = fromIntegral $ 0x00ff .&. versionAndType
        typ = case fromMessageCode code of
            Nothing -> fail $ "Unknown message type: " ++ show code
            Just t -> return t

    -- name~4 type:1 seqid:4 payload
    parseNonStrict nameLength =
        Message
            <$> TE.decodeUtf8 <$> P.take (fromIntegral nameLength)
            <*> parseMessageType
            <*> P.int32
            <*> binaryParser ttype


parseMessageType :: Parser MessageType
parseMessageType = P.int8 >>= \code -> case fromMessageCode code of
    Nothing -> fail $ "Unknown message type: " ++ show code
    Just t -> return t

------------------------------------------------------------------------------

binaryDeserialize :: TType a -> ByteString -> Either String (Value a)
binaryDeserialize t = runParser (binaryParser t)

binaryParser :: TType a -> Parser (Value a)
binaryParser typ = case typ of
  TBool   -> parseBool
  TByte   -> parseByte
  TDouble -> parseDouble
  TInt16  -> parseInt16
  TInt32  -> parseInt32
  TInt64  -> parseInt64
  TBinary -> parseBinary
  TStruct -> parseStruct
  TMap    -> parseMap
  TSet    -> parseSet
  TList   -> parseList

getTType :: Int8 -> Parser SomeTType
getTType code =
    maybe (fail $ "Unknown TType: " ++ show code) return $ fromTypeCode code

parseTType :: Parser SomeTType
parseTType = P.int8 >>= getTType

parseBool :: Parser (Value TBool)
parseBool = VBool . (== 1) <$> P.int8

parseByte :: Parser (Value TByte)
parseByte = VByte <$> P.int8

parseDouble :: Parser (Value TDouble)
parseDouble = VDouble <$> P.double

parseInt16 :: Parser (Value TInt16)
parseInt16 = VInt16 <$> P.int16

parseInt32 :: Parser (Value TInt32)
parseInt32 = VInt32 <$> P.int32

parseInt64 :: Parser (Value TInt64)
parseInt64 = VInt64 <$> P.int64

parseBinary :: Parser (Value TBinary)
parseBinary = VBinary <$> (P.int32 >>= P.take . fromIntegral)


parseMap :: Parser (Value TMap)
parseMap = do
    ktype' <- parseTType
    vtype' <- parseTType
    count <- P.int32

    case (ktype', vtype') of
      (SomeTType ktype, SomeTType vtype) -> do
        pairs <- replicateM (fromIntegral count) $
            (,) <$> binaryParser ktype
                <*> binaryParser vtype
        return $ VMap (M.fromList pairs)


parseSet :: Parser (Value TSet)
parseSet = do
    vtype' <- parseTType
    count <- P.int32

    case vtype' of
      SomeTType vtype -> do
          items <- replicateM (fromIntegral count) (binaryParser vtype)
          return $ VSet (S.fromList items)


parseList :: Parser (Value TList)
parseList = do
    vtype' <- parseTType
    count <- P.int32

    case vtype' of
      SomeTType vtype ->
        VList <$> V.replicateM (fromIntegral count) (binaryParser vtype)


parseStruct :: Parser (Value TStruct)
parseStruct = P.int8 >>= loop M.empty
  where
    loop :: HashMap Int16 SomeValue -> Int8 -> Parser (Value TStruct)
    loop fields    0 = return $ VStruct fields
    loop fields code = do
        vtype' <- getTType code
        fieldId <- P.int16
        case vtype' of
          SomeTType vtype -> do
            value <- SomeValue <$> binaryParser vtype
            loop (M.insert fieldId value fields) =<< P.int8


------------------------------------------------------------------------------

binarySerialize :: Value a -> Build
binarySerialize v0 = case v0 of
  VBinary  x -> do
    BB.int32 . fromIntegral . B.length $ x
    BB.byteString x
  VBool    x -> BB.int8 $ if x then 1 else 0
  VByte    x -> BB.int8   x
  VDouble  x -> BB.double x
  VInt16   x -> BB.int16  x
  VInt32   x -> BB.int32  x
  VInt64   x -> BB.int64  x
  VStruct xs -> serializeStruct xs
  VList   xs -> serializeList ttype       xs
  VMap    xs -> serializeMap  ttype ttype xs
  VSet    xs -> serializeSet  ttype       xs


serializeStruct :: HashMap Int16 SomeValue -> Build
serializeStruct fields = do
    forM_ (M.toList fields) $ \(fieldId, SomeValue fieldValue) ->
        writeField fieldId ttype fieldValue
    BB.int8 0
  where
    writeField :: Int16 -> TType a -> Value a -> Build
    writeField fieldId fieldType fieldValue = do
        BB.int8 (toTypeCode fieldType)
        BB.int16 fieldId
        binarySerialize fieldValue


serializeList :: TType a -> Vector (Value a) -> Build
serializeList vtype xs = do
    BB.int8  $ toTypeCode vtype
    BB.int32 $ fromIntegral (V.length xs)
    mapM_ binarySerialize (V.toList xs)


serializeMap :: TType k -> TType v -> HashMap (Value k) (Value v) -> Build
serializeMap kt vt xs = do
    BB.int8  $ toTypeCode kt
    BB.int8  $ toTypeCode vt
    BB.int32 $ fromIntegral (M.size xs)
    forM_ (M.toList xs) $ \(k, v) -> do
        binarySerialize k
        binarySerialize v


serializeSet :: TType a -> HashSet (Value a) -> Build
serializeSet vtype xs = do
    BB.int8  $ toTypeCode vtype
    BB.int32 $ fromIntegral (S.size xs)
    mapM_ binarySerialize (S.toList xs)


------------------------------------------------------------------------------


messageCode :: MessageType -> Int8
messageCode Call      = 1
messageCode Reply     = 2
messageCode Exception = 3
messageCode Oneway    = 4


fromMessageCode :: Int8 -> Maybe MessageType
fromMessageCode 1 = Just Call
fromMessageCode 2 = Just Reply
fromMessageCode 3 = Just Exception
fromMessageCode 4 = Just Oneway
fromMessageCode _ = Nothing


-- | Map a TType to its type code.
toTypeCode :: TType a -> Int8
toTypeCode TBool   = 2
toTypeCode TByte   = 3
toTypeCode TDouble = 4
toTypeCode TInt16  = 6
toTypeCode TInt32  = 8
toTypeCode TInt64  = 10
toTypeCode TBinary = 11
toTypeCode TStruct = 12
toTypeCode TMap    = 13
toTypeCode TSet    = 14
toTypeCode TList   = 15


-- | Map a type code to the corresponding TType.
fromTypeCode :: Int8 -> Maybe SomeTType
fromTypeCode 2  = Just $ SomeTType TBool
fromTypeCode 3  = Just $ SomeTType TByte
fromTypeCode 4  = Just $ SomeTType TDouble
fromTypeCode 6  = Just $ SomeTType TInt16
fromTypeCode 8  = Just $ SomeTType TInt32
fromTypeCode 10 = Just $ SomeTType TInt64
fromTypeCode 11 = Just $ SomeTType TBinary
fromTypeCode 12 = Just $ SomeTType TStruct
fromTypeCode 13 = Just $ SomeTType TMap
fromTypeCode 14 = Just $ SomeTType TSet
fromTypeCode 15 = Just $ SomeTType TList
fromTypeCode _  = Nothing
