{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pinch.Protocol.Binary (binaryProtocol) where

import Control.Monad

import Data.ByteString         (ByteString)
import Data.ByteString.Builder (Builder)
import Data.HashMap.Strict     (HashMap)
import Data.HashSet            (HashSet)
import Data.Int                (Int16)
import Data.Monoid
import Data.Vector             (Vector)
import Data.Word               (Word8)

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.HashMap.Strict     as M
import qualified Data.HashSet            as S
import qualified Data.Text.Encoding      as TE
import qualified Data.Vector             as V

import Pinch.Internal.Message
import Pinch.Internal.Parser  (Parser, runParser)
import Pinch.Internal.TType
import Pinch.Internal.Value
import Pinch.Protocol         (Protocol (..))

import qualified Pinch.Internal.Parser as P

binaryProtocol :: Protocol
binaryProtocol = Protocol
    { serializeValue     = binarySerialize
    , deserializeValue   = binaryDeserialize ttype
    , serializeMessage   = binarySerializeMessage
    , deserializeMessage = binaryDeserializeMessage
    }

------------------------------------------------------------------------------

binarySerializeMessage :: Message a -> Builder
binarySerializeMessage msg = mconcat
    [ binarySerialize . VBinary . TE.encodeUtf8 $ messageName msg
    , BB.word8 $ messageCode (messageType msg)
    , BB.int32BE (messageId msg)
    , binarySerialize (messageBody msg)
    ]

binaryDeserializeMessage
    :: IsTType a => ByteString -> Either String (Message a)
binaryDeserializeMessage = runParser binaryMessageParser

binaryMessageParser :: IsTType a => Parser (Message a)
binaryMessageParser =
    Message
        <$> (TE.decodeUtf8 . vbinary <$> parseBinary)
        <*> parseMessageType
        <*> P.int32
        <*> binaryParser ttype

parseMessageType :: Parser MessageType
parseMessageType = P.word8 >>= \code -> case fromMessageCode code of
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

getTType :: Word8 -> Parser SomeTType
getTType code =
    maybe (fail $ "Unknown TType: " ++ show code) return $ fromCode code

parseTType :: Parser SomeTType
parseTType = P.word8 >>= getTType

parseBool :: Parser (Value TBool)
parseBool = VBool . (== 1) <$> P.word8

parseByte :: Parser (Value TByte)
parseByte = VByte <$> P.word8

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
parseStruct = P.word8 >>= loop M.empty
  where
    loop :: HashMap Int16 SomeValue -> Word8 -> Parser (Value TStruct)
    loop fields    0 = return $ VStruct fields
    loop fields code = do
        vtype' <- getTType code
        fieldId <- P.int16
        case vtype' of
          SomeTType vtype -> do
            value <- SomeValue <$> binaryParser vtype
            loop (M.insert fieldId value fields) =<< P.word8


------------------------------------------------------------------------------

binarySerialize :: Value a -> Builder
binarySerialize v0 = case v0 of
  VBinary  x -> BB.int32BE (fromIntegral $ B.length x) <> BB.byteString x
  VBool    x -> BB.word8 $ if x then 1 else 0
  VByte    x -> BB.word8    x
  VDouble  x -> BB.doubleBE x
  VInt16   x -> BB.int16BE  x
  VInt32   x -> BB.int32BE  x
  VInt64   x -> BB.int64BE  x
  VStruct xs -> serializeStruct xs
  VList   xs -> serializeList ttype       xs
  VMap    xs -> serializeMap  ttype ttype xs
  VSet    xs -> serializeSet  ttype       xs


serializeStruct :: HashMap Int16 SomeValue -> Builder
serializeStruct fields =
    mconcat (map go (M.toList fields)) <> BB.word8 0
  where
    go (fieldId, SomeValue fieldValue) =
        writeField fieldId ttype fieldValue

    writeField :: Int16 -> TType a -> Value a -> Builder
    writeField fieldId fieldType fieldValue = mconcat
        [ BB.word8 $ toCode fieldType
        , BB.int16BE fieldId
        , binarySerialize fieldValue
        ]


serializeList :: TType a -> Vector (Value a) -> Builder
serializeList vtype xs = mconcat
    [ BB.word8   $ toCode vtype
    , BB.int32BE $ fromIntegral (V.length xs)
    , mconcat    $ map binarySerialize (V.toList xs)
    ]


serializeMap :: TType k -> TType v -> HashMap (Value k) (Value v) -> Builder
serializeMap kt vt xs = mconcat
    [ BB.word8   $ toCode kt
    , BB.word8   $ toCode vt
    , BB.int32BE $ fromIntegral (M.size xs)
    , mconcat    $
        map (\(k, v) -> binarySerialize k <> binarySerialize v) (M.toList xs)
    ]


serializeSet :: TType a -> HashSet (Value a) -> Builder
serializeSet vtype xs = mconcat
    [ BB.word8   $ toCode vtype
    , BB.int32BE $ fromIntegral (S.size xs)
    , mconcat    $ map binarySerialize (S.toList xs)
    ]
