{-# LANGUAGE BangPatterns        #-}
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
import Data.Int            (Int16, Int32, Int8)
import Data.Monoid

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as M
import qualified Data.Text.Encoding  as TE

import Pinch.Internal.Builder (Builder)
import Pinch.Internal.Message
import Pinch.Internal.Parser  (Parser, runParser, runParser')
import Pinch.Internal.TType
import Pinch.Internal.Value
import Pinch.Protocol         (Protocol (..))

import qualified Pinch.Internal.Builder  as BB
import qualified Pinch.Internal.FoldList as FL
import qualified Pinch.Internal.Parser   as P


-- | Provides an implementation of the Thrift Binary Protocol.
binaryProtocol :: Protocol
binaryProtocol = Protocol
    { serializeValue     = binarySerialize
    , deserializeValue'  = binaryDeserialize ttype
    , serializeMessage   = binarySerializeMessage
    , deserializeMessage = binaryDeserializeMessage
    }

------------------------------------------------------------------------------

binarySerializeMessage :: Message -> Builder
binarySerializeMessage msg =
    string (TE.encodeUtf8 $ messageName msg) <>
    BB.int8 (messageCode (messageType msg)) <> BB.int32BE (messageId msg) <>
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

binaryDeserialize :: TType a -> ByteString -> Either String (ByteString, Value a)
binaryDeserialize t = runParser' (binaryParser t)

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
        items <- FL.replicateM (fromIntegral count) $
            MapItem <$> binaryParser ktype
                    <*> binaryParser vtype
        return $ VMap items


parseSet :: Parser (Value TSet)
parseSet = do
    vtype' <- parseTType
    count <- P.int32

    case vtype' of
      SomeTType vtype ->
          VSet <$> FL.replicateM (fromIntegral count) (binaryParser vtype)


parseList :: Parser (Value TList)
parseList = do
    vtype' <- parseTType
    count <- P.int32

    case vtype' of
      SomeTType vtype ->
        VList <$> FL.replicateM (fromIntegral count) (binaryParser vtype)


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

binarySerialize :: forall a. IsTType a => Value a -> Builder
binarySerialize = case (ttype :: TType a) of
  TBinary  -> serializeBinary
  TBool    -> serializeBool
  TByte    -> serializeByte
  TDouble  -> serializeDouble
  TInt16   -> serializeInt16
  TInt32   -> serializeInt32
  TInt64   -> serializeInt64
  TStruct  -> serializeStruct
  TList    -> serializeList
  TMap     -> serializeMap
  TSet     -> serializeSet
{-# INLINE binarySerialize #-}

serializeBinary :: Value TBinary -> Builder
serializeBinary (VBinary x) = string x
{-# INLINE serializeBinary #-}

serializeBool :: Value TBool -> Builder
serializeBool (VBool x) = BB.int8 $ if x then 1 else 0
{-# INLINE serializeBool #-}

serializeByte :: Value TByte -> Builder
serializeByte (VByte x) = BB.int8 x
{-# INLINE serializeByte #-}

serializeDouble :: Value TDouble -> Builder
serializeDouble (VDouble x) = BB.doubleBE x
{-# INLINE serializeDouble #-}

serializeInt16 :: Value TInt16 -> Builder
serializeInt16 (VInt16 x) = BB.int16BE x
{-# INLINE serializeInt16 #-}

serializeInt32 :: Value TInt32 -> Builder
serializeInt32 (VInt32 x) = BB.int32BE x
{-# INLINE serializeInt32 #-}

serializeInt64 :: Value TInt64 -> Builder
serializeInt64 (VInt64 x) = BB.int64BE x
{-# INLINE serializeInt64 #-}

serializeList :: Value TList -> Builder
serializeList (VList xs) = serializeCollection ttype xs
{-# INLINE serializeList #-}

serializeSet :: Value TSet -> Builder
serializeSet (VSet xs) = serializeCollection ttype xs
{-# INLINE serializeSet #-}

serializeStruct :: Value TStruct -> Builder
serializeStruct (VStruct fields) =
    M.foldlWithKey'
        (\rest fid (SomeValue val) -> rest <> writeField fid ttype val)
        mempty fields
    <> BB.int8 0
  where
    writeField :: IsTType a => Int16 -> TType a -> Value a -> Builder
    writeField fieldId fieldType fieldValue =
        typeCode fieldType <> BB.int16BE fieldId <> binarySerialize fieldValue
    {-# INLINE writeField #-}
{-# INLINE serializeStruct #-}

serializeMap :: Value TMap -> Builder
serializeMap VNullMap = error "serializeMap: VNullMap"
serializeMap (VMap items) = serialize ttype ttype items
  where
    serialize
        :: (IsTType k, IsTType v)
        => TType k -> TType v -> FL.FoldList (MapItem k v) -> Builder
    serialize kt vt xs =
        typeCode kt <> typeCode vt <> BB.int32BE size <> body
      where
        (body, size) = FL.foldl' go (mempty, 0 :: Int32) xs
        go (prev, !c) (MapItem k v) =
            ( prev <> binarySerialize k <> binarySerialize v
            , c + 1
            )
{-# INLINE serializeMap #-}

serializeCollection
    :: IsTType a
    => TType a -> FL.FoldList (Value a) -> Builder
serializeCollection vtype xs =
    let go (prev, !c) item = (prev <> binarySerialize item, c + 1)
        (body, size) = FL.foldl' go (mempty, 0 :: Int32) xs
    in typeCode vtype <> BB.int32BE size <> body
{-# INLINE serializeCollection #-}

------------------------------------------------------------------------------


messageCode :: MessageType -> Int8
messageCode Call      = 1
messageCode Reply     = 2
messageCode Exception = 3
messageCode Oneway    = 4
{-# INLINE messageCode #-}


fromMessageCode :: Int8 -> Maybe MessageType
fromMessageCode 1 = Just Call
fromMessageCode 2 = Just Reply
fromMessageCode 3 = Just Exception
fromMessageCode 4 = Just Oneway
fromMessageCode _ = Nothing
{-# INLINE fromMessageCode #-}


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
{-# INLINE toTypeCode #-}


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
{-# INLINE fromTypeCode #-}

------------------------------------------------------------------------------


string :: ByteString -> Builder
string b = BB.int32BE (fromIntegral $ B.length b) <> BB.byteString b
{-# INLINE string #-}

typeCode :: TType a -> Builder
typeCode = BB.int8 . toTypeCode
{-# INLINE typeCode #-}
