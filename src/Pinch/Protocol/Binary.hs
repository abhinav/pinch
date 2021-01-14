{-# LANGUAGE BangPatterns        #-}
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


import Control.Monad
import Data.Bits           (shiftR, (.&.))
import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Int            (Int16, Int32, Int8)

import qualified Data.ByteString        as B
import qualified Data.HashMap.Strict    as M
import qualified Data.Serialize.Get     as G
import qualified Data.Serialize.IEEE754 as G
import qualified Data.Text.Encoding     as TE

import Pinch.Internal.Builder (Builder)
import Pinch.Internal.Message
import Pinch.Internal.TType
import Pinch.Internal.Value
import Pinch.Protocol         (Protocol (..))

import qualified Pinch.Internal.Builder  as BB
import qualified Pinch.Internal.FoldList as FL


-- | Provides an implementation of the Thrift Binary Protocol.
binaryProtocol :: Protocol
binaryProtocol = Protocol
    { serializeValue     = binarySerialize
    , deserializeValue'  = binaryDeserialize ttype
    , serializeMessage   = binarySerializeMessage
    , deserializeMessage' = binaryDeserializeMessage
    }

------------------------------------------------------------------------------

binarySerializeMessage :: Message -> Builder
binarySerializeMessage msg =
    BB.word8 0x80 <>
    BB.word8 0x01 <>
    BB.word8 0x00 <>
    BB.int8 (messageCode $ messageType msg) <>
    string (TE.encodeUtf8 $ messageName msg) <>
    BB.int32BE (messageId msg) <>
    binarySerialize (messagePayload msg)

binaryDeserializeMessage :: G.Get Message
binaryDeserializeMessage = do
    size <- G.getInt32be
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
            <$> TE.decodeUtf8 <$> (G.getInt32be >>= G.getBytes . fromIntegral)
            <*> typ
            <*> G.getInt32be
            <*> binaryDeserialize ttype
      where
        version = (0x7fff0000 .&. versionAndType) `shiftR` 16

        code = fromIntegral $ 0x00ff .&. versionAndType
        typ = case fromMessageCode code of
            Nothing -> fail $ "Unknown message type: " ++ show code
            Just t -> return t

    -- name~4 type:1 seqid:4 payload
    parseNonStrict nameLength =
        Message
            <$> TE.decodeUtf8 <$> G.getBytes (fromIntegral nameLength)
            <*> parseMessageType
            <*> G.getInt32be
            <*> binaryDeserialize ttype


parseMessageType :: G.Get MessageType
parseMessageType = G.getInt8 >>= \code -> case fromMessageCode code of
    Nothing -> fail $ "Unknown message type: " ++ show code
    Just t -> return t

------------------------------------------------------------------------------

binaryDeserialize :: TType a -> G.Get (Value a)
binaryDeserialize typ = case typ of
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

getTType :: Int8 -> G.Get SomeTType
getTType code =
    maybe (fail $ "Unknown TType: " ++ show code) return $ fromTypeCode code

parseTType :: G.Get SomeTType
parseTType = G.getInt8 >>= getTType

parseBool :: G.Get (Value TBool)
parseBool = VBool . (== 1) <$> G.getInt8

parseByte :: G.Get (Value TByte)
parseByte = VByte <$> G.getInt8

parseDouble :: G.Get (Value TDouble)
parseDouble = VDouble <$> G.getFloat64be

parseInt16 :: G.Get (Value TInt16)
parseInt16 = VInt16 <$> G.getInt16be

parseInt32 :: G.Get (Value TInt32)
parseInt32 = VInt32 <$> G.getInt32be

parseInt64 :: G.Get (Value TInt64)
parseInt64 = VInt64 <$> G.getInt64be

parseBinary :: G.Get (Value TBinary)
parseBinary = VBinary <$> (G.getInt32be >>= G.getBytes . fromIntegral)


parseMap :: G.Get (Value TMap)
parseMap = do
    ktype' <- parseTType
    vtype' <- parseTType
    count <- G.getInt32be

    case (ktype', vtype') of
      (SomeTType ktype, SomeTType vtype) -> do
        items <- FL.replicateM (fromIntegral count) $
            MapItem <$> binaryDeserialize ktype
                    <*> binaryDeserialize vtype
        return $ VMap items


parseSet :: G.Get (Value TSet)
parseSet = do
    vtype' <- parseTType
    count <- G.getInt32be

    case vtype' of
      SomeTType vtype ->
          VSet <$> FL.replicateM (fromIntegral count) (binaryDeserialize vtype)


parseList :: G.Get (Value TList)
parseList = do
    vtype' <- parseTType
    count <- G.getInt32be

    case vtype' of
      SomeTType vtype ->
        VList <$> FL.replicateM (fromIntegral count) (binaryDeserialize vtype)


parseStruct :: G.Get (Value TStruct)
parseStruct = G.getInt8 >>= loop M.empty
  where
    loop :: HashMap Int16 SomeValue -> Int8 -> G.Get (Value TStruct)
    loop fields    0 = return $ VStruct fields
    loop fields code = do
        vtype' <- getTType code
        fieldId <- G.getInt16be
        case vtype' of
          SomeTType vtype -> do
            value <- SomeValue <$> binaryDeserialize vtype
            loop (M.insert fieldId value fields) =<< G.getInt8


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
