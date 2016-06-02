{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  Pinch.Protocol.Compact
-- Copyright   :  (c) Ben Gamari 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Implements the Thrift Compact Protocol as a 'Protocol'.
module Pinch.Protocol.Compact (compactProtocol) where


#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Control.Monad
import Data.Bits           hiding (shift)
import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Int            (Int16, Int32, Int64)
import Data.List           (sortBy)
import Data.Monoid
import Data.Ord            (comparing)
import Data.Word           (Word64, Word8)

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


-- | Provides an implementation of the Thrift Compact Protocol.
compactProtocol :: Protocol
compactProtocol = Protocol
    { serializeValue     = compactSerialize
    , deserializeValue'  = compactDeserialize ttype
    , serializeMessage   = compactSerializeMessage
    , deserializeMessage = compactDeserializeMessage
    }

------------------------------------------------------------------------------

protocolId, version :: Word8
protocolId = 0x82
version = 0x01

compactSerializeMessage :: Message -> Builder
compactSerializeMessage msg =
    BB.word8 protocolId <>
    BB.word8 ((version .&. 0x1f) .|. (messageCode (messageType msg) `shiftL` 5)) <>
    serializeVarint (fromIntegral $ messageId msg) <>
    string (TE.encodeUtf8 $ messageName msg) <>
    compactSerialize (messagePayload msg)

compactDeserializeMessage :: ByteString -> Either String Message
compactDeserializeMessage = runParser compactMessageParser

compactMessageParser :: Parser Message
compactMessageParser = do
    pid <- P.word8
    when (pid /= protocolId) $ fail "Invalid protocol ID"
    w <- P.word8
    let ver = w .&. 0x1f
    when (ver /= version) $ fail $ "Unsupported version: " ++ show ver
    let code = w `shiftR` 5
    msgId <- parseVarint
    msgName <- TE.decodeUtf8 <$> (parseVarint >>= P.take . fromIntegral)
    payload <- compactParser ttype
    mtype <- case fromMessageCode code of
        Nothing -> fail $ "unknown message type: " ++ show code
        Just t -> return t
    return Message { messageType = mtype
                   , messageId = fromIntegral msgId
                   , messageName = msgName
                   , messagePayload = payload
                   }


------------------------------------------------------------------------------

compactDeserialize :: TType a -> ByteString -> Either String (ByteString, Value a)
compactDeserialize t = runParser' (compactParser t)

compactParser :: TType a -> Parser (Value a)
compactParser typ = case typ of
  TBool      -> do
      n <- P.int8
      return $ VBool (n == 1)
  TByte      -> parseByte
  TDouble    -> parseDouble
  TInt16     -> parseInt16
  TInt32     -> parseInt32
  TInt64     -> parseInt64
  TBinary    -> parseBinary
  TStruct    -> parseStruct
  TMap       -> parseMap
  TSet       -> parseSet
  TList      -> parseList

intToZigZag :: Int64 -> Int64
intToZigZag n =
    (n `shiftL` 1) `xor` (n `shiftR` 63)

zigZagToInt :: Int64 -> Int64
zigZagToInt n =
    fromIntegral (n' `shiftR` 1) `xor` (-(n .&. 1))
  where
    n' = fromIntegral n :: Word64
    -- ensure no sign extension

parseVarint :: Parser Int64
parseVarint = go 0 0
  where
    go !val !shift = do
        when (shift >= 64) $ fail "parseVarint: too wide"
        n <- P.word8
        let val' = val .|. ((fromIntegral n .&. 0x7f) `shiftL` shift)
        if testBit n 7
          then go val' (shift + 7)
          else return val'

getCType :: Word8 -> Parser SomeCType
getCType code =
    maybe (fail $ "Unknown CType: " ++ show code) return $ fromCompactCode code

parseByte :: Parser (Value TByte)
parseByte = VByte <$> P.int8

parseDouble :: Parser (Value TDouble)
parseDouble = VDouble <$> P.double

parseInt16 :: Parser (Value TInt16)
parseInt16 = VInt16 . fromIntegral . zigZagToInt <$> parseVarint

parseInt32 :: Parser (Value TInt32)
parseInt32 = VInt32 . fromIntegral . zigZagToInt <$> parseVarint

parseInt64 :: Parser (Value TInt64)
parseInt64 = VInt64 . fromIntegral . zigZagToInt <$> parseVarint

parseBinary :: Parser (Value TBinary)
parseBinary = do
    n <- parseVarint
    when (n < 0) $
        fail $ "parseBinary: invalid length " ++ show n
    VBinary <$> P.take (fromIntegral n)


parseMap :: Parser (Value TMap)
parseMap = do
    count <- parseVarint
    case count of
      0 -> return VNullMap
      _ -> do
          tys <- P.word8
          SomeCType kctype <- getCType (tys `shiftR` 4)
          SomeCType vctype <- getCType (tys .&. 0x0f)

          let ktype = cTypeToTType kctype
              vtype = cTypeToTType vctype

          items <- FL.replicateM (fromIntegral count) $
              MapItem <$> compactParser ktype
                      <*> compactParser vtype
          return $ VMap items


parseCollection
    :: (forall a. IsTType a => FL.FoldList (Value a) -> Value b)
    -> Parser (Value b)
parseCollection buildValue = do
    sizeAndType <- P.word8
    SomeCType ctype <- getCType (sizeAndType .&. 0x0f)
    count <- case sizeAndType `shiftR` 4 of
                 0xf -> parseVarint
                 n   -> return $ fromIntegral n
    let vtype  = cTypeToTType ctype
    buildValue <$> FL.replicateM (fromIntegral count) (compactParser vtype)

parseSet :: Parser (Value TSet)
parseSet = parseCollection VSet

parseList :: Parser (Value TList)
parseList = parseCollection VList

parseStruct :: Parser (Value TStruct)
parseStruct = loop M.empty 0
  where
    loop :: HashMap Int16 SomeValue -> Int16 -> Parser (Value TStruct)
    loop fields lastFieldId = do
        sizeAndType <- P.word8
        SomeCType ctype <- getCType (sizeAndType .&. 0x0f)
        case ctype of
            CStop -> return (VStruct fields)
            _     -> do
                fieldId <- case sizeAndType `shiftR` 4 of
                               0x0 -> fromIntegral . zigZagToInt <$> parseVarint
                               n   -> return (lastFieldId + fromIntegral n)
                value <- case ctype of
                  CBoolTrue  -> return (SomeValue $ VBool True)
                  CBoolFalse -> return (SomeValue $ VBool False)
                  _          ->
                    let vtype = cTypeToTType ctype
                     in SomeValue <$> compactParser vtype
                loop (M.insert fieldId value fields) fieldId


------------------------------------------------------------------------------

compactSerialize :: forall a. IsTType a => Value a -> Builder
compactSerialize = case (ttype :: TType a) of
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
{-# INLINE compactSerialize #-}

serializeBinary :: Value TBinary -> Builder
serializeBinary (VBinary x) = string x
{-# INLINE serializeBinary #-}

serializeBool :: Value TBool -> Builder
serializeBool (VBool x) = compactCode $ if x then CBoolTrue else CBoolFalse
{-# INLINE serializeBool #-}

serializeByte :: Value TByte -> Builder
serializeByte (VByte x) = BB.int8 x
{-# INLINE serializeByte #-}

serializeDouble :: Value TDouble -> Builder
serializeDouble (VDouble x) = BB.doubleBE x
{-# INLINE serializeDouble #-}

serializeVarint :: Int64 -> Builder
serializeVarint = go . fromIntegral
  where
    -- Ensure we don't sign extend
    go :: Word64 -> Builder
    go n
      | complement 0x7f .&. n == 0 =
        BB.word8 $ fromIntegral n
      | otherwise =
        BB.word8 (0x80 .|. (fromIntegral n .&. 0x7f)) <>
        go (n `shiftR` 7)

serializeInt16 :: Value TInt16 -> Builder
serializeInt16 (VInt16 x) = serializeVarint $ intToZigZag $ fromIntegral x
{-# INLINE serializeInt16 #-}

serializeInt32 :: Value TInt32 -> Builder
serializeInt32 (VInt32 x) = serializeVarint $ intToZigZag $ fromIntegral x
{-# INLINE serializeInt32 #-}

serializeInt64 :: Value TInt64 -> Builder
serializeInt64 (VInt64 x) = serializeVarint $ intToZigZag x
{-# INLINE serializeInt64 #-}

serializeList :: Value TList -> Builder
serializeList (VList xs) = serializeCollection ttype xs
{-# INLINE serializeList #-}

serializeSet :: Value TSet -> Builder
serializeSet (VSet xs) = serializeCollection ttype xs
{-# INLINE serializeSet #-}

serializeStruct :: Value TStruct -> Builder
serializeStruct (VStruct fields) =
    loop 0 (sortBy (comparing fst) $ M.toList fields)
  where
    loop _ [] = compactCode CStop
    loop lastFieldId ((fieldId, val) : rest) =
        let x = case val of
                  SomeValue (VBool True)  -> writeFieldHeader CBoolTrue
                  SomeValue (VBool False) -> writeFieldHeader CBoolFalse
                  SomeValue (v :: Value a) ->
                      writeFieldHeader (tTypeToCType (ttype :: TType a)) <> compactSerialize v
        in x <> loop fieldId rest
      where
        writeFieldHeader :: CType a -> Builder
        writeFieldHeader ccode
          | fieldId > lastFieldId && fieldId - lastFieldId < 16
          = compactCode' ccode (fromIntegral $ fieldId - lastFieldId)
          | otherwise
          = compactCode ccode <> serializeVarint (intToZigZag $ fromIntegral fieldId)
{-# INLINE serializeStruct #-}

serializeMap :: Value TMap -> Builder
serializeMap VNullMap = BB.int8 0
serializeMap (VMap items) = serialize ttype ttype items
  where
    serialize
        :: (IsTType k, IsTType v)
        => TType k -> TType v -> FL.FoldList (MapItem k v) -> Builder
    serialize _  _  xs
      | FL.null xs        = BB.int8 0
    serialize kt vt xs =
        serializeVarint (fromIntegral size) <> BB.word8 typeByte <> body
      where
        code = toCompactCode . tTypeToCType
        typeByte = (code kt `shiftL` 4) .|. code vt
        (body, size) = FL.foldl' go (mempty, 0 :: Int32) xs
        go (prev, !c) (MapItem k v) =
            ( prev <> compactSerialize k <> compactSerialize v
            , c + 1
            )
{-# INLINE serializeMap #-}

serializeCollection
    :: IsTType a
    => TType a -> FL.FoldList (Value a) -> Builder
serializeCollection vtype xs =
    let go (prev, !c) item = (prev <> compactSerialize item, c + 1)
        (body, size) = FL.foldl' go (mempty, 0 :: Int32) xs
        type_and_size
          | size < 15 = typeCode' vtype (fromIntegral size)
          | otherwise = typeCode' vtype 0xf <> serializeVarint (fromIntegral size)
    in type_and_size <> body
{-# INLINE serializeCollection #-}

------------------------------------------------------------------------------


messageCode :: MessageType -> Word8
messageCode Call      = 1
messageCode Reply     = 2
messageCode Exception = 3
messageCode Oneway    = 4
{-# INLINE messageCode #-}


fromMessageCode :: Word8 -> Maybe MessageType
fromMessageCode 1 = Just Call
fromMessageCode 2 = Just Reply
fromMessageCode 3 = Just Exception
fromMessageCode 4 = Just Oneway
fromMessageCode _ = Nothing
{-# INLINE fromMessageCode #-}


data TStop

instance IsTType TStop where
    ttype = error "ttype TStop"

-- | A compact message type.
data CType a where
    CStop      :: CType TStop
    CBoolTrue  :: CType TBool
    CBoolFalse :: CType TBool
    CByte      :: CType TByte
    CInt16     :: CType TInt16
    CInt32     :: CType TInt32
    CInt64     :: CType TInt64
    CDouble    :: CType TDouble
    CBinary    :: CType TBinary
    CList      :: CType TList
    CSet       :: CType TSet
    CMap       :: CType TMap
    CStruct    :: CType TStruct


data SomeCType where
    SomeCType :: forall a. IsTType a => CType a -> SomeCType


-- | Map a TType to its type code.
toCompactCode :: CType a -> Word8
toCompactCode CStop      = 0
toCompactCode CBoolTrue  = 1
toCompactCode CBoolFalse = 2
toCompactCode CByte      = 3
toCompactCode CInt16     = 4
toCompactCode CInt32     = 5
toCompactCode CInt64     = 6
toCompactCode CDouble    = 7
toCompactCode CBinary    = 8
toCompactCode CList      = 9
toCompactCode CSet       = 10
toCompactCode CMap       = 11
toCompactCode CStruct    = 12
{-# INLINE toCompactCode #-}


-- | Map a type code to the corresponding TType.
fromCompactCode :: Word8 -> Maybe SomeCType
fromCompactCode 0  = Just $ SomeCType CStop
fromCompactCode 1  = Just $ SomeCType CBoolTrue
fromCompactCode 2  = Just $ SomeCType CBoolFalse
fromCompactCode 3  = Just $ SomeCType CByte
fromCompactCode 4  = Just $ SomeCType CInt16
fromCompactCode 5  = Just $ SomeCType CInt32
fromCompactCode 6  = Just $ SomeCType CInt64
fromCompactCode 7  = Just $ SomeCType CDouble
fromCompactCode 8  = Just $ SomeCType CBinary
fromCompactCode 9  = Just $ SomeCType CList
fromCompactCode 10 = Just $ SomeCType CSet
fromCompactCode 11 = Just $ SomeCType CMap
fromCompactCode 12 = Just $ SomeCType CStruct
fromCompactCode _  = Nothing
{-# INLINE fromCompactCode #-}

tTypeToCType :: TType a -> CType a
tTypeToCType TBool      = CBoolTrue
tTypeToCType TByte      = CByte
tTypeToCType TInt16     = CInt16
tTypeToCType TInt32     = CInt32
tTypeToCType TInt64     = CInt64
tTypeToCType TDouble    = CDouble
tTypeToCType TBinary    = CBinary
tTypeToCType TList      = CList
tTypeToCType TSet       = CSet
tTypeToCType TMap       = CMap
tTypeToCType TStruct    = CStruct

cTypeToTType :: CType a -> TType a
cTypeToTType CStop      = error "cTypeToTType: CStop"
cTypeToTType CBoolTrue  = TBool
cTypeToTType CBoolFalse = TBool
cTypeToTType CByte      = TByte
cTypeToTType CInt16     = TInt16
cTypeToTType CInt32     = TInt32
cTypeToTType CInt64     = TInt64
cTypeToTType CDouble    = TDouble
cTypeToTType CBinary    = TBinary
cTypeToTType CList      = TList
cTypeToTType CSet       = TSet
cTypeToTType CMap       = TMap
cTypeToTType CStruct    = TStruct

------------------------------------------------------------------------------


string :: ByteString -> Builder
string b = serializeVarint (fromIntegral $ B.length b) <> BB.byteString b
{-# INLINE string #-}

compactCode :: CType a -> Builder
compactCode = BB.word8 . toCompactCode
{-# INLINE compactCode #-}

compactCode' :: CType a  -- ^ The compact type code
             -> Word8    -- ^ a four-bit (unshifted) payload
             -> Builder
compactCode' ty payload =
    BB.word8 (toCompactCode ty .|. (fromIntegral payload `shiftL` 4))
{-# INLINE compactCode' #-}

typeCode' :: TType a -> Word8 -> Builder
typeCode' ty = compactCode' (tTypeToCType ty)
{-# INLINE typeCode' #-}
