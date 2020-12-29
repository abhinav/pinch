{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pinch.Protocol.CompactSpec (spec) where

import Data.ByteString       (ByteString)
import Data.Word             (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.ByteString as B

import Pinch.Arbitrary        ()
import Pinch.Internal.Builder (runBuilder)
import Pinch.Internal.Message
import Pinch.Internal.TType
import Pinch.Internal.Util
import Pinch.Internal.Value   (SomeValue (..), Value (..))
import Pinch.Protocol
import Pinch.Protocol.Compact (compactProtocol)


serialize :: IsTType a => Value a -> ByteString
serialize = runBuilder . serializeValue compactProtocol


deserialize :: IsTType a => ByteString -> Either String (Value a)
deserialize = deserializeValue compactProtocol


serializeMsg :: Message -> ByteString
serializeMsg = runBuilder . serializeMessage compactProtocol

deserializeMsg :: ByteString -> Either String Message
deserializeMsg = deserializeMessage compactProtocol


-- | For each given pair, verifies that parsing the byte array yields the
-- value, and that serializing the value yields the byte array.
readWriteCases :: IsTType a => [([Word8], Value a)] -> Expectation
readWriteCases = mapM_ . uncurry $ \bytes value -> do
    let bs = B.pack bytes
    deserialize bs  `shouldBe` Right value
    serialize value `shouldBe` bs


readWriteMessageCases :: [([Word8], Message)] -> Expectation
readWriteMessageCases = mapM_ . uncurry $ \bytes msg -> do
    let bs = B.pack bytes
    deserializeMsg bs  `shouldBe` Right msg
    serializeMsg msg `shouldBe` bs


-- | For each pair, verifies that if the given TType is parsed, the request
-- fails to parse because the type ID was invalid.
invalidTypeIDCases :: [(SomeTType, [Word8])] -> Expectation
invalidTypeIDCases = mapM_ . uncurry $ \(SomeTType t) v -> go t v
  where
    go :: forall a. IsTType a => TType a -> [Word8] -> Expectation
    go _ bytes =
        case deserialize (B.pack bytes) :: Either String (Value a) of
            Right v -> expectationFailure $
              "Expected " ++ show bytes ++ " to fail to parse. " ++
              "Got: " ++ show v
            Left msg -> msg `shouldContain` "Unknown CType"


-- | For each pair, verifies that if the given TType is parsed, the request
-- fails to parse because the input was too short.
tooShortCases :: [(SomeTType, [Word8])] -> Expectation
tooShortCases = mapM_ . uncurry $ \(SomeTType t) v -> go t v
  where
    go :: forall a. IsTType a => TType a -> [Word8] -> Expectation
    go _ bytes =
        case deserialize (B.pack bytes) :: Either String (Value a) of
            Right v -> expectationFailure $
              "Expected " ++ show bytes ++ " to fail to parse. " ++
              "Got: " ++ show v
            Left msg -> msg `shouldContain` "too few bytes"


spec :: Spec
spec = describe "CompactProtocol" $ do

    prop "can roundtrip values" $ \(SomeValue someVal) ->
        deserialize (serialize someVal) === Right someVal

    prop "can roundtrip messages" $ \(msg :: Message) ->
        deserializeMsg (serializeMsg msg) == Right msg

    it "can read and write booleans" $ readWriteCases
        [ ([0x01], vbool True)
        , ([0x02], vbool False)
        ]

    it "can read and write binary" $ readWriteCases
        [ ([ 0x00 ], vbin "")
        , ([ 0x05                          -- length = 5
           , 0x68, 0x65, 0x6c, 0x6c, 0x6f  -- hello
           ], vbin "hello")
        ]

    it "can read and write 8-bit integers" $ readWriteCases
        [ ([0x01], vbyt 1)
        , ([0x05], vbyt 5)
        , ([0x7f], vbyt 127)
        , ([0xff], vbyt -1)
        , ([0x80], vbyt -128)
        ]

    it "can read and write 16-bit integers" $ readWriteCases
        [ ([0x02],             vi16 1)
        , ([0xfe, 0x03],       vi16 255)
        , ([0x80, 0x04],       vi16 256)
        , ([0x82, 0x04],       vi16 257)
        , ([0xfe, 0xff, 0x03], vi16 32767)
        , ([0x01],             vi16 -1)
        , ([0x03],             vi16 -2)
        , ([0xff, 0x03],       vi16 -256)
        , ([0xfd, 0x03],       vi16 -255)
        , ([0xff, 0xff, 0x03], vi16 -32768)
        ]

    it "can read and write 32-bit integers" $ readWriteCases
        [ ([0x02],                         vi32 1)
        , ([0xfe, 0x03],                   vi32 255)
        , ([0xfe, 0xff, 0x07],             vi32 65535)
        , ([0xfe, 0xff, 0xff, 0x0f],       vi32 16777215)
        , ([0xfe, 0xff, 0xff, 0xff, 0x0f], vi32 2147483647)
        , ([0x01],                         vi32 -1)
        , ([0xff, 0x03],                   vi32 -256)
        , ([0xff, 0xff, 0x07],             vi32 -65536)
        , ([0xff, 0xff, 0xff, 0x0f],       vi32 -16777216)
        , ([0xff, 0xff, 0xff, 0xff, 0x0f], vi32 -2147483648)
        ]

    it "can read and write 64-bit integers" $ readWriteCases
        [ ([0x02],                                           vi64 1)
        , ([0xfe, 0xff, 0xff, 0xff, 0x1f],                   vi64 4294967295)
        , ([0xfe, 0xff, 0xff, 0xff, 0xff, 0x3f],             vi64 1099511627775)
        , ([0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],       vi64 281474976710655)
        , ([0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01], vi64 72057594037927935)
        , ([0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01], vi64 9223372036854775807)
        , ([0x01],                                           vi64 -1)
        , ([0xff, 0xff, 0xff, 0xff, 0x1f],                   vi64 -4294967296)
        , ([0xff, 0xff, 0xff, 0xff, 0xff, 0x3f],             vi64 -1099511627776)
        , ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],       vi64 -281474976710656)
        , ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01], vi64 -72057594037927936)
        , ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01], vi64 -9223372036854775808)
        ]

    it "can read and write doubles" $ readWriteCases
        [ ([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], vdub 0.0)
        , ([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f], vdub 1.0)
        , ([0x38, 0xdf, 0x06, 0x00, 0x00, 0x00, 0xf0, 0x3f], vdub 1.0000000001)
        , ([0x9a, 0x99, 0x99, 0x99, 0x99, 0x99, 0xf1, 0x3f], vdub 1.1)
        , ([0x9a, 0x99, 0x99, 0x99, 0x99, 0x99, 0xf1, 0xbf], vdub -1.1)
        , ([0x18, 0x2d, 0x44, 0x54, 0xfb, 0x21, 0x09, 0x40], vdub 3.141592653589793)
        , ([0x38, 0xdf, 0x06, 0x00, 0x00, 0x00, 0xf0, 0xbf], vdub -1.0000000001)
        ]

    it "can read and write structs" $ readWriteCases
        [ ([0x00], vstruct [])

        , ([ 0x15                    -- ttype = i32, field ID = 1
           , 0x54                    -- 42
           , 0x00                    -- stop
           ], vstruct [(1, vi32_ 42)])

        , ([ 0x11                    -- ttype = bool true, field ID = 1
           , 0x52                    -- ttype = bool false, field ID = 6
           , 0x23, 0x2a              -- ttype = byte, field ID = 8, byte 42
           , 0x03, 0x40, 0x2b        -- ttype = byte, field ID = 32, byte 42
           , 0x00                    -- stop
           ], vstruct [(1, vbool_ True), (6, vbool_ False), (8, vbyt_ 42), (32, vbyt_ 43)])

        , ([ 0x29                    -- ttype = list, field ID = 2
           , 0x28

           , 0x03, 0x66, 0x6f, 0x6f  -- "foo"
           , 0x03, 0x62, 0x61, 0x72  -- "bar"

           , 0x00                    -- stop
           ], vstruct
           [ (2, vlist_ [vbin "foo", vbin "bar"])
           ])
        ]

    it "can read and write maps" $ readWriteCases
        [ ([ 0x00
           ], vmap ([] :: [(Value TBool, Value TByte)]))
        , ([ 0x01, 0x89              -- ktype = binary, vtype = list

           -- "world"
           , 0x05                          -- length = 5
           , 0x77, 0x6f, 0x72, 0x6c, 0x64  -- world

           -- [1, 2, 3]
           , 0x33                          -- type = byte, count = 3
           , 0x01, 0x02, 0x03              -- 1, 2, 3
           ], vmap
           [ (vbin "world", vlist [vbyt 1, vbyt 2, vbyt 3])
           ])
        ]

    it "can read and write sets" $ readWriteCases
        [ ([0x01
           ], vset ([] :: [Value TBool]))
        , ([ 0x11, 0x01
           ], vset [vbool True])
        ]

    it "can read and write lists" $ readWriteCases
        [ ([0x01
           ], vlist ([] :: [Value TBool]))
        , ([ 0x51, 0x01, 0x02, 0x02
           , 0x01, 0x01
           ], vlist
               [ vbool True
               , vbool False
               , vbool False
               , vbool True
               , vbool True
               ])
        ]

    it "fails if the input is too short" $ tooShortCases
        [ (SomeTType TBool, [])
        , (SomeTType TByte, [])
        , (SomeTType TInt16, [])
        , (SomeTType TInt32, [])
        , (SomeTType TInt64, [])
        , (SomeTType TDouble, [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07])
        , (SomeTType TBinary, [0x01])

        , (SomeTType TMap, [0x02])
        , (SomeTType TMap, [0x02, 0x33])
        , (SomeTType TMap, [0x02, 0x33, 0x01])

        , (SomeTType TSet, [0x2a])
        , (SomeTType TSet, [0x2a, 0x33, 0x00])

        , (SomeTType TList, [0x29])
        , (SomeTType TList, [0x29, 0x33])
        ]

    it "denies invalid type IDs" $ invalidTypeIDCases
        [ (SomeTType TStruct, [0x0d, 0x00, 0x01])
        , (SomeTType TMap, [0x1a, 0xd1, 0x00])
        , (SomeTType TSet, [0x1d])
        , (SomeTType TList, [0x1d])
        ]

    it "can read and write messages" $ readWriteMessageCases
        [ ([ 0x82                                   -- Protocol id
           , 0x21                                   -- Version and Type = Call
           , 0x2a                                   -- seqId = 42
           , 0x06                                   -- name length = 6
           , 0x67, 0x65, 0x74, 0x46, 0x6f, 0x6f     -- 'getFoo'
           , 0x00                                   -- stop
           ], Message "getFoo" Call 42 (vstruct []))
        , ([ 0x82                                   -- Protocol id
           , 0x41                                   -- Version and Type = Reply
           , 0x01                                   -- seqId = 01
           , 0x06                                   -- name length = 6
           , 0x73, 0x65, 0x74, 0x42, 0x61, 0x72     -- 'setBar'
           , 0x00                                   -- stop
           ], Message "setBar" Reply 1 (vstruct []))
        ]
