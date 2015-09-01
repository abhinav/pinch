{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pinch.Protocol.BinarySpec (spec) where

import Data.ByteString       (ByteString)
import Data.ByteString.Lazy  (toStrict)
import Data.Word             (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB

import Pinch.Arbitrary       ()
import Pinch.Internal.TType
import Pinch.Internal.Util
import Pinch.Internal.Value  (SomeValue (..), Value (..))
import Pinch.Protocol        (Protocol (..))
import Pinch.Protocol.Binary (binaryProtocol)


serialize :: IsTType a => Value a -> ByteString
serialize = toStrict . BB.toLazyByteString . snd . serializeValue binaryProtocol


deserialize :: IsTType a => ByteString -> Either String (Value a)
deserialize = deserializeValue binaryProtocol


-- | For each given pair, verifies that parsing the byte array yields the
-- value, and that serializing the value yields the byte array.
readWriteCases :: IsTType a => [([Word8], Value a)] -> Expectation
readWriteCases = mapM_ . uncurry $ \bytes value -> do
    let bs = B.pack bytes
    deserialize bs  `shouldBe` Right value
    serialize value `shouldBe` bs


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
            Left msg -> msg `shouldContain` "Unknown TType"


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
            Left msg -> msg `shouldContain` "Input is too short"


spec :: Spec
spec = describe "BinaryProtocol" $ do

    prop "can roundtrip values" $ \(SomeValue someVal) ->
        deserialize (serialize someVal) === Right someVal

    it "can read and write booleans" $ readWriteCases
        [ ([0x01], vbool True)
        , ([0x00], vbool False)
        ]

    it "can read and write binary" $ readWriteCases
        [ ([ 0x00, 0x00, 0x00, 0x00 ], vbin "")
        , ([ 0x00, 0x00, 0x00, 0x05        -- length = 5
           , 0x68, 0x65, 0x6c, 0x6c, 0x6f  -- hello
           ], vbin "hello")
        ]

    it "can read and write structs" $ readWriteCases
        [ ([0x00], vstruct [])

        , ([ 0x08                    -- ttype = i32
           , 0x00, 0x01              -- field ID = 1
           , 0x00, 0x00, 0x00, 0x2a  -- 42
           , 0x00                    -- stop
           ], vstruct [(1, vi32_ 42)])

        , ([ 0x0F       -- ttype = list
           , 0x00, 0x02 -- field ID = 2

           , 0x0B                    -- ttype binary
           , 0x00, 0x00, 0x00, 0x02  -- size = 2

           , 0x00, 0x00, 0x00, 0x03  -- length = 3
           , 0x66, 0x6f, 0x6f        -- foo

           , 0x00, 0x00, 0x00, 0x03  -- length = 3
           , 0x62, 0x61, 0x72        -- bar

           , 0x00
           ], vstruct
           [ (2, vlist_ [vbin "foo", vbin "bar"])
           ])
        ]

    it "can read and write maps" $ readWriteCases
        [ ([ 0x02, 0x03              -- ktype = bool, vtype = byte
           , 0x00, 0x00, 0x00, 0x00  -- count = 0
           ], vmap ([] :: [(Value TBool, Value TByte)]))
        , ([ 0x0B, 0x0F              -- ktype = binary, vtype = list
           , 0x00, 0x00, 0x00, 0x01  -- count = 1

           -- "world"
           , 0x00, 0x00, 0x00, 0x05        -- length = 5
           , 0x77, 0x6f, 0x72, 0x6c, 0x64  -- world

           -- [1, 2, 3]
           , 0x03                          -- type = byte
           , 0x00, 0x00, 0x00, 0x03        -- count = 3
           , 0x01, 0x02, 0x03              -- 1, 2, 3
           ], vmap
           [ (vbin "world", vlist [vbyt 1, vbyt 2, vbyt 3])
           ])
        ]

    it "can read and write sets" $ readWriteCases
        [ ([0x02, 0x00, 0x00, 0x00, 0x00
           ], vset ([] :: [Value TBool]))
        , ([ 0x02
           , 0x00, 0x00, 0x00, 0x01
           , 0x01
           ], vset [vbool True])
        ]

    it "can read and write lists" $ readWriteCases
        [ ([0x02, 0x00, 0x00, 0x00, 0x00
           ], vlist ([] :: [Value TBool]))
        , ([ 0x02
           , 0x00, 0x00, 0x00, 0x05
           , 0x01, 0x00, 0x00, 0x01, 0x01
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
        , (SomeTType TInt16, [0x01])
        , (SomeTType TInt32, [0x01, 0x02, 0x03])
        , (SomeTType TInt64, [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07])
        , (SomeTType TDouble, [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07])
        , (SomeTType TBinary, [0x00, 0x00, 0x00])
        , (SomeTType TBinary, [0x00, 0x00, 0x00, 0x01])
        , (SomeTType TBinary, [0x00, 0x00, 0x00, 0x02, 0x01])

        , (SomeTType TMap, [0x02])
        , (SomeTType TMap, [0x02, 0x03])
        , (SomeTType TMap, [0x02, 0x03, 0x00, 0x00, 0x00])
        , (SomeTType TMap, [0x02, 0x03, 0x00, 0x00, 0x00, 0x01])
        , (SomeTType TMap, [0x02, 0x03, 0x00, 0x00, 0x00, 0x01, 0x01])
        , (SomeTType TMap,
           [0x02, 0x03, 0x00, 0x00, 0x00, 0x02, 0x01, 0x01, 0x01])

        , (SomeTType TSet, [0x02])
        , (SomeTType TSet, [0x02, 0x00, 0x00, 0x00])
        , (SomeTType TSet, [0x02, 0x00, 0x00, 0x00, 0x01])
        , (SomeTType TSet, [0x02, 0x00, 0x00, 0x00, 0x02, 0x01])

        , (SomeTType TList, [0x02])
        , (SomeTType TList, [0x02, 0x00, 0x00, 0x00])
        , (SomeTType TList, [0x02, 0x00, 0x00, 0x00, 0x01])
        , (SomeTType TList, [0x02, 0x00, 0x00, 0x00, 0x02, 0x01])
        ]

    it "denies invalid type IDs" $ invalidTypeIDCases
        [ (SomeTType TStruct, [0x01, 0x00, 0x01])
        , (SomeTType TMap, [0x05, 0x07, 0x00, 0x00, 0x00, 0x00])
        , (SomeTType TSet, [0x09, 0x00, 0x00, 0x00, 0x00])
        , (SomeTType TList, [0x10, 0x00, 0x00, 0x00, 0x00])
        ]
