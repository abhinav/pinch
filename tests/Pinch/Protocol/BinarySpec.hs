{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Pinch.Protocol.BinarySpec (spec) where

import Data.ByteString       (ByteString)
import Data.ByteString.Lazy  (toStrict)
import Data.Word             (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.HashMap.Strict     as HM

import Pinch.Arbitrary       ()
import Pinch.Internal.TType  (IsTType)
import Pinch.Internal.Value  (SomeValue (..), Value (..))
import Pinch.Protocol        (Protocol (..))
import Pinch.Protocol.Binary (binaryProtocol)


serialize :: IsTType a => Value a -> ByteString
serialize = toStrict . BB.toLazyByteString . serializeValue binaryProtocol


deserialize :: IsTType a => ByteString -> Either String (Value a)
deserialize = deserializeValue binaryProtocol


-- | Verifies that for each given pair, verifies that parsing the byte array
-- yields the value, and that serializing the value yields the byte array.
readWriteCases :: IsTType a => [([Word8], Value a)] -> Expectation
readWriteCases = mapM_ . uncurry $ \bytes value -> do
    let bs = B.pack bytes
    deserialize bs  `shouldBe` Right value
    serialize value `shouldBe` bs


spec :: Spec
spec = describe "BinaryProtocol" $ do

    prop "can roundtrip values" $ \(SomeValue someVal) ->
        deserialize (serialize someVal) === Right someVal

    it "can read and write booleans" $ readWriteCases
        [ ([0x01], VBool True)
        , ([0x00], VBool False)
        ]

    it "can read and write binary" $ readWriteCases
        [ ([ 0x00, 0x00, 0x00, 0x00 ], VBinary "")
        , ([ 0x00, 0x00, 0x00, 0x05        -- length = 5
           , 0x68, 0x65, 0x6c, 0x6c, 0x6f  -- 'h', 'e', 'l', 'l', 'o'
           ], VBinary "hello")
        ]

    it "can read and write structs" $ readWriteCases
        [ ([0x00], VStruct HM.empty)

        , ([ 0x08                    -- ttype = i32
           , 0x00, 0x01              -- field ID = 1
           , 0x00, 0x00, 0x00, 0x2a  -- 42
           , 0x00                    -- stop
           ], VStruct (HM.singleton 1 (SomeValue $ VInt32 42)))

        , ([ 0x03       -- ttype = byte
           , 0x00, 0x01 -- field ID = 1
           , 0x80       -- -128

           , 0x0F       -- ttype = list
           , 0x00, 0x02 -- field ID = 2

           , 0x0B                    -- ttype binary
           , 0x00, 0x00, 0x00, 0x02  -- size = 2

           , 0x00, 0x00, 0x00, 0x03  -- length = 3
           , 0x66, 0x6f, 0x6f        -- 'f', 'o', 'o'

           , 0x00, 0x00, 0x00, 0x03  -- length = 3
           , 0x62, 0x61, 0x72        -- 'b', 'a', 'r'

           , 0x00
           ], VStruct
           [ (1, SomeValue $ VByte (-128))
           , (2, SomeValue $ VList [VBinary "foo", VBinary "bar"])
           ])
        ]
