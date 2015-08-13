module Pinch.Protocol.BinarySpec (spec) where

import Data.ByteString       (ByteString)
import Data.ByteString.Lazy  (toStrict)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.ByteString.Builder as BB

import Pinch.Arbitrary       ()
import Pinch.Protocol        (Protocol (..))
import Pinch.Protocol.Binary (binaryProtocol)

import qualified Pinch.Internal.TType as T
import qualified Pinch.Internal.Value as V


serialize :: V.Value a -> ByteString
serialize = toStrict . BB.toLazyByteString . serializeValue binaryProtocol


deserialize :: T.IsTType a => ByteString -> Either String (V.Value a)
deserialize = deserializeValue binaryProtocol


spec :: Spec
spec = describe "BinaryProtocol" $

    prop "can roundtrip values" $ \(V.SomeValue someVal) ->
        deserialize (serialize someVal) === Right someVal
