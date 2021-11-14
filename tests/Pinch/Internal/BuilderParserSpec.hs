{-# LANGUAGE CPP #-}
module Pinch.Internal.BuilderParserSpec (spec) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.ByteString as B
import qualified Data.Serialize.IEEE754 as G
import qualified Data.Serialize.Get as G

import Pinch.Arbitrary

import qualified Pinch.Internal.Builder as BB

roundTrip
    :: (Show a, Eq a)
    => G.Get a -> (a -> BB.Builder) -> a -> Expectation
roundTrip parser builder a =
        G.runGet parser (BB.runBuilder (builder a)) `shouldBe` Right a

spec :: Spec
spec = describe "Builder and Parser" $ do

    prop "can round trip 8-bit integers" $
        roundTrip G.getInt8 BB.int8

    prop "can round trip 16-bit integers" $
        roundTrip G.getInt16be BB.int16BE

    prop "can round trip 32-bit integers" $
        roundTrip G.getInt32be BB.int32BE

    prop "can round trip 64-bit integers" $
        roundTrip G.getInt64be BB.int64BE

    prop "can round trip 64-bit little endian integers" $
        roundTrip G.getInt64le BB.int64LE

    prop "can round trip doubles" $
        roundTrip G.getFloat64be BB.doubleBE

    prop "can round trip little endian doubles" $
        roundTrip G.getFloat64le BB.doubleLE

    prop "can round trip bytestrings" $ \(SomeByteString bs) ->
        roundTrip (G.getByteString (B.length bs)) BB.byteString bs

    prop "can round trip primitive appends" $
        roundTrip
            ((,) <$> G.getInt32be <*> G.getInt64be)
            (\(a, b) -> BB.int32BE a <> BB.int64BE b)

    prop "can round trip byteString appends" $
        \(SomeByteString l) (SomeByteString r) ->
            roundTrip
                ((,) <$> G.getByteString (B.length l) <*> G.getByteString (B.length r))
                (\(a, b) -> BB.byteString a <> BB.byteString b)
                (l, r)

    prop "can round trip byteString-primitive appends" $
        \(SomeByteString l) r ->
            roundTrip
                ((,) <$> G.getByteString (B.length l) <*> G.getFloat64be)
                (\(a, b) -> BB.byteString a <> BB.doubleBE b)
                (l, r)

    prop "can round trip primitive-byteString appends" $
        \l (SomeByteString r) ->
            roundTrip
                ((,) <$> G.getInt64be <*> G.getByteString (B.length r))
                (\(a, b) -> BB.int64BE a <> BB.byteString b)
                (l, r)
