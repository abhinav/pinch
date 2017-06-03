{-# LANGUAGE CPP #-}
module Pinch.Internal.BuilderParserSpec (spec) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.ByteString as B

import Pinch.Arbitrary

import qualified Pinch.Internal.Builder as BB
import qualified Pinch.Internal.Parser  as P

roundTrip
    :: (Show a, Eq a)
    => P.Parser a -> (a -> BB.Builder) -> a -> Expectation
roundTrip parser builder a =
        P.runParser parser (BB.runBuilder (builder a)) `shouldBe` Right a

spec :: Spec
spec = describe "Builder and Parser" $ do

    prop "can round trip 8-bit integers" $
        roundTrip P.int8 BB.int8

    prop "can round trip 16-bit integers" $
        roundTrip P.int16 BB.int16BE

    prop "can round trip 32-bit integers" $
        roundTrip P.int32 BB.int32BE

    prop "can round trip 64-bit integers" $
        roundTrip P.int64 BB.int64BE

    prop "can round trip 64-bit little endian integers" $
        roundTrip P.int64LE BB.int64LE

    prop "can round trip doubles" $
        roundTrip P.double BB.doubleBE

    prop "can round trip little endian doubles" $
        roundTrip P.doubleLE BB.doubleLE

    prop "can round trip bytestrings" $ \(SomeByteString bs) ->
        roundTrip (P.take (B.length bs)) BB.byteString bs

    prop "can round trip primitive appends" $
        roundTrip
            ((,) <$> P.int32 <*> P.int64)
            (\(a, b) -> BB.int32BE a <> BB.int64BE b)

    prop "can round trip byteString appends" $
        \(SomeByteString l) (SomeByteString r) ->
            roundTrip
                ((,) <$> P.take (B.length l) <*> P.take (B.length r))
                (\(a, b) -> BB.byteString a <> BB.byteString b)
                (l, r)

    prop "can round trip byteString-primitive appends" $
        \(SomeByteString l) r ->
            roundTrip
                ((,) <$> P.take (B.length l) <*> P.double)
                (\(a, b) -> BB.byteString a <> BB.doubleBE b)
                (l, r)

    prop "can round trip primitive-byteString appends" $
        \l (SomeByteString r) ->
            roundTrip
                ((,) <$> P.int64 <*> P.take (B.length r))
                (\(a, b) -> BB.int64BE a <> BB.byteString b)
                (l, r)
