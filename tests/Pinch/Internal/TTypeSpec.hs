module Pinch.Internal.TTypeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Pinch.Arbitrary ()

import qualified Pinch.Internal.TType as T

spec :: Spec
spec = describe "TType" $

    -- silly sanity test
    prop "has matching IsTType results" $ \someType ->
        case someType of
            T.SomeTType typ -> T.ttype `shouldBe` typ
