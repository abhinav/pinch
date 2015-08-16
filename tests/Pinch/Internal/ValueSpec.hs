module Pinch.Internal.ValueSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Pinch.Arbitrary ()

import qualified Pinch.Internal.Value as V

spec :: Spec
spec = describe "Value" $ do

    prop "is equal to itself" $ \(V.SomeValue v) ->
        v === v

    prop "can be cast via SomeValue" $ \(V.SomeValue v) ->
        V.castValue (V.SomeValue v) === Just v
