{-# LANGUAGE ScopedTypeVariables #-}
module Pinch.Internal.ValueSpec (spec) where

import Data.Function         (on)
import Data.List             (nubBy)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Pinch.Arbitrary ()

import qualified Pinch.Internal.FoldList as FL
import qualified Pinch.Internal.TType    as T
import qualified Pinch.Internal.Value    as V


spec :: Spec
spec = describe "Value" $ do

    prop "is equal to itself" $ \(V.SomeValue v) ->
        v === v

    prop "can be cast" $ \(V.SomeValue v) ->
        V.castValue v === Just v

    prop "list equality" $ \(xs, ys :: [V.Value T.TInt32]) ->
        (xs /= ys) ==>
            (V.VList (FL.fromFoldable xs) /= V.VList (FL.fromFoldable ys))
                === True

    prop "sets are unordered" $ \(xs :: [V.Value T.TInt32]) -> do
        xs' <- generate (shuffle xs)
        V.VSet (FL.fromFoldable xs) `shouldBe` V.VSet (FL.fromFoldable xs')

    prop "maps are unordered" $ \(xs0 :: [V.MapItem T.TInt32 T.TByte]) -> do
        let xs = nubBy ((==) `on` (\(V.MapItem k _) -> k)) xs0
        xs' <- generate (shuffle xs)
        V.VMap (FL.fromFoldable xs) `shouldBe` V.VMap (FL.fromFoldable xs')
