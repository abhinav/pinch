{-# LANGUAGE ScopedTypeVariables #-}
module Pinch.Internal.FoldListSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Foldable           as F
import qualified Data.IORef              as IORef
import qualified Data.Vector             as V
import qualified Pinch.Internal.FoldList as FL

spec :: Spec
spec = describe "FoldList" $ do

    prop "is equal to itself" $ \(xs :: [Int]) ->
        FL.fromFoldable xs === FL.fromFoldable xs

    prop "is equal if the elements are the same" $ \(xs :: [Int]) ->
        FL.fromFoldable xs === FL.fromFoldable (V.fromList xs)

    prop "can convert to and from lists" $ \(xs :: [Int]) ->
        F.toList (FL.fromFoldable xs) === xs

    describe "replicate" $ do

        it "can be empty" $
            F.toList (FL.replicate 0 'a') `shouldBe` []

        it "produces the requested number of duplicates" $
            F.toList (FL.replicate 5 'a') `shouldBe` "aaaaa"

    describe "replicateM" $

        it "preserves order" $ do
            ref <- IORef.newIORef (1 :: Int)
            let get = IORef.atomicModifyIORef' ref (\a -> (a + 1, a))

            result <- FL.replicateM 100 get
            F.toList result `shouldBe` [1..100]

