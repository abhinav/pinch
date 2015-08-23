{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Pinch.GenericSpec (spec) where

import Data.ByteString       (ByteString)
import Data.Int              (Int32, Int8)
import Data.Set              (Set)
import GHC.Generics          (Generic)
import GHC.TypeLits          ()
import Test.Hspec
import Test.Hspec.QuickCheck

import Pinch.TestUtils

import qualified Pinch.Generic            as G
import qualified Pinch.Internal.Pinchable as P
import qualified Pinch.Internal.TType     as T
import qualified Pinch.Internal.Value     as V


data AnEnum
    = EnumA (G.Enumeration 1)
    | EnumB (G.Enumeration 2)
    | EnumC (G.Enumeration 3)
  deriving (Show, Ord, Eq, Generic)

instance P.Pinchable AnEnum


enumSpec :: Spec
enumSpec = describe "Enum" $ do

    it "can pinch and unpinch" $ do
        P.pinch (EnumA G.enum) `shouldBe` V.VInt32 1
        P.pinch (EnumB G.enum) `shouldBe` V.VInt32 2
        P.pinch (EnumC G.enum) `shouldBe` V.VInt32 3

        P.unpinch (V.VInt32 1) `shouldBe` Right (EnumA G.enum)
        P.unpinch (V.VInt32 2) `shouldBe` Right (EnumB G.enum)
        P.unpinch (V.VInt32 3) `shouldBe` Right (EnumC G.enum)

    it "reject invalid values" $
        (P.unpinch :: V.Value T.TInt32 -> Either String AnEnum)
          (V.VInt32 4) `leftShouldContain` "Couldn't match enum value 4"

data AUnion
    = UnionDouble (G.Field 1 Double)
    | UnionByte   (G.Field 2 Int8)
    | UnionSet    (G.Field 5 (Set AnEnum))
  deriving (Show, Ord, Eq, Generic)

instance P.Pinchable AUnion


unionSpec :: Spec
unionSpec = describe "Union" $ do

    prop "can pinch (1)" $ \dub ->
        P.pinch (UnionDouble (G.putField dub)) `shouldBe`
            V.VStruct [(1, V.SomeValue $ V.VDouble dub)]

    prop "can pinch (2)" $ \byt ->
        P.pinch (UnionByte (G.putField byt)) `shouldBe`
            V.VStruct [(2, V.SomeValue $ V.VByte byt)]

    it "can pinch (3)" $
        P.pinch (UnionSet (G.putField [EnumA G.enum, EnumB G.enum]))
            `shouldBe`
              V.VStruct
                [(5, V.SomeValue $ V.VSet [V.VInt32 1, V.VInt32 2])]

    it "can unpinch" $ do
        P.unpinch (V.VStruct [(1, V.SomeValue $ V.VDouble 12.34)])
            `shouldBe` Right (UnionDouble $ G.putField 12.34)

        P.unpinch (V.VStruct [(2, V.SomeValue $ V.VByte 123)])
            `shouldBe` Right (UnionByte $ G.putField 123)

        P.unpinch
            (V.VStruct [(5, V.SomeValue $ V.VSet [V.VInt32 1, V.VInt32 2])])
            `shouldBe` Right
                (UnionSet $ G.putField [EnumA G.enum, EnumB G.enum])

    it "reject invalid types" $ do
        (P.unpinch :: V.Value T.TUnion -> Either String AUnion)
          (V.VStruct [(1, V.SomeValue $ V.VInt32 1)])
            `leftShouldContain` "is absent"

        (P.unpinch :: V.Value T.TUnion -> Either String AUnion)
          (V.VStruct [(2, V.SomeValue $ V.VBool True)])
            `leftShouldContain` "is absent"

        (P.unpinch :: V.Value T.TUnion -> Either String AUnion)
          (V.VStruct [(5, V.SomeValue $ V.VList [V.VInt32 1, V.VInt32 2])])
            `leftShouldContain` "has the incorrect type"

    it "reject invalid IDs" $
        (P.unpinch :: V.Value T.TUnion -> Either String AUnion)
          (V.VStruct [(3, V.SomeValue $ V.VDouble 1.0)])
            `leftShouldContain` "is absent"


data AStruct = AStruct (G.Field 1 ByteString) (G.Field 5 (Maybe Int32))
  deriving (Show, Ord, Eq, Generic)

instance P.Pinchable AStruct


structSpec :: Spec
structSpec = describe "Struct" $ do

    it "can pinch and unpinch" $ do
        P.pinch (AStruct (G.putField "foo") (G.putField Nothing))
            `shouldBe` V.VStruct [(1, V.SomeValue $ V.VBinary "foo")]

        P.pinch (AStruct (G.putField "bar") (G.putField $ Just 42))
            `shouldBe` V.VStruct
                [ (1, V.SomeValue $ V.VBinary "bar")
                , (5, V.SomeValue $ V.VInt32 42)
                ]

        P.unpinch (V.VStruct [(1, V.SomeValue $ V.VBinary "hello")])
            `shouldBe` Right
                (AStruct (G.putField "hello") (G.putField Nothing))

        P.unpinch
          (V.VStruct
            [ (1, V.SomeValue $ V.VBinary "hello")
            , (5, V.SomeValue $ V.VInt32 42)
            ]) `shouldBe`
                Right (AStruct (G.putField "hello") (G.putField $ Just 42))

    it "ignores unrecognized fields" $ do
        P.unpinch
          (V.VStruct
            [ (1, V.SomeValue $ V.VBinary "foo")
            , (2, V.SomeValue $ V.VInt32 42)
            ]) `shouldBe`
                Right (AStruct (G.putField "foo") (G.putField Nothing))

        P.unpinch
          (V.VStruct
            [ (1, V.SomeValue $ V.VBinary "foo")
            , (4, V.SomeValue $ V.VByte 12)
            , (5, V.SomeValue $ V.VInt32 34)
            ]) `shouldBe`
                Right (AStruct (G.putField "foo") (G.putField $ Just 34))

    it "rejects missing required fields" $
        (P.unpinch :: V.Value T.TStruct -> Either String AStruct)
          (V.VStruct
            [ (4, V.SomeValue $ V.VByte 12)
            , (5, V.SomeValue $ V.VInt32 34)
            ]) `leftShouldContain` "1 is absent"

    it "rejects invalid types" $
        (P.unpinch :: V.Value T.TStruct -> Either String AStruct)
          (V.VStruct
            [ (1, V.SomeValue $ V.VList [V.VInt32 42])
            ]) `leftShouldContain` "has the incorrect type"


spec :: Spec
spec = do
    enumSpec
    unionSpec
    structSpec
