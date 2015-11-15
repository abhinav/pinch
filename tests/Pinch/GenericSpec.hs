{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Pinch.GenericSpec (spec) where

import Data.ByteString       (ByteString)
import Data.Int              (Int32, Int8)
import Data.Set              (Set)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import GHC.TypeLits          ()
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Set as S

import Pinch.Expectations
import Pinch.Internal.Util

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
        P.pinch (EnumA G.enum) `shouldBe` vi32 1
        P.pinch (EnumB G.enum) `shouldBe` vi32 2
        P.pinch (EnumC G.enum) `shouldBe` vi32 3

        P.unpinch (vi32 1) `shouldBe` Right (EnumA G.enum)
        P.unpinch (vi32 2) `shouldBe` Right (EnumB G.enum)
        P.unpinch (vi32 3) `shouldBe` Right (EnumC G.enum)

    it "reject invalid values" $
        (P.unpinch :: V.Value T.TInt32 -> Either String AnEnum)
          (vi32 4) `leftShouldContain` "Couldn't match enum value 4"

data AUnion
    = UnionDouble (G.Field 1 Double)
    | UnionByte   (G.Field 2 Int8)
    | UnionSet    (G.Field 5 (Set AnEnum))
  deriving (Show, Ord, Eq, Generic)

instance P.Pinchable AUnion


data UnionWithVoid
    = UnionVoidBefore (G.Field 1 Int8)
    | UnionVoid G.Void
    | UnionVoidAfter (G.Field 2 Text)
  deriving (Show, Ord, Eq, Generic)

instance P.Pinchable UnionWithVoid

unionSpec :: Spec
unionSpec = describe "Union" $ do

    prop "can pinch (1)" $ \dub ->
        P.pinch (UnionDouble (G.putField dub)) `shouldBe`
            vstruct [(1, vdub_ dub)]

    prop "can pinch (2)" $ \byt ->
        P.pinch (UnionByte (G.putField byt)) `shouldBe`
            vstruct [(2, vbyt_ byt)]

    it "can pinch (3)" $
        P.pinch
          (UnionSet (G.putField $ S.fromList [EnumA G.enum, EnumB G.enum]))
            `shouldBe`
              vstruct
                [(5, vset_ [vi32 1, vi32 2])]

    it "can pinch (4)" $ do
        P.pinch (UnionVoidBefore (G.putField 42))
            `shouldBe` vstruct [(1, vbyt_ 42)]

        P.pinch (UnionVoidAfter (G.putField "foo"))
            `shouldBe` vstruct [(2, vbin_ "foo")]

        P.pinch (UnionVoid G.Void) `shouldBe` vstruct []

    it "can unpinch" $ do
        P.unpinch (vstruct [(1, vdub_ 12.34)])
            `shouldBe` Right (UnionDouble $ G.putField 12.34)

        P.unpinch (vstruct [(2, vbyt_ 123)])
            `shouldBe` Right (UnionByte $ G.putField 123)

        P.unpinch
            (vstruct [(5, vset_ [vi32 1, vi32 2])])
            `shouldBe` Right
                (UnionSet . G.putField . S.fromList
                    $ [EnumA G.enum, EnumB G.enum])

        P.unpinch (vstruct [(1, vbyt_ 42)])
            `shouldBe` Right (UnionVoidBefore $ G.putField 42)

        P.unpinch (vstruct [(2, vbin_ "foo")])
            `shouldBe` Right (UnionVoidAfter $ G.putField "foo")

        P.unpinch (vstruct []) `shouldBe` Right (UnionVoid G.Void)

    it "reject invalid types" $ do
        (P.unpinch :: V.Value T.TUnion -> Either String AUnion)
          (vstruct [(1, vi32_ 1)])
            `leftShouldContain` "is absent"

        (P.unpinch :: V.Value T.TUnion -> Either String AUnion)
          (vstruct [(2, vbool_ True)])
            `leftShouldContain` "is absent"

        (P.unpinch :: V.Value T.TUnion -> Either String AUnion)
          (vstruct [(5, vlist_ [vi32 1, vi32 2])])
            `leftShouldContain` "has the incorrect type"

    it "reject invalid IDs" $
        (P.unpinch :: V.Value T.TUnion -> Either String AUnion)
          (vstruct [(3, vdub_ 1.0)])
            `leftShouldContain` "is absent"


data AStruct = AStruct (G.Field 1 ByteString) (G.Field 5 (Maybe Int32))
  deriving (Show, Ord, Eq, Generic)

instance P.Pinchable AStruct


structSpec :: Spec
structSpec = describe "Struct" $ do

    it "can pinch and unpinch" $ do
        P.pinch (AStruct (G.putField "foo") (G.putField Nothing))
            `shouldBe` vstruct [(1, vbin_ "foo")]

        P.pinch (AStruct (G.putField "bar") (G.putField $ Just 42))
            `shouldBe` vstruct
                [ (1, vbin_ "bar")
                , (5, vi32_ 42)
                ]

        P.unpinch (vstruct [(1, vbin_ "hello")])
            `shouldBe` Right
                (AStruct (G.putField "hello") (G.putField Nothing))

        P.unpinch
          (vstruct
            [ (1, vbin_ "hello")
            , (5, vi32_ 42)
            ]) `shouldBe`
                Right (AStruct (G.putField "hello") (G.putField $ Just 42))

    it "ignores unrecognized fields" $ do
        P.unpinch
          (vstruct
            [ (1, vbin_ "foo")
            , (2, vi32_ 42)
            ]) `shouldBe`
                Right (AStruct (G.putField "foo") (G.putField Nothing))

        P.unpinch
          (vstruct
            [ (1, vbin_ "foo")
            , (4, vbyt_ 12)
            , (5, vi32_ 34)
            ]) `shouldBe`
                Right (AStruct (G.putField "foo") (G.putField $ Just 34))

    it "rejects missing required fields" $
        (P.unpinch :: V.Value T.TStruct -> Either String AStruct)
          (vstruct
            [ (4, vbyt_ 12)
            , (5, vi32_ 34)
            ]) `leftShouldContain` "1 is absent"

    it "rejects invalid types" $
        (P.unpinch :: V.Value T.TStruct -> Either String AStruct)
          (vstruct
            [ (1, vlist_ [vi32 42])
            ]) `leftShouldContain` "has the incorrect type"


spec :: Spec
spec = do
    enumSpec
    unionSpec
    structSpec
