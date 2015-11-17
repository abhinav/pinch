{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Pinch.Internal.PinchableSpec (spec) where

import Control.Applicative
import Control.Monad
import Data.ByteString       (ByteString)
import Data.HashMap.Strict   (HashMap)
import Data.HashSet          (HashSet)
import Data.Int              (Int16, Int32, Int8)
import Data.Map.Strict       (Map)
import Data.Set              (Set)
import Data.Text             (Text)
import Data.Vector           (Vector)
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import qualified Data.Vector         as Vec

import Pinch.Arbitrary
import Pinch.Expectations
import Pinch.Internal.Pinchable ((.:), (.:?), (.=), (?=))
import Pinch.Internal.Util

import qualified Pinch.Internal.Pinchable as P
import qualified Pinch.Internal.TType     as T
import qualified Pinch.Internal.Value     as V


data AnEnum = EnumA | EnumB | EnumC
    deriving (Show, Ord, Eq)

instance P.Pinchable AnEnum where
    type Tag AnEnum = T.TEnum

    pinch EnumA = P.pinch (1 :: Int32)
    pinch EnumB = P.pinch (2 :: Int32)
    pinch EnumC = P.pinch (3 :: Int32)

    unpinch = P.unpinch >=> \v -> case (v :: Int32) of
        1 -> return EnumA
        2 -> return EnumB
        3 -> return EnumC
        _ -> fail "Unknown enum value"

unpinch' :: P.Pinchable a => V.Value (P.Tag a) -> Either String a
unpinch' = P.runParser . P.unpinch

enumSpec :: Spec
enumSpec = describe "Enum" $ do

    it "can pinch and unpinch" $ do
        P.pinch EnumA `shouldBe` vi32 1
        P.pinch EnumB `shouldBe` vi32 2
        P.pinch EnumC `shouldBe` vi32 3

        unpinch' (vi32 1) `shouldBe` Right EnumA
        unpinch' (vi32 2) `shouldBe` Right EnumB
        unpinch' (vi32 3) `shouldBe` Right EnumC

    it "reject invalid values" $
        unpinch' (vi32 4) `shouldBe`
            (Left "Unknown enum value" :: Either String AnEnum)


data AUnion
    = UnionDouble Double
    | UnionByte Int8
    | UnionSet (Set AnEnum)
  deriving (Show, Ord, Eq)

instance P.Pinchable AUnion where
    type Tag AUnion = T.TUnion

    pinch (UnionDouble d) = P.union 1 d
    pinch (UnionByte i) = P.union 2 i
    pinch (UnionSet s) = P.union 5 s

    unpinch m =
            UnionDouble <$> m .: 1
        <|> UnionByte   <$> m .: 2
        <|> UnionSet    <$> m .: 5

unionSpec :: Spec
unionSpec = describe "Union" $ do

    prop "can pinch (1)" $ \dub ->
        P.pinch (UnionDouble dub) `shouldBe`
            vstruct [(1, vdub_ dub)]

    prop "can pinch (2)" $ \byt ->
        P.pinch (UnionByte byt) `shouldBe`
            vstruct [(2, vbyt_ byt)]

    it "can pinch (3)" $
        P.pinch (UnionSet $ S.fromList [EnumA, EnumB]) `shouldBe`
            vstruct [(5, vset_ [vi32 1, vi32 2])]

    it "can unpinch" $ do
        unpinch' (vstruct [(1, vdub_ 12.34)])
            `shouldBe` Right (UnionDouble 12.34)

        unpinch' (vstruct [(2, vbyt_ 123)])
            `shouldBe` Right (UnionByte 123)

        unpinch'
            (vstruct [(5, vset_ [vi32 1, vi32 2])])
            `shouldBe` Right (UnionSet $ S.fromList [EnumA, EnumB])

    it "reject invalid types" $ do
        (unpinch' :: V.Value T.TUnion -> Either String AUnion)
          (vstruct [(1, vi32_ 1)])
            `leftShouldContain` "is absent"

        (unpinch' :: V.Value T.TUnion -> Either String AUnion)
          (vstruct [(2, vbool_ True)])
            `leftShouldContain` "is absent"

        (unpinch' :: V.Value T.TUnion -> Either String AUnion)
          (vstruct [(5, vlist_ [vi32 1, vi32 2])])
            `leftShouldContain` "has the incorrect type"

    it "reject invalid IDs" $
        (unpinch' :: V.Value T.TUnion -> Either String AUnion)
          (vstruct [(3, vdub_ 1.0)])
            `leftShouldContain` "is absent"


data AStruct = AStruct ByteString (Maybe Int32)
  deriving (Show, Ord, Eq)

instance P.Pinchable AStruct where
    type Tag AStruct = T.TStruct

    pinch (AStruct a b) = P.struct [1 .= a, 5 ?= b]
    unpinch m = AStruct <$> m .: 1 <*> m .:? 5

structSpec :: Spec
structSpec = describe "Struct" $ do

    it "can pinch and unpinch" $ do
        P.pinch (AStruct "foo" Nothing)
            `shouldBe` vstruct [(1, vbin_ "foo")]

        P.pinch (AStruct "bar" (Just 42))
            `shouldBe` vstruct
                [ (1, vbin_ "bar")
                , (5, vi32_ 42)
                ]

        unpinch' (vstruct [(1, vbin_ "hello")])
            `shouldBe` Right (AStruct "hello" Nothing)

        unpinch'
          (vstruct
            [ (1, vbin_ "hello")
            , (5, vi32_ 42)
            ]) `shouldBe` Right (AStruct "hello" (Just 42))

    it "ignores unrecognized fields" $ do
        unpinch'
          (vstruct
            [ (1, vbin_ "foo")
            , (2, vi32_ 42)
            ]) `shouldBe` Right (AStruct "foo" Nothing)

        unpinch'
          (vstruct
            [ (1, vbin_ "foo")
            , (4, vbyt_ 12)
            , (5, vi32_ 34)
            ]) `shouldBe` Right (AStruct "foo" (Just 34))

    it "rejects missing required fields" $
        (unpinch' :: V.Value T.TStruct -> Either String AStruct)
          (vstruct
            [ (4, vbyt_ 12)
            , (5, vi32_ 34)
            ]) `leftShouldContain` "1 is absent"

    it "rejects invalid types" $
        (unpinch' :: V.Value T.TStruct -> Either String AStruct)
          (vstruct
            [ (1, vlist_ [vi32 42])
            ]) `leftShouldContain` "has the incorrect type"


primitivesSpec :: Spec
primitivesSpec = do

    it "can pinch and unpinch Bools" $ do
        P.pinch True `shouldBe` vbool True
        P.pinch False `shouldBe` vbool False

        unpinch' (vbool True) `shouldBe` Right True
        unpinch' (vbool False) `shouldBe` Right False

    prop "can pinch and unpinch Int8" $ \i -> do
        P.pinch i `shouldBe` vbyt i
        unpinch' (vbyt i) `shouldBe` Right i

    prop "can pinch and unpinch Int32" $ \i -> do
        P.pinch i `shouldBe` vi32 i
        unpinch' (vi32 i) `shouldBe` Right i

    prop "can pinch and unpinch Int64" $ \i -> do
        P.pinch i `shouldBe` vi64 i
        unpinch' (vi64 i) `shouldBe` Right i

    prop "can pinch and unpinch Double" $ \d -> do
        P.pinch d `shouldBe` vdub d
        unpinch' (vdub d) `shouldBe` Right d

    prop "can pinch and unpinch ByteString" $ \(SomeByteString bs) -> do
        P.pinch bs `shouldBe` vbin bs
        unpinch' (vbin bs) `shouldBe` Right bs

    it "can pinch and unpinch Text" $ do
        P.pinch ("☕️" :: Text)
            `shouldBe` vbin (B.pack [0xe2, 0x98, 0x95, 0xef, 0xb8, 0x8f])

        unpinch' (vbin (B.pack [0xe2, 0x98, 0x95, 0xef, 0xb8, 0x8f]))
            `shouldBe` Right ("☕️" :: Text)


containerSpec :: Spec
containerSpec = do

    describe "Vector" $ do
        it "can pinch and unpinch" $ do

            P.pinch (Vec.fromList [1, 2, 3 :: Int32])
                `shouldBe` vlist [vi32 1, vi32 2, vi32 3]

            unpinch' (vlist [vi32 1, vi32 2, vi32 3])
                `shouldBe` Right (Vec.fromList [1, 2, 3 :: Int32])

        it "rejects type mismatch" $
          (unpinch' :: V.Value T.TList -> Either String (Vector Int8))
            (vlist [vi32 1, vi32 2, vi32 3])
                `leftShouldContainAll`
                    ["Type mismatch", "Expected TByte", "Got TInt32"]

    describe "List" $ do

        it "can pinch and unpinch" $ do

            P.pinch ([1, 2, 3] :: [Int32])
                `shouldBe` vlist [vi32 1, vi32 2, vi32 3]

            unpinch' (vlist [vi32 1, vi32 2, vi32 3])
                `shouldBe` Right ([1, 2, 3] :: [Int32])

        it "rejects type mismatch" $
          (unpinch' :: V.Value T.TList -> Either String [Int8])
            (vlist [vi32 1, vi32 2, vi32 3])
                `leftShouldContain` "Type mismatch"

    describe "HashSet" $ do
        it "can pinch and unpinch" $ do

            P.pinch (HS.fromList [1, 2, 3 :: Int32])
                `shouldBe` vset [vi32 1, vi32 2, vi32 3]

            unpinch' (vset [vi32 1, vi32 2, vi32 3])
                `shouldBe` Right (HS.fromList [1, 2, 3 :: Int32])

        it "rejects type mismatch" $
          (unpinch' :: V.Value T.TSet -> Either String (HashSet Int8))
            (vset [vi32 1, vi32 2, vi32 3])
                `leftShouldContain` "Type mismatch"

    describe "Set" $ do
        it "can pinch and unpinch" $ do

            P.pinch (S.fromList [1, 2, 3 :: Int32])
                `shouldBe` vset [vi32 1, vi32 2, vi32 3]

            unpinch' (vset [vi32 1, vi32 2, vi32 3])
                `shouldBe` Right (S.fromList [1, 2, 3 :: Int32])

        it "rejects type mismatch" $
          (unpinch' :: V.Value T.TSet -> Either String (Set Int8))
            (vset [vi32 1, vi32 2, vi32 3])
                `leftShouldContain` "Type mismatch"

    describe "HashMap" $ do

        it "can pinch and unpinch" $ do

            P.pinch (HM.fromList [("a", 1), ("b", 2) :: (ByteString, Int16)])
                `shouldBe` vmap
                    [ (vbin "a", vi16 1)
                    , (vbin "b", vi16 2)
                    ]

            unpinch'
              (vmap [ (vbin "a", vi16 1)
                      , (vbin "b", vi16 2)
                      ]) `shouldBe`
                        Right
                          (HM.fromList
                            [("a", 1), ("b", 2) :: (ByteString, Int16)])

        it "rejects key type mismatch" $
          (unpinch' :: V.Value T.TMap -> Either String (HashMap Int32 Int16))
              (vmap [(vbin "a", vi16 1)])
                  `leftShouldContain` "Type mismatch"

        it "rejects value type mismatch" $
          (unpinch' :: V.Value T.TMap -> Either String (HashMap ByteString Bool))
              (vmap [(vbin "a", vi16 1)])
                  `leftShouldContain` "Type mismatch"

    describe "Map" $ do

        it "can pinch and unpinch" $ do

            P.pinch (M.fromList [("a", 1), ("b", 2) :: (ByteString, Int16)])
                `shouldBe` vmap
                    [ (vbin "a", vi16 1)
                    , (vbin "b", vi16 2)
                    ]

            unpinch'
              (vmap [ (vbin "a", vi16 1)
                      , (vbin "b", vi16 2)
                      ]) `shouldBe`
                        Right
                          (M.fromList
                            [("a", 1), ("b", 2) :: (ByteString, Int16)])

        it "rejects key type mismatch" $
          (unpinch' :: V.Value T.TMap -> Either String (Map Int32 Int16))
              (vmap [(vbin "a", vi16 1)])
                  `leftShouldContain` "Type mismatch"

        it "rejects value type mismatch" $
          (unpinch' :: V.Value T.TMap -> Either String (Map ByteString Bool))
              (vmap [(vbin "a", vi16 1)])
                  `leftShouldContain` "Type mismatch"


spec :: Spec
spec = describe "Pinchable" $ do
    primitivesSpec
    containerSpec
    enumSpec
    unionSpec
    structSpec
