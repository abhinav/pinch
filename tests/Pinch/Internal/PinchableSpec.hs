{-# LANGUAGE OverloadedLists   #-}
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

import qualified Data.ByteString as B

import Pinch.Arbitrary
import Pinch.Internal.Pinchable ((.:), (.:?), (.=), (?=))

import qualified Pinch.Internal.Pinchable as P
import qualified Pinch.Internal.TType     as T
import qualified Pinch.Internal.Value     as V

-- | Expectation
leftShouldContain :: Show a => Either String a -> String -> Expectation
leftShouldContain (Right a) _ =
    expectationFailure $ "Expected failure but got: " ++ show a
leftShouldContain (Left msg) x = msg `shouldContain` x
infix 1 `leftShouldContain`


data AnEnum = EnumA | EnumB | EnumC
    deriving (Show, Ord, Eq)

instance P.Pinchable AnEnum where
    type Tag AnEnum = T.TEnum

    pinch EnumA = P.pinch (1 :: Int32)
    pinch EnumB = P.pinch (2 :: Int32)
    pinch EnumC = P.pinch (3 :: Int32)

    unpinch = P.unpinch >=> \v -> case (v :: Int32) of
        1 -> Right EnumA
        2 -> Right EnumB
        3 -> Right EnumC
        _ -> Left "Unknown enum value"

enumSpec :: Spec
enumSpec = describe "Enum" $ do

    it "can pinch and unpinch" $ do
        P.pinch EnumA `shouldBe` V.VInt32 1
        P.pinch EnumB `shouldBe` V.VInt32 2
        P.pinch EnumC `shouldBe` V.VInt32 3

        P.unpinch (V.VInt32 1) `shouldBe` Right EnumA
        P.unpinch (V.VInt32 2) `shouldBe` Right EnumB
        P.unpinch (V.VInt32 3) `shouldBe` Right EnumC

    it "reject invalid values" $
        P.unpinch (V.VInt32 4) `shouldBe`
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
            V.VStruct [(1, V.SomeValue $ V.VDouble dub)]

    prop "can pinch (2)" $ \byt ->
        P.pinch (UnionByte byt) `shouldBe`
            V.VStruct [(2, V.SomeValue $ V.VByte byt)]

    it "can pinch (3)" $
        P.pinch (UnionSet [EnumA, EnumB]) `shouldBe`
            V.VStruct [(5, V.SomeValue $ V.VSet [V.VInt32 1, V.VInt32 2])]

    it "can unpinch" $ do
        P.unpinch (V.VStruct [(1, V.SomeValue $ V.VDouble 12.34)])
            `shouldBe` Right (UnionDouble 12.34)

        P.unpinch (V.VStruct [(2, V.SomeValue $ V.VByte 123)])
            `shouldBe` Right (UnionByte 123)

        P.unpinch
            (V.VStruct [(5, V.SomeValue $ V.VSet [V.VInt32 1, V.VInt32 2])])
            `shouldBe` Right (UnionSet [EnumA, EnumB])

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
            `shouldBe` V.VStruct [(1, V.SomeValue $ V.VBinary "foo")]

        P.pinch (AStruct "bar" (Just 42))
            `shouldBe` V.VStruct
                [ (1, V.SomeValue $ V.VBinary "bar")
                , (5, V.SomeValue $ V.VInt32 42)
                ]

        P.unpinch (V.VStruct [(1, V.SomeValue $ V.VBinary "hello")])
            `shouldBe` Right (AStruct "hello" Nothing)

        P.unpinch
          (V.VStruct
            [ (1, V.SomeValue $ V.VBinary "hello")
            , (5, V.SomeValue $ V.VInt32 42)
            ]) `shouldBe` Right (AStruct "hello" (Just 42))

    it "ignores unrecognized fields" $ do
        P.unpinch
          (V.VStruct
            [ (1, V.SomeValue $ V.VBinary "foo")
            , (2, V.SomeValue $ V.VInt32 42)
            ]) `shouldBe` Right (AStruct "foo" Nothing)

        P.unpinch
          (V.VStruct
            [ (1, V.SomeValue $ V.VBinary "foo")
            , (4, V.SomeValue $ V.VByte 12)
            , (5, V.SomeValue $ V.VInt32 34)
            ]) `shouldBe` Right (AStruct "foo" (Just 34))

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


primitivesSpec :: Spec
primitivesSpec = do

    it "can pinch and unpinch Bools" $ do
        P.pinch True `shouldBe` V.VBool True
        P.pinch False `shouldBe` V.VBool False

        P.unpinch (V.VBool True) `shouldBe` Right True
        P.unpinch (V.VBool False) `shouldBe` Right False

    prop "can pinch and unpinch Int8" $ \i -> do
        P.pinch i `shouldBe` V.VByte i
        P.unpinch (V.VByte i) `shouldBe` Right i

    prop "can pinch and unpinch Int32" $ \i -> do
        P.pinch i `shouldBe` V.VInt32 i
        P.unpinch (V.VInt32 i) `shouldBe` Right i

    prop "can pinch and unpinch Int64" $ \i -> do
        P.pinch i `shouldBe` V.VInt64 i
        P.unpinch (V.VInt64 i) `shouldBe` Right i

    prop "can pinch and unpinch Double" $ \d -> do
        P.pinch d `shouldBe` V.VDouble d
        P.unpinch (V.VDouble d) `shouldBe` Right d

    prop "can pinch and unpinch ByteString" $ \(SomeByteString bs) -> do
        P.pinch bs `shouldBe` V.VBinary bs
        P.unpinch (V.VBinary bs) `shouldBe` Right bs

    it "can pinch and unpinch Text" $ do
        P.pinch ("☕️" :: Text)
            `shouldBe` V.VBinary (B.pack [0xe2, 0x98, 0x95, 0xef, 0xb8, 0x8f])

        P.unpinch (V.VBinary (B.pack [0xe2, 0x98, 0x95, 0xef, 0xb8, 0x8f]))
            `shouldBe` Right ("☕️" :: Text)


containerSpec :: Spec
containerSpec = do

    describe "Vector" $ do
        it "can pinch and unpinch" $ do

            P.pinch ([1, 2, 3] :: Vector Int32)
                `shouldBe` V.VList [V.VInt32 1, V.VInt32 2, V.VInt32 3]

            P.unpinch (V.VList [V.VInt32 1, V.VInt32 2, V.VInt32 3])
                `shouldBe` Right ([1, 2, 3] :: Vector Int32)

        it "rejects type mismatch" $
          (P.unpinch :: V.Value T.TList -> Either String (Vector Int8))
            (V.VList [V.VInt32 1, V.VInt32 2, V.VInt32 3])
                `leftShouldContain` "Type mismatch"

    describe "List" $ do

        it "can pinch and unpinch" $ do

            P.pinch ([1, 2, 3] :: [Int32])
                `shouldBe` V.VList [V.VInt32 1, V.VInt32 2, V.VInt32 3]

            P.unpinch (V.VList [V.VInt32 1, V.VInt32 2, V.VInt32 3])
                `shouldBe` Right ([1, 2, 3] :: [Int32])

        it "rejects type mismatch" $
          (P.unpinch :: V.Value T.TList -> Either String [Int8])
            (V.VList [V.VInt32 1, V.VInt32 2, V.VInt32 3])
                `leftShouldContain` "Type mismatch"

    describe "HashSet" $ do
        it "can pinch and unpinch" $ do

            P.pinch ([1, 2, 3] :: HashSet Int32)
                `shouldBe` V.VSet [V.VInt32 1, V.VInt32 2, V.VInt32 3]

            P.unpinch (V.VSet [V.VInt32 1, V.VInt32 2, V.VInt32 3])
                `shouldBe` Right ([1, 2, 3] :: HashSet Int32)

        it "rejects type mismatch" $
          (P.unpinch :: V.Value T.TSet -> Either String (HashSet Int8))
            (V.VSet [V.VInt32 1, V.VInt32 2, V.VInt32 3])
                `leftShouldContain` "Type mismatch"

    describe "Set" $ do
        it "can pinch and unpinch" $ do

            P.pinch ([1, 2, 3] :: Set Int32)
                `shouldBe` V.VSet [V.VInt32 1, V.VInt32 2, V.VInt32 3]

            P.unpinch (V.VSet [V.VInt32 1, V.VInt32 2, V.VInt32 3])
                `shouldBe` Right ([1, 2, 3] :: Set Int32)

        it "rejects type mismatch" $
          (P.unpinch :: V.Value T.TSet -> Either String (Set Int8))
            (V.VSet [V.VInt32 1, V.VInt32 2, V.VInt32 3])
                `leftShouldContain` "Type mismatch"

    describe "HashMap" $ do

        it "can pinch and unpinch" $ do

            P.pinch ([("a", 1), ("b", 2)] :: HashMap ByteString Int16)
                `shouldBe` V.VMap
                    [ (V.VBinary "a", V.VInt16 1)
                    , (V.VBinary "b", V.VInt16 2)
                    ]

            P.unpinch
              (V.VMap [ (V.VBinary "a", V.VInt16 1)
                      , (V.VBinary "b", V.VInt16 2)
                      ]) `shouldBe`
                        Right ([("a", 1), ("b", 2)] :: HashMap ByteString Int16)

        it "rejects key type mismatch" $
          (P.unpinch :: V.Value T.TMap -> Either String (HashMap Int32 Int16))
              (V.VMap [(V.VBinary "a", V.VInt16 1)])
                  `leftShouldContain` "Type mismatch"

        it "rejects value type mismatch" $
          (P.unpinch :: V.Value T.TMap -> Either String (HashMap ByteString Bool))
              (V.VMap [(V.VBinary "a", V.VInt16 1)])
                  `leftShouldContain` "Type mismatch"

    describe "Map" $ do

        it "can pinch and unpinch" $ do

            P.pinch ([("a", 1), ("b", 2)] :: Map ByteString Int16)
                `shouldBe` V.VMap
                    [ (V.VBinary "a", V.VInt16 1)
                    , (V.VBinary "b", V.VInt16 2)
                    ]

            P.unpinch
              (V.VMap [ (V.VBinary "a", V.VInt16 1)
                      , (V.VBinary "b", V.VInt16 2)
                      ]) `shouldBe`
                        Right ([("a", 1), ("b", 2)] :: Map ByteString Int16)

        it "rejects key type mismatch" $
          (P.unpinch :: V.Value T.TMap -> Either String (Map Int32 Int16))
              (V.VMap [(V.VBinary "a", V.VInt16 1)])
                  `leftShouldContain` "Type mismatch"

        it "rejects value type mismatch" $
          (P.unpinch :: V.Value T.TMap -> Either String (Map ByteString Bool))
              (V.VMap [(V.VBinary "a", V.VInt16 1)])
                  `leftShouldContain` "Type mismatch"


spec :: Spec
spec = describe "Pinchable" $ do
    primitivesSpec
    containerSpec
    enumSpec
    unionSpec
    structSpec
