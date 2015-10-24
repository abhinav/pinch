{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pinch.Arbitrary
    ( SomeByteString(..)
    ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Data.ByteString    (ByteString)
import Data.Text          (Text)
import Test.QuickCheck

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.Text           as Tx
import qualified Data.Vector         as V

import qualified Pinch.Internal.Message as TM
import qualified Pinch.Internal.TType   as T
import qualified Pinch.Internal.Value   as V

#if !MIN_VERSION_QuickCheck(2, 8, 0)
scale :: (Int -> Int) -> Gen a -> Gen a
scale f g = sized (\n -> resize (f n) g)
#endif

halfSize :: Gen a -> Gen a
halfSize = scale (\n -> truncate (fromIntegral n / 2 :: Double))

newtype SomeByteString = SomeByteString
    { getSomeByteString :: ByteString }
  deriving (Show, Eq)

instance Arbitrary SomeByteString where
    arbitrary = SomeByteString . B.pack <$> arbitrary

    shrink (SomeByteString bs)
        | B.null bs = []
        | otherwise =
            SomeByteString . B.pack <$> shrink (B.unpack bs)

newtype SomeText = SomeText { getSomeText :: Text }

instance Arbitrary SomeText where
    arbitrary = SomeText . Tx.pack <$> arbitrary
    shrink = map (SomeText . Tx.pack) . shrink . Tx.unpack . getSomeText


instance Arbitrary T.SomeTType where
    arbitrary = elements
        [ T.SomeTType T.TBool
        , T.SomeTType T.TByte
        , T.SomeTType T.TDouble
        , T.SomeTType T.TInt16
        , T.SomeTType T.TInt32
        , T.SomeTType T.TInt64
        , T.SomeTType T.TBinary
        , T.SomeTType T.TStruct
        , T.SomeTType T.TMap
        , T.SomeTType T.TSet
        , T.SomeTType T.TList
        ]
    shrink _ = []

instance Arbitrary V.SomeValue where
    arbitrary = arbitrary >>= \(T.SomeTType t) -> genValue t
      where
        genValue
            :: forall a. T.IsTType a
            => T.TType a -> Gen V.SomeValue
        genValue _ = V.SomeValue <$> (arbitrary :: Gen (V.Value a))

    shrink (V.SomeValue v) = V.SomeValue <$> shrink v


instance T.IsTType a => Arbitrary (V.Value a) where
    arbitrary = case T.ttype :: T.TType a of
        T.TBool -> V.VBool <$> arbitrary
        T.TByte -> V.VByte <$> arbitrary
        T.TDouble -> V.VDouble <$> arbitrary
        T.TInt16 -> V.VInt16 <$> arbitrary
        T.TInt32 -> V.VInt32 <$> arbitrary
        T.TInt64 -> V.VInt64 <$> arbitrary
        T.TBinary -> V.VBinary . getSomeByteString <$> arbitrary
        T.TStruct -> genStruct
        T.TMap -> do
            ktype <- arbitrary
            vtype <- arbitrary
            case (ktype, vtype) of
                (T.SomeTType kt, T.SomeTType vt) ->
                    V.VMap <$> genMap kt vt
        T.TSet -> arbitrary >>= \(T.SomeTType t) -> V.VSet <$> genSet t
        T.TList -> arbitrary >>= \(T.SomeTType t) -> V.VList <$> genVec t
      where
        genStruct = halfSize $ V.VStruct . M.fromList <$> listOf genField
          where
            genField = (,) <$> (getPositive <$> arbitrary)
                           <*> arbitrary

        genMap :: (T.IsTType k, T.IsTType v)
               => T.TType k -> T.TType v
               -> Gen (M.HashMap (V.Value k) (V.Value v))
        genMap _ _ = M.fromList <$> halfSize arbitrary

        genSet :: T.IsTType x => T.TType x -> Gen (S.HashSet (V.Value x))
        genSet _ = S.fromList <$> halfSize arbitrary

        genVec :: T.IsTType x => T.TType x -> Gen (V.Vector (V.Value x))
        genVec _ = V.fromList <$> halfSize arbitrary

    shrink = case T.ttype :: T.TType a of
        T.TByte -> \(V.VByte x) -> V.VByte <$> shrink x
        T.TDouble -> \(V.VDouble x) -> V.VDouble <$> shrink x
        T.TInt16 -> \(V.VInt16 x) -> V.VInt16 <$> shrink x
        T.TInt32 -> \(V.VInt32 x) -> V.VInt32 <$> shrink x
        T.TInt64 -> \(V.VInt64 x) -> V.VInt64 <$> shrink x
        T.TBinary -> shrinkBinary
        T.TStruct ->
            \(V.VStruct xs) -> V.VStruct . M.fromList <$> shrink (M.toList xs)
        T.TMap ->
            \(V.VMap xs) -> V.VMap . M.fromList <$> shrink (M.toList xs)
        T.TSet ->
            \(V.VSet xs) -> V.VSet . S.fromList <$> shrink (S.toList xs)
        T.TList ->
            \(V.VList xs) -> V.VList . V.fromList <$> shrink (V.toList xs)
        _ -> const []
      where
        shrinkBinary :: V.Value T.TBinary -> [V.Value T.TBinary]
        shrinkBinary (V.VBinary bs)
            | B.null bs = []
            | otherwise = V.VBinary . B.pack <$> shrink (B.unpack bs)


instance Arbitrary TM.MessageType where
    arbitrary = elements
        [ TM.CallMessage
        , TM.ReplyMessage
        , TM.ExceptionMessage
        , TM.OnewayMessage
        ]
    shrink _ = []


instance T.IsTType a => Arbitrary (TM.Message a) where
    arbitrary =
        TM.Message
            <$> (getSomeText <$> arbitrary)
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

    shrink (TM.Message name  typ  mid  body) =
        [   TM.Message name' typ' mid' body'
        | (SomeText name', typ', mid', body') <-
            shrink ((SomeText name), typ, mid, body)
        ]
