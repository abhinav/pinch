{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.DeepSeq     (NFData)
import Control.Exception   (bracket_)
import Control.Monad
import Criterion
import Criterion.Main      (defaultMain)
import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashSet        (HashSet)
import Data.Int
import Data.Text           (Text)
import Data.Vector         (Vector)
import GHC.Generics        (Generic)
import GHC.Profiling
import Pinch               ((.:), (.=))

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Pinch               as P
import qualified Test.QuickCheck     as QC

------------------------------------------------------------------------------

data A = A
    { aName     :: Text
    , aBirthDay :: Int64
    , aPhone    :: Text
    , aSiblings :: Int32
    , aSpouse   :: Bool
    , aMoney    :: Double
    } deriving (Show, Ord, Eq, Generic)

instance NFData A

instance QC.Arbitrary A where
    arbitrary = A
        <$> (T.pack <$> replicateM 30 QC.arbitrary)
        <*> QC.arbitrary
        <*> (T.pack <$> replicateM 10 QC.arbitrary)
        <*> QC.arbitrary
        <*> QC.arbitrary
        <*> QC.arbitrary

instance P.Pinchable A where
    type Tag A = P.TStruct

    pinch A{..} = P.struct
        [ 1 .= aName
        , 2 .= aBirthDay
        , 3 .= aPhone
        , 4 .= aSiblings
        , 5 .= aSpouse
        , 6 .= aMoney
        ]

    unpinch m = A
        <$> m .: 1
        <*> m .: 2
        <*> m .: 3
        <*> m .: 4
        <*> m .: 5
        <*> m .: 6

------------------------------------------------------------------------------

data NestedMixed = NestedMixed
    { nestedMixedIntSetList       :: Vector (HashSet Int32)
    , nestedMixedMapIntStrset     :: HashMap Int32 (HashSet ByteString)
    , nestedMixedMapIntStrsetList :: Vector (HashMap Int32 (HashSet ByteString))
    } deriving (Show, Eq, Generic)

instance NFData NestedMixed

generateNestedMixedFields
    :: IO ( Vector (HashSet Int32)
          , HashMap Int32 (HashSet ByteString)
          , Vector (HashMap Int32 (HashSet ByteString))
          )
generateNestedMixedFields = bracket_ stopProfTimer startProfTimer $
    QC.generate $
        (,,) <$> V.replicateM 100 (HS.fromList <$> replicateM 100 QC.arbitrary)
             <*> genHM
             <*> V.replicateM 100 genHM
  where
    genHM = HM.fromList <$> replicateM 100 ((,) <$> QC.arbitrary <*> stringSet)
    stringSet = HS.fromList <$> replicateM 100 genBS
    genBS = B.pack <$> replicateM 16 QC.arbitrary

instance P.Pinchable NestedMixed where
    type Tag NestedMixed = P.TStruct

    pinch NestedMixed{..} = P.struct
        [ 1 .= nestedMixedIntSetList
        , 2 .= nestedMixedMapIntStrset
        , 3 .= nestedMixedMapIntStrsetList
        ]

    unpinch m = NestedMixed
        <$> m .: 1
        <*> m .: 2
        <*> m .: 3

------------------------------------------------------------------------------

data Struct = Struct
    { structStrings :: Vector ByteString
    , structInts    :: HashSet Int32
    , structMapped  :: HashMap Int32 ByteString
    } deriving (Show, Eq, Generic)

structFields
    :: IO
        ( Vector ByteString
        , HashSet Int32
        , HashMap Int32 ByteString
        )
structFields = bracket_ stopProfTimer startProfTimer $ return
    ( V.replicate 100000 "foo"
    , HS.fromList [1..100000]
    , HM.fromList [(n, "bar") | n <- [1..100000]]
    )

instance NFData Struct

instance P.Pinchable Struct where
    type Tag Struct = P.TStruct

    pinch Struct{..} = P.struct
        [ 1 .= structStrings
        , 2 .= structInts
        , 3 .= structMapped
        ]

    unpinch m = Struct
        <$> m .: 1
        <*> m .: 2
        <*> m .: 3

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ bgroup "A"
        [ env (generate :: IO A) $ \a -> bench "encode" $ whnf encode a
        , env generateEncodedA $ \bs -> bench "decode" $
            nf (P.decode P.binaryProtocol :: ByteString -> Either String A) bs
        ]
    , bgroup "NestedMixed"
        [ env generateNestedMixedFields $ \ ~(f1, f2, f3) -> bench "encode" $
            whnf encode (NestedMixed f1 f2 f3)
        , env generateEncodedNestedMixed $ \bs -> bench "decode" $
            nf (P.decode P.binaryProtocol :: ByteString -> Either String NestedMixed) bs
        ]
    , bgroup "Struct"
        [ env structFields $ \ ~(f1, f2, f3) -> bench "encode" $
            whnf encode (Struct f1 f2 f3)
        , env generateEncodedStruct $ \bs -> bench "deode" $
            nf (P.decode P.binaryProtocol :: ByteString -> Either String Struct) bs
        ]
    ]
  where
    generateEncodedNestedMixed = bracket_ stopProfTimer startProfTimer $ do
        (f1, f2, f3) <- generateNestedMixedFields
        return $ P.encode P.binaryProtocol (NestedMixed f1 f2 f3)

    generateEncodedA = bracket_ stopProfTimer startProfTimer $ do
        a <- generate :: IO A
        return $ P.encode P.binaryProtocol a

    generateEncodedStruct = bracket_ stopProfTimer startProfTimer $ do
        (f1, f2, f3) <- structFields
        return $ P.encode P.binaryProtocol (Struct f1 f2 f3)

    generate :: QC.Arbitrary a => IO a
    generate = QC.generate QC.arbitrary

    encode :: P.Pinchable a => a -> ByteString
    encode = P.encode P.binaryProtocol
