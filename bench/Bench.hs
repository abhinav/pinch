{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Main (main) where

import Control.DeepSeq (NFData)
import Criterion
import Criterion.Main  (defaultMain)
import Data.ByteString (ByteString)
import Data.Int
import Data.Text       (Text)
import GHC.Generics    (Generic)
import Pinch           ((.:), (.=))

import qualified Data.Text       as T
import qualified Pinch           as P
import qualified Test.QuickCheck as QC

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
        <$> (T.pack <$> QC.arbitrary)
        <*> QC.arbitrary
        <*> (T.pack <$> QC.arbitrary)
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


data A2 = A2
    { a2Name     :: P.Field 1 Text
    , a2BirthDay :: P.Field 2 Int64
    , a2Phone    :: P.Field 3 Text
    , a2Siblings :: P.Field 4 Int32
    , a2Spouse   :: P.Field 5 Bool
    , a2Money    :: P.Field 6 Double
    } deriving (Show, Ord, Eq, Generic)

instance P.Pinchable A2
instance NFData A2

instance QC.Arbitrary A2 where
    arbitrary = A2
        <$> (P.putField . T.pack <$> QC.arbitrary)
        <*> (P.putField          <$> QC.arbitrary)
        <*> (P.putField . T.pack <$> QC.arbitrary)
        <*> (P.putField          <$> QC.arbitrary)
        <*> (P.putField          <$> QC.arbitrary)
        <*> (P.putField          <$> QC.arbitrary)


main :: IO ()
main = defaultMain
    [ bgroup "Pinch"
        [ env (generate :: IO A) $ \a ->
          bench "encode" $ whnf (P.encode P.binaryProtocol) a
        , env (P.encode P.binaryProtocol <$> (generate :: IO A)) $ \bs ->
          bench "decode" $
          whnf (P.decode P.binaryProtocol :: ByteString -> Either String A) bs
        ]
    , bgroup "Pinch Generic"
        [ env (generate :: IO A2) $ \a ->
          bench "encode" $ whnf (P.encode P.binaryProtocol) a
        , env (P.encode P.binaryProtocol <$> (generate :: IO A2)) $ \bs ->
          bench "decode" $
          whnf (P.decode P.binaryProtocol :: ByteString -> Either String A2) bs
        ]
    ]
  where
    generate :: QC.Arbitrary a => IO a
    generate = QC.generate QC.arbitrary
