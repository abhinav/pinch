{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
module Types
    ( Value(..)
    , KeyDoesNotExist(..)

    , SetValueRequest(..)
    , SetValueResponse(..)

    , GetValueRequest(..)
    , GetValueResponse(..)
    ) where

import Data.ByteString (ByteString)
import Data.Set        (Set)
import Data.Text       (Text)
import GHC.Generics    (Generic)

import qualified Pinch as P

data Value
    = ValueBin (P.Field 1 ByteString)
    | ValueBinSet (P.Field 2 (Set ByteString))
    deriving (Show, Ord, Eq, Generic)

instance P.Pinchable Value


data KeyDoesNotExist = KeyDoesNotExist (P.Field 1 (Maybe Text))
    deriving (Show, Ord, Eq, Generic)

instance P.Pinchable KeyDoesNotExist


data SetValueRequest = SetValueRequest
    { setValueKey   :: P.Field 1 Text
    , setValueValue :: P.Field 2 Value
    } deriving (Show, Ord, Eq, Generic)

instance P.Pinchable SetValueRequest


data SetValueResponse = SetValueSuccess P.Void
    deriving (Show, Ord, Eq, Generic)

instance P.Pinchable SetValueResponse


data GetValueRequest = GetValueRequest
    { getValueKey :: P.Field 1 Text
    } deriving (Show, Ord, Eq, Generic)

instance P.Pinchable GetValueRequest


data GetValueResponse
    = GetValueSuccess (P.Field 0 Value)
    | GetValueDoesNotExist (P.Field 1 KeyDoesNotExist)
    deriving (Show, Ord, Eq, Generic)

instance P.Pinchable GetValueResponse
