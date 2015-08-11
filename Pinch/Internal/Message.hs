{-# LANGUAGE DeriveDataTypeable #-}
module Pinch.Internal.Message
    ( Message(..)
    , MessageType(..)
    ) where

import Data.Data     (Data)
import Data.Int      (Int32)
import Data.Text     (Text)
import Data.Typeable (Typeable)

import Pinch.Internal.Value (Value)

data MessageType
    = CallMessage      -- ^ 1
    | ReplyMessage     -- ^ 2
    | ExceptionMessage -- ^ 3
    | OnewayMessage    -- ^ 4
  deriving (Show, Eq, Data, Typeable)


data Message a = Message
    { messageName :: !Text
    , messageType :: !MessageType
    , messageId   :: !Int32
    , messageBody :: !(Value a)
    }
  deriving (Show, Eq, Typeable)
