{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Pinch.Internal.Exception
  ( ApplicationException (..)
  , ExceptionType (..)
  , ThriftError(..)
  )
where

import           Control.Exception        (Exception)
import           Data.Int
import           Data.Typeable            (Typeable)
import           Pinch.Internal.Pinchable
import           Pinch.Internal.TType

import qualified Data.Text                as T

-- | Thrift application exception as defined in <https://github.com/apache/thrift/blob/master/doc/specs/thrift-rpc.md#response-struct>.
data ApplicationException
  = ApplicationException
  { appExMessage :: T.Text
  , appExType    :: ExceptionType
  }
  deriving (Show, Eq, Typeable)

instance Exception ApplicationException

instance Pinchable ApplicationException where
  type Tag ApplicationException = TStruct

  pinch p = struct
    [ 1 .= appExMessage p
    , 2 .= appExType p
    ]

  unpinch value = ApplicationException
    <$> value .: 1
    <*> value .: 2

-- | Thrift exception type as defined in <https://github.com/apache/thrift/blob/master/doc/specs/thrift-rpc.md#response-struct>.
data ExceptionType
  -- DO NOT RE-ORDER, the enum values need to match the ones defined in the Thrift specification!
  = Unknown               -- 0
  | UnknownMethod         -- 1
  | InvalidMessageType    -- 2
  | WrongMethodName       -- 3
  | BadSequenceId         -- 4
  | MissingResult         -- 5
  | InternalError         -- 6
  | ProtocolError         -- 7
  | InvalidTransform      -- 8
  | InvalidProtocol       -- 9
  | UnsupportedClientType -- 10
  deriving (Show, Eq, Enum, Bounded)

instance Pinchable ExceptionType where
  type Tag ExceptionType = TEnum

  pinch t = pinch ((fromIntegral $ fromEnum t) :: Int32)

  unpinch v = do
    value <- (fromIntegral :: Int32 -> Int) <$> unpinch v
    if (fromEnum $ minBound @ExceptionType) <= value && value <= (fromEnum $ maxBound @ExceptionType)
      then pure $ toEnum $ fromIntegral value
      else fail $ "Unknown application exception type: " ++ show value

-- | An error occured while processing a thrift call.
-- Signals errors like premature EOF, Thrift protocol parsing failures etc.
data ThriftError = ThriftError T.Text
  deriving (Show, Eq)
instance Exception ThriftError
