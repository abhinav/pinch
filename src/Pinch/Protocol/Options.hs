module Pinch.Protocol.Options
  ( ProtocolOptions
  , defaultProtocolOptions
  , poMaxBinaryLength
  , poIncrementalChunkSize
  , poMaxMethodNameLength
  , poMaxListLength
  , poMaxMapSize
  , poMaxSetSize
  , poMethodNameParser
  ) where

import Data.Text (Text)
import Pinch.Protocol.Options.Type

import qualified Data.Serialize.Get    as G
import qualified Pinch.Protocol.Common as Common

defaultProtocolOptions :: ProtocolOptions
defaultProtocolOptions
  = ProtocolOptions
  { poMaxBinaryLength      = 2147483647
  , poMaxMethodNameLength  = 2147483647
  , poMaxListLength        = 2147483647
  , poMaxMapSize           = 2147483647
  , poMaxSetSize           = 2147483647
  , poIncrementalChunkSize = 4096 * 4
  , poMethodNameParser     = Common.parseUtf8Incremental
  }
