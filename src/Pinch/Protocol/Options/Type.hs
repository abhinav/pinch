-- | This ultra-specific module exists so that Pinch.Protocol.Options
-- and Pinch.Protocol.Common don't have to reference each other

module Pinch.Protocol.Options.Type
  ( ProtocolOptions(..)
  , poMaxBinaryLength
  , poIncrementalChunkSize
  , poMaxMethodNameLength
  , poMaxListLength
  , poMaxMapSize
  , poMaxSetSize
  , poMethodNameParser
  ) where

import Data.Text (Text)

import qualified Data.Serialize.Get       as G

data ProtocolOptions
  = ProtocolOptions
  { poMaxBinaryLength      :: !Int
  , poIncrementalChunkSize :: !Int
  , poMaxMethodNameLength  :: !Int
  , poMaxListLength        :: !Int
  , poMaxSetSize           :: !Int
  , poMaxMapSize           :: !Int
  , poMethodNameParser     :: !(Int -> Int -> G.Get Text) -- ^ eg. 'Common.parseIDLMethodName'
  }
