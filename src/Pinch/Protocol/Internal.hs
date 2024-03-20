module Pinch.Protocol.Internal
  ( guardMaxBinaryLength
  , guardMaxMethodNameLength
  , guardMaxListLength
  , guardMaxSetSize
  , guardMaxMapSize
  , guardNonNegativeSize
  ) where

import Pinch.Protocol.Options (ProtocolOptions(..))

import qualified Data.Serialize.Get as G

guardLimit :: (ProtocolOptions -> Int) -> String -> ProtocolOptions -> Int -> G.Get ()
guardLimit limitSelector measurementName opts n
  | n > limitSelector opts = fail $ measurementName ++ " exceeds maximum: " ++ show n
  | otherwise = pure ()

guardMaxBinaryLength :: ProtocolOptions -> Int -> G.Get ()
guardMaxBinaryLength = guardLimit poMaxBinaryLength "Binary blob length"

guardMaxMethodNameLength :: ProtocolOptions -> Int -> G.Get ()
guardMaxMethodNameLength = guardLimit poMaxMethodNameLength "Method name length"

guardMaxListLength :: ProtocolOptions -> Int -> G.Get ()
guardMaxListLength = guardLimit poMaxListLength "List length"

guardMaxSetSize :: ProtocolOptions -> Int -> G.Get ()
guardMaxSetSize = guardLimit poMaxSetSize "Set size"

guardMaxMapSize :: ProtocolOptions -> Int -> G.Get ()
guardMaxMapSize = guardLimit poMaxMapSize "Map size"

guardNonNegativeSize :: String -> Int -> G.Get ()
guardNonNegativeSize formName n
  | n < 0 = fail $ "Negative " ++ formName ++ ": " ++ show n
  | otherwise = pure ()
