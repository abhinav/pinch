-- |
-- Module      :  Pinch.Value
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- 'Value' is an intermediate representation of Thrift payloads tagged with
-- TType tags from 'Pinch.TType.TType'. Types that want to be serialized into
-- Thrift payloads need only define a way to convert themselves to and from
-- 'Value' objects (via 'Pinch.Pinchable.Pinchable').
module Pinch.Value
    ( Value
    ) where

import Pinch.Internal.Value
