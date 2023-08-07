-- |
-- Module      :  Pinch.Transport.Builder
-- Copyright   :  (c) Abhinav Gupta 2023
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- This module implements a ByteString builder very similar to
-- 'Data.ByteString.Builder' except that it keeps track of its final serialized
-- length. This allows it to allocate the target ByteString in one @malloc@ and
-- simply write the bytes to it.
module Pinch.Transport.Builder 
    ( module B
    ) where

import Pinch.Internal.Builder as B
