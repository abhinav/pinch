-- |
-- Module      :  Pinch.TType
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- TType tags allow writing code that depends on knowing the @TType@ of values
-- at compile time.
--
-- For example, values in a map, list, or set must all have the same TType.
-- This is enforced at the type level by parameterizing 'Pinch.Value.Value'
-- over these tags.
module Pinch.TType 
    ( TType
    , IsTType

    -- * Tags

    , TBool
    , TByte
    , TDouble
    , TInt16
    , TInt32
    , TInt64
    , TBinary
    , TStruct
    , TMap
    , TSet
    , TList
    ) where

import Pinch.Internal.TType
