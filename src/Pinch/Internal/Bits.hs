{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module      :  Pinch.Internal.Bits
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Provides unchecked bitwise shift operations. Similar versions of @shiftR@
-- are used by @ByteString.Builder.Prim@.
module Pinch.Internal.Bits
    ( w16ShiftL
    , w32ShiftL
    , w64ShiftL
    ) where

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base (
    Int (..),
#if MIN_VERSION_base(4,16,0)
    uncheckedShiftLWord16#,
    uncheckedShiftLWord32#,
#endif
#if MIN_VERSION_ghc_prim(0,9,0)
    uncheckedShiftL64#,
#else
    uncheckedShiftL#,
#endif
    )
import GHC.Word (Word16 (..), Word32 (..), Word64 (..))
#else
import Data.Bits (shiftL)
import Data.Word (Word16, Word32, Word64)
#endif

{-# INLINE w16ShiftL #-}
w16ShiftL :: Word16 -> Int -> Word16

{-# INLINE w32ShiftL #-}
w32ShiftL :: Word32 -> Int -> Word32

{-# INLINE w64ShiftL #-}
w64ShiftL :: Word64 -> Int -> Word64

-- If we're not on GHC, the regular shiftL will be returned.

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#if MIN_VERSION_base(4,16,0)
w16ShiftL (W16# w) (I# i) = W16# (w `uncheckedShiftLWord16#` i)
w32ShiftL (W32# w) (I# i) = W32# (w `uncheckedShiftLWord32#` i)
#else
w16ShiftL (W16# w) (I# i) = W16# (w `uncheckedShiftL#` i)
w32ShiftL (W32# w) (I# i) = W32# (w `uncheckedShiftL#` i)
#endif

#if MIN_VERSION_ghc_prim(0,9,0)
w64ShiftL (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)
#else
w64ShiftL (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
w16ShiftL = shiftL
w32ShiftL = shiftL
w64ShiftL = shiftL
#endif
