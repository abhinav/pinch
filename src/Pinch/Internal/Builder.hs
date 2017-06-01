{-# LANGUAGE CPP #-}
-- |
-- Module      :  Pinch.Internal.Builder
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- This module implements a ByteString builder very similar to
-- 'Data.ByteString.Builder' except that it keeps track of its final serialized
-- length. This allows it to allocate the target ByteString in one @malloc@ and
-- simply write the bytes to it.
module Pinch.Internal.Builder
    ( Builder
    , runBuilder

    , append
    , int8
    , word8
    , int16BE
    , int32BE
    , int64BE
    , int64LE
    , doubleBE
    , doubleLE
    , byteString
    ) where

#if __GLASGOW_HASKELL__ < 709
import Data.Monoid
#endif

import Data.ByteString              (ByteString)
import Data.ByteString.Builder.Prim ((>*<))
import Data.Int
import Data.Word                    (Word8)
import Foreign.ForeignPtr           (withForeignPtr)
import Foreign.Ptr                  (Ptr, plusPtr)

import qualified Data.ByteString.Builder.Prim          as BP
import qualified Data.ByteString.Builder.Prim.Internal as BPI
import qualified Data.ByteString.Internal              as BI

-- | A ByteString Builder that knows its final size.
data Builder = B {-# UNPACK #-} !Int (Ptr Word8 -> IO ())

-- | Build a ByteString from the given ByteString builder.
runBuilder :: Builder -> ByteString
runBuilder (B size fill) = BI.unsafeCreate size fill
{-# INLINE runBuilder #-}

-- | Append two Builders into one.
append :: Builder -> Builder -> Builder
append (B ll lf) (B rl rf) = B (ll + rl) (\p -> lf p >> rf (p `plusPtr` ll))
{-# INLINE [1] append #-}
    -- Don't inline append until phase 1. This ensures that the
    -- append/primFixed* rules have a chance to fire.

instance Monoid Builder where
    {-# INLINE mempty #-}
    mempty = B 0 (\_ -> return ())

    {-# INLINE mappend #-}
    mappend = append

    {-# INLINE mconcat #-}
    mconcat = foldr mappend mempty

primFixed :: BP.FixedPrim a -> a -> Builder
primFixed prim a = B (BPI.size prim) (BPI.runF prim a)
{-# INLINE [1] primFixed #-}
    -- Don't inline append until phase 1. This ensures that the
    -- append/primFixed* rules have a chance to fire.

-- The following rules try to join together instances of primFixed that are
-- being appended together. These were adapted almost as-is from
-- ByteString.Builder.Prim's rules around this.

{-# RULES

"append/primFixed" forall p1 p2 v1 v2.
    append (primFixed p1 v1) (primFixed p2 v2) =
        primFixed (p1 >*< p2) (v1, v2)

"append/primFixed/rightAssociative" forall p1 p2 v1 v2 b.
    append (primFixed p1 v1) (append (primFixed p2 v2) b) =
        append (primFixed (p1 >*< p2) (v1, v2)) b

"append/primFixed/leftAssociative" forall p1 p2 v1 v2 b.
    append (append b (primFixed p1 v1)) (primFixed p2 v2) =
        append b (primFixed (p1 >*< p2) (v1, v2))

  #-}

-- | Serialize a single signed byte.
int8 :: Int8 -> Builder
int8 = primFixed BP.int8
{-# INLINE int8 #-}

-- | Serialize a single unsigned byte.
word8 :: Word8 -> Builder
word8 = primFixed BP.word8
{-# INLINE word8 #-}

-- | Serialize a signed 16-bit integer in big endian format.
int16BE :: Int16 -> Builder
int16BE = primFixed BP.int16BE
{-# INLINE int16BE #-}

-- | Serialize a signed 32-bit integer in big endian format.
int32BE :: Int32 -> Builder
int32BE = primFixed BP.int32BE
{-# INLINE int32BE #-}

-- | Serialize a signed 64-bit integer in big endian format.
int64BE :: Int64 -> Builder
int64BE = primFixed BP.int64BE
{-# INLINE int64BE #-}

-- | Serialize a signed 64-bit integer in little endian format.
int64LE :: Int64 -> Builder
int64LE = primFixed BP.int64LE
{-# INLINE int64LE #-}

-- | Serialize a signed 64-bit floating point number in big endian format.
doubleBE :: Double -> Builder
doubleBE = primFixed BP.doubleBE
{-# INLINE doubleBE #-}

-- | Serialize a signed 64-bit floating point number in little endian format.
doubleLE :: Double -> Builder
doubleLE = primFixed BP.doubleLE
{-# INLINE doubleLE #-}


-- | Inlcude the given ByteString as-is in the builder.
--
-- Note that because this operation is applied lazily, we will maintain a
-- reference to the ByteString until the builder is executed.
byteString :: ByteString -> Builder
byteString (BI.PS fp off len) =
    B len $ \dst ->
    withForeignPtr fp $ \src ->
        BI.memcpy dst (src `plusPtr` off) len
{-# INLINE byteString #-}
