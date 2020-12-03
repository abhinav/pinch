{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
-- |
-- Module      :  Pinch.Internal.Parser
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Implements a basic parser for binary data. The parser does not do any extra
-- book-keeping besides keeping track of the current position in the
-- ByteString.
module Pinch.Internal.Parser
    ( Parser
    , runParser
    , runParser'

    , int8
    , word8
    , int16
    , int32
    , int64
    , int64LE
    , double
    , doubleLE
    , take
    ) where

import Control.Applicative
import Control.Monad

import Control.Monad.ST (ST)
import Data.Bits        ((.|.))
import Data.ByteString  (ByteString)
import Data.Int         (Int16, Int32, Int64, Int8)
import Data.Word        (Word8)
import Prelude          hiding (take)

import qualified Control.Monad.ST       as ST
import qualified Data.Array.ST          as A
import qualified Data.Array.Unsafe      as A
import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as BU

import Pinch.Internal.Bits

-- | Failure continuation. Called with the failure message.
type Failure   r = String          -> r
type Success a r = ByteString -> a -> r
-- ^ Success continuation. Called with the remaining bytestring and the
-- result.

-- | A simple ByteString parser.
newtype Parser a = Parser
    { unParser :: forall r.
          ByteString   -- Bytestring being parsed
       -> Failure r    -- Failure continuation
       -> Success a r  -- Success continuation
       -> r
    }


instance Functor Parser where
    {-# INLINE fmap #-}
    fmap f (Parser g) = Parser
        $ \b0 kFail kSucc -> g b0 kFail
        $ \b1 a -> kSucc b1 (f a)


instance Applicative Parser where
    {-# INLINE pure #-}
    pure a = Parser $ \b _ kSucc -> kSucc b a

    {-# INLINE (<*>) #-}
    Parser f' <*> Parser a' = Parser
        $ \b0 kFail kSucc -> f' b0 kFail
        $ \b1 f -> a' b1 kFail
        $ \b2 a -> kSucc b2 (f a)


instance Monad Parser where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>) #-}
    (>>) = (*>)

    {-# INLINE (>>=) #-}
    Parser m >>= k = Parser
        $ \b0 kFail kSucc -> m b0 kFail
        $ \b1 a -> unParser (k a) b1 kFail kSucc

instance MonadFail Parser where
    {-# INLINE fail #-}
    fail msg = Parser $ \_ kFail _ -> kFail msg

-- | Run the parser on the given ByteString. Return either the failure message
-- or the result.
runParser :: Parser a -> ByteString -> Either String a
runParser (Parser f) b = f b Left (const Right)
{-# INLINE runParser #-}


-- | Run the parser on the given ByteString. Return either the failure message
-- or the result and any left-over content.
runParser' :: Parser a -> ByteString -> Either String (ByteString, a)
runParser' (Parser f) b = f b Left (\b' r -> Right (b', r))
{-# INLINE runParser' #-}


-- | @take n@ gets exactly @n@ bytes or fails the parse.
take :: Int -> Parser ByteString
take n = Parser $ \b kFail kSucc ->
    let l = B.length b
    in if l >= n
        then let remaining = BU.unsafeDrop n b
                 requested = BU.unsafeTake n b
             in kSucc remaining requested
        else kFail
              ("Input is too short. Expected " ++ show n ++ " bytes. " ++
               "Got " ++ show l ++ " bytes.")
{-# INLINE take #-}


-- | Produces the next byte and advances the parser.
int8 :: Parser Int8
int8 = Parser
    $ \b0 kFail kSucc -> case B.uncons b0 of
        Nothing -> kFail "Input is too short. Expected 1 bytes. Got 0 bytes."
        Just (w, b1) -> kSucc b1 (fromIntegral w)
{-# INLINE int8 #-}


-- | Produces the next byte and advances the parser.
word8 :: Parser Word8
word8 = fromIntegral <$> int8
{-# INLINE word8 #-}


-- | Produces a signed 16-bit integer and advances the parser.
int16 :: Parser Int16
int16 = mk <$> take 2
  where
    {-# INLINE mk #-}
    mk b = fromIntegral $
        (fromIntegral (b `BU.unsafeIndex` 0) `w16ShiftL` 8) .|.
         fromIntegral (b `BU.unsafeIndex` 1)
{-# INLINE int16 #-}


-- | Produces a signed 32-bit integer and advances the parser.
int32 :: Parser Int32
int32 = mk <$> take 4
  where
    {-# INLINE mk #-}
    mk b = fromIntegral $
        (fromIntegral (b `BU.unsafeIndex` 0) `w32ShiftL` 24) .|.
        (fromIntegral (b `BU.unsafeIndex` 1) `w32ShiftL` 16) .|.
        (fromIntegral (b `BU.unsafeIndex` 2) `w32ShiftL`  8) .|.
         fromIntegral (b `BU.unsafeIndex` 3)
{-# INLINE int32 #-}


-- | Produces a signed 64-bit integer and advances the parser.
int64 :: Parser Int64
int64 = mk <$> take 8
  where
    {-# INLINE mk #-}
    mk b = fromIntegral $
        (fromIntegral (b `BU.unsafeIndex` 0) `w64ShiftL` 56) .|.
        (fromIntegral (b `BU.unsafeIndex` 1) `w64ShiftL` 48) .|.
        (fromIntegral (b `BU.unsafeIndex` 2) `w64ShiftL` 40) .|.
        (fromIntegral (b `BU.unsafeIndex` 3) `w64ShiftL` 32) .|.
        (fromIntegral (b `BU.unsafeIndex` 4) `w64ShiftL` 24) .|.
        (fromIntegral (b `BU.unsafeIndex` 5) `w64ShiftL` 16) .|.
        (fromIntegral (b `BU.unsafeIndex` 6) `w64ShiftL`  8) .|.
         fromIntegral (b `BU.unsafeIndex` 7)
{-# INLINE int64 #-}

-- | Produces a signed 64-bit integer (parsed in little endian byte ordering)
-- and advances the parser.
int64LE :: Parser Int64
int64LE = mk <$> take 8
  where
    {-# INLINE mk #-}
    mk b = fromIntegral $
        (fromIntegral (b `BU.unsafeIndex` 7) `w64ShiftL` 56) .|.
        (fromIntegral (b `BU.unsafeIndex` 6) `w64ShiftL` 48) .|.
        (fromIntegral (b `BU.unsafeIndex` 5) `w64ShiftL` 40) .|.
        (fromIntegral (b `BU.unsafeIndex` 4) `w64ShiftL` 32) .|.
        (fromIntegral (b `BU.unsafeIndex` 3) `w64ShiftL` 24) .|.
        (fromIntegral (b `BU.unsafeIndex` 2) `w64ShiftL` 16) .|.
        (fromIntegral (b `BU.unsafeIndex` 1) `w64ShiftL`  8) .|.
         fromIntegral (b `BU.unsafeIndex` 0)
{-# INLINE int64LE #-}

-- | Produces a 64-bit floating point number and advances the parser.
double :: Parser Double
double = int64 >>= \i -> return (ST.runST (cast i))
{-# INLINE double #-}

-- | Produces a 64-bit floating point number (parsed in little endian byte
-- ordering) and advances the parser.
doubleLE :: Parser Double
doubleLE = int64LE >>= \i -> return (ST.runST (cast i))
{-# INLINE doubleLE #-}


cast :: (A.MArray (A.STUArray s) a (ST s),
         A.MArray (A.STUArray s) b (ST s)) => a -> ST s b
-- As per: http://stackoverflow.com/a/7002812
cast x = A.newArray (0 :: Int, 0) x >>= A.castSTUArray >>= flip A.readArray 0
{-# INLINE cast #-}
