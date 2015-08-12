{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
module Pinch.Internal.Parser
    ( Parser
    , runParser

    , word8
    , int16
    , int32
    , int64
    , double
    , take
    ) where

import Control.Applicative
import Control.Monad

import Control.Monad.ST (ST)
import Data.Bits        (shiftL, (.|.))
import Data.ByteString  (ByteString)
import Data.Int         (Int16, Int32, Int64)
import Data.Word        (Word8)
import Prelude          hiding (take)

import qualified Control.Monad.ST       as ST
import qualified Data.Array.ST          as A
import qualified Data.Array.Unsafe      as A
import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as BU


type Failure   r = String          -> r
type Success a r = ByteString -> a -> r

-- | A simple ByteString parser.
newtype Parser a = Parser
    { unParser :: forall r.
          ByteString   -- Bytestring being parsed
       -> Failure r    -- Failure continuation
       -> Success a r  -- Success continuation
       -> r
    }


instance Functor Parser where
    fmap f (Parser g) = Parser
        $ \b0 kFail kSucc -> g b0 kFail
        $ \b1 a -> kSucc b1 (f a)


instance Applicative Parser where
    pure a = Parser $ \b _ kSucc -> kSucc b a

    Parser f' <*> Parser a' = Parser
        $ \b0 kFail kSucc -> f' b0 kFail
        $ \b1 f -> a' b1 kFail
        $ \b2 a -> kSucc b2 (f a)


instance Monad Parser where
    return = pure
    (>>) = (*>)

    fail msg = Parser $ \_ kFail _ -> kFail msg

    Parser m >>= k = Parser
        $ \b0 kFail kSucc -> m b0 kFail
        $ \b1 a -> unParser (k a) b1 kFail kSucc


runParser :: Parser a -> ByteString -> Either String a
runParser (Parser f) b = f b Left (const Right)
{-# INLINE runParser #-}


-- | @take n@ gets exactly @n@ bytes or fails the parse.
take :: Int -> Parser ByteString
take n = Parser $ \b kFail kSucc ->
    let l = B.length b
    in if l >= n
        then let requested = BU.unsafeDrop n b
                 remaining = BU.unsafeTake n b
             in kSucc requested remaining
        else kFail
              ("Input is too short. Expected " ++ show n ++ " bytes. " ++
               "Got " ++ show l ++ " bytes.")
{-# INLINE take #-}


-- | Produces the next byte and advances the parser.
word8 :: Parser Word8
word8 = Parser
    $ \b0 kFail kSucc -> case B.uncons b0 of
        Nothing -> kFail "Unexpected end of input."
        Just (w, b1) -> kSucc b1 w
{-# INLINE word8 #-}


-- | Produces a signed 16-bit integer and advances the parser.
int16 :: Parser Int16
int16 = mk <$> take 2
  where
    {-# INLINE mk #-}
    mk b =
        (fromIntegral (b `BU.unsafeIndex` 0) `shiftL` 8) .|.
         fromIntegral (b `BU.unsafeIndex` 1)
{-# INLINE int16 #-}


-- | Produces a signed 32-bit integer and advances the parser.
int32 :: Parser Int32
int32 = mk <$> take 4
  where
    {-# INLINE mk #-}
    mk b =
        (fromIntegral (b `BU.unsafeIndex` 0) `shiftL` 24) .|.
        (fromIntegral (b `BU.unsafeIndex` 1) `shiftL` 16) .|.
        (fromIntegral (b `BU.unsafeIndex` 2) `shiftL`  8) .|.
         fromIntegral (b `BU.unsafeIndex` 3)
{-# INLINE int32 #-}


-- | Produces a signed 64-bit integer and advances the parser.
int64 :: Parser Int64
int64 = mk <$> take 8
  where
    {-# INLINE mk #-}
    mk b =
        (fromIntegral (b `BU.unsafeIndex` 0) `shiftL` 56) .|.
        (fromIntegral (b `BU.unsafeIndex` 1) `shiftL` 48) .|.
        (fromIntegral (b `BU.unsafeIndex` 2) `shiftL` 40) .|.
        (fromIntegral (b `BU.unsafeIndex` 3) `shiftL` 32) .|.
        (fromIntegral (b `BU.unsafeIndex` 4) `shiftL` 24) .|.
        (fromIntegral (b `BU.unsafeIndex` 5) `shiftL` 16) .|.
        (fromIntegral (b `BU.unsafeIndex` 6) `shiftL`  8) .|.
         fromIntegral (b `BU.unsafeIndex` 7)
{-# INLINE int64 #-}


-- | Produces a 64-bit floating point number and advances the parser.
double :: Parser Double
double = do
    i <- int64
    return (ST.runST (cast i))
{-# INLINE double #-}


-- As per: http://stackoverflow.com/a/7002812
cast :: (A.MArray (A.STUArray s) a (ST s),
         A.MArray (A.STUArray s) b (ST s)) => a -> ST s b
cast x = A.newArray (0 :: Int, 0) x >>= A.castSTUArray >>= flip A.readArray 0
{-# INLINE cast #-}
