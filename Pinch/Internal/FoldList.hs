{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  Pinch.Internal.FoldList
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Implements a representation of a list as a fold over it.
module Pinch.Internal.FoldList
    ( FoldList
    , map
    , replicate
    , replicateM
    , F.foldl'
    , F.foldr
    , F.toList
    , fromFoldable
    , T.mapM
    , T.sequence
    ) where

import Prelude hiding (foldr, map, mapM, replicate, sequence)

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Control.DeepSeq (NFData (..))
import Data.Hashable   (Hashable (..))
import Data.List       (intercalate)
import Data.Monoid
import Data.Typeable   (Typeable)

import qualified Data.Foldable    as F
import qualified Data.List        as L
import qualified Data.Traversable as T

-- | FoldList represents a list as a @foldl'@ traversal over it.
--
-- This allows us to avoid allocating new collections for an intermediate
-- representation of various data types that users provide.
newtype FoldList a = FoldList (forall r. (r -> a -> r) -> r -> r)
  deriving Typeable

-- | Builds a FoldList from a Foldable.
fromFoldable :: F.Foldable f => f a -> FoldList a
fromFoldable l = FoldList (\k r -> F.foldl' k r l)
{-# INLINE fromFoldable #-}

-- | Applies the given function to all elements in the FoldList.
--
-- Note that the function is applied lazily when the results are requested. If
-- the results of the same FoldList are requested multiple times, the function
-- will be called multiple times on the same elements.
map :: (a -> b) -> FoldList a -> FoldList b
map = fmap
{-# INLINE map #-}

-- | Returns a FoldList with the given item repeated @n@ times.
replicate :: Int -> a -> FoldList a
replicate n a = fromFoldable (L.replicate n a)
{-# INLINE replicate #-}

-- | Executes the given monadic action the given number of times and returns
-- a FoldList of the results.
replicateM :: Monad m => Int -> m a -> m (FoldList a)
replicateM n m = T.sequence (replicate n m)
{-# INLINE replicateM #-}

instance Show a => Show (FoldList a) where
    show l = "[" ++ intercalate ", " (F.foldr go [] l) ++ "]"
      where
        go a xs = show a:xs

instance Functor FoldList where
    fmap f (FoldList l) = FoldList $ \k r0 -> l (\r1 a -> k r1 (f a)) r0

instance F.Foldable FoldList where
    foldMap f (FoldList l) = l (\r a -> r <> f a) mempty
    foldl' f r (FoldList l) = l f r

instance T.Traversable FoldList where
    sequenceA (FoldList f) =
        f (\l a -> go <$> l <*> a) (pure (FoldList (\_ r -> r)))
      where
        go (FoldList xs) x = FoldList (\k r -> k (xs k r) x)
        {-# INLINE go #-}

instance Eq a => Eq (FoldList a) where
    l == r = F.toList l == F.toList r

instance NFData a => NFData (FoldList a) where
    rnf (FoldList l) = l (\() a -> rnf a `seq` ()) ()

instance Hashable a => Hashable (FoldList a) where
    hashWithSalt s (FoldList l) = l hashWithSalt s
