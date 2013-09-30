-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   This module defines a simple key value list which both faster and
--   more suitable for bencode dictionaries.
--
module Data.BEncode.BDict
       ( BKey
       , BDictMap (..)

         -- * Construction
       , Data.BEncode.BDict.empty
       , Data.BEncode.BDict.singleton

         -- * Query
       , Data.BEncode.BDict.lookup

         -- * Combine
       , Data.BEncode.BDict.union

         -- * Transformations
       , Data.BEncode.BDict.map
       , Data.BEncode.BDict.bifoldMap

         -- * Conversion
       , Data.BEncode.BDict.fromAscList
       , Data.BEncode.BDict.toAscList
       ) where

import Control.DeepSeq
import Data.ByteString as BS
import Data.Foldable
import Data.Monoid


type BKey = ByteString

-- STRICTNESS NOTE: the BKey is always evaluated since we either use a
-- literal or compare before insert to the dict
--
-- LAYOUT NOTE: we don't use [StrictPair BKey a] since it introduce
-- one more constructor per cell
--

-- | BDictMap is list of key value pairs sorted by keys.
data BDictMap a
  = Cons !BKey a !(BDictMap a)
  | Nil
    deriving (Show, Read, Eq, Ord)

instance NFData a => NFData (BDictMap a) where
  rnf  Nil         = ()
  rnf (Cons _ v xs)= rnf v `seq` rnf xs

instance Functor BDictMap where
  fmap = Data.BEncode.BDict.map
  {-# INLINE fmap #-}

instance Foldable BDictMap where
  foldMap f = go
    where
      go  Nil          = mempty
      go (Cons _ v xs) = f v `mappend` go xs
  {-# INLINE foldMap #-}

instance Monoid (BDictMap a) where
  mempty  = Data.BEncode.BDict.empty
  mappend = Data.BEncode.BDict.union

empty :: BDictMap a
empty = Nil
{-# INLINE empty #-}

singleton :: BKey -> a -> BDictMap a
singleton k v = Cons k v Nil
{-# INLINE singleton #-}

lookup :: BKey -> BDictMap a -> Maybe a
lookup x = go
  where
    go Nil = Nothing
    go (Cons k v xs)
      |   k == x  = Just v
      | otherwise = go xs
{-# INLINE lookup #-}

union :: BDictMap a -> BDictMap a -> BDictMap a
union Nil xs  = xs
union xs  Nil = xs
union bd @ (Cons k v xs) bd' @ (Cons k' v' xs')
  |   k < k'  = Cons k  v  (union xs bd')
  | otherwise = Cons k' v' (union bd xs')

map :: (a -> b) -> BDictMap a -> BDictMap b
map f = go
  where
    go Nil = Nil
    go (Cons k v xs) = Cons k (f v) (go xs)
{-# INLINE map #-}

bifoldMap :: Monoid m => (BKey -> a -> m) -> BDictMap a -> m
bifoldMap f = go
  where
    go  Nil          = mempty
    go (Cons k v xs) = f k v `mappend` go xs
{-# INLINE bifoldMap #-}

fromAscList :: [(BKey, a)] -> BDictMap a
fromAscList [] = Nil
fromAscList ((k, v) : xs) = Cons k v (fromAscList xs)

toAscList :: BDictMap a -> [(BKey, a)]
toAscList Nil = []
toAscList (Cons k v xs) = (k, v) : toAscList xs
