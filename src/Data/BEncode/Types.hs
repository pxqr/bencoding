-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Types for working with bencode data.
--
module Data.BEncode.Types
       ( BInteger
       , BString
       , BList
       , BDict
       , BValue (..)
       ) where

import Control.DeepSeq
import Data.ByteString

import Data.BEncode.BDict

-- | A bencode "integer".
type BInteger = Integer

-- | A raw bencode string.
type BString  = ByteString

-- | A plain bencode list.
type BList    = [BValue]

-- | A bencode dictionary.
type BDict    = BDictMap BValue

-- | 'BValue' is straightforward ADT for b-encoded values. Please
-- note that since dictionaries are sorted, in most cases we can
-- compare BEncoded values without serialization and vice versa.
-- Lists is not required to be sorted through.
--
data BValue
  = BInteger !BInteger -- ^ bencode integers;
  | BString  !BString  -- ^ bencode strings;
  | BList     BList    -- ^ list of bencode values;
  | BDict     BDict    -- ^ bencode key-value dictionary.
    deriving (Show, Read, Eq, Ord)

instance NFData BValue where
    rnf (BInteger i) = rnf i
    rnf (BString  s) = rnf s
    rnf (BList    l) = rnf l
    rnf (BDict    d) = rnf d
