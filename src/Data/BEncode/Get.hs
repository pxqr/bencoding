{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.BEncode.Get
       ( FromBEncode (..)
       , getList
       , getDict
       , req
       , opt
       , decode'
       , skip
       ) where

import Control.Applicative as Ap
import Control.Monad
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.BEncode.BDict
import Data.BEncode.Types
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.String

import Data.BEncode.Internal

import Debug.Trace


newtype Get k a = Get { runGet :: Parser a }

instance Functor (Get a) where
  fmap f (Get p) = Get (fmap f p)
  {-# INLINE fmap #-}

instance Applicative (Get BDict) where
  pure = Get . return
  {-# INLINE pure #-}

  Get f <*> Get p = Get (f <*> p)
  {-# INLINE (<*>) #-}

instance Alternative (Get BDict) where
  empty = Get Ap.empty
  {-# INLINE empty #-}

  Get m <|> Get n = Get (m <|> n)
  {-# INLINE (<|>) #-}

instance Monad (Get BDict) where
  return = pure
  {-# INLINE return #-}

  Get m >> Get n = Get (m >> n)
  {-# INLINE (>>) #-}

  Get p >>= f = Get (p >>= runGet . f)
  {-# INLINE (>>=) #-}

  fail = Get . fail
  {-# INLINE fail #-}

instance FromBEncode BValue a => IsString (Get BDict (Maybe a)) where
  fromString = getLookupMaybe . fromString
  {-# INLINE fromString #-}

instance FromBEncode BValue a => IsString (Get BDict a) where
  fromString = getLookup . fromString
  {-# INLINE fromString #-}

getSigned :: (Num a, Integral a) => Get BInteger a
getSigned = Get $ do
  c <- P.peekChar
  case c of
    Just '-' -> do
      P.anyChar
      negate <$> P.decimal
    _        ->  P.decimal
{-# INLINE getSigned #-}

getUnsigned :: Integral a => Get BInteger a
getUnsigned = Get P.decimal
{-# INLINE getUnsigned #-}

getIntegral :: Get BInteger a -> Get BValue a
getIntegral g = Get $ P.char 'i' *> runGet g <*  P.anyChar

getByteString :: Get BValue ByteString
getByteString = Get $ do
  n <- P.decimal :: Parser Int
  P.anyChar -- NOTE: should be ':'
  P.take n
{-# INLINE getByteString #-}


-- TODO make it faster using 'skipWhile'
skip :: Get k ()
skip = Get skipValue
  where
    skipValue = do
      mc <- P.peekChar
      case mc of
        Nothing -> fail "end of input"
        Just c  ->
            case c of
              -- if we have digit it always should be string length
              di | di <= '9' -> skipByteString
              'i' -> P.anyChar *> skipInteger <* P.anyChar
              'l' -> P.anyChar *> skipList <* P.anyChar
              'd' -> P.anyChar *> skipDict <* P.anyChar
              t   -> fail ("bencode unknown tag: " ++ [t])

    skipByteString = void (runGet getByteString)
    skipInteger    = void (runGet (getSigned :: Parser Integer))

    skipList = do
      c <- P.peekChar
      case c of
        Just 'e' -> return ()
        _        -> skipValue >> skipList

    skipDict = (skipByteString >> skipValue) <|> pure ()


getLookup :: FromBEncode BValue a => ByteString -> Get BDict a
getLookup !key = Get search
  where
    search = do
      !k <- runGet getByteString
--      traceShow (key, k) $ pure ()
      case compare k key of
        EQ -> runGet get
        LT -> runGet skip >> search
        GT -> fail $ "key " ++ BC.unpack key ++ " do not exist"

getLookupMaybe :: FromBEncode BValue a => ByteString -> Get BDict (Maybe a)
getLookupMaybe !key = Get search
  where
    step = do
      k <- runGet getByteString
--      traceShow (key, k) $ pure ()
      case compare k key of
        EQ -> Just <$> runGet get
        LT -> runGet skip >> search
        GT -> fail ""

    -- short cirtuiting; otherwise we can get O(n ^ 2)
    search = step <|> pure Nothing

getList :: Get BValue a -> Get BValue [a]
getList g = Get $ do
    P.char 'l'
    loop
  where
    loop = do
      c <- P.peekChar
      case c of
        Just 'e' -> P.anyChar *> pure []
        _        -> (:) <$> runGet g <*> loop

getDict :: Get BDict a -> Get BValue a
getDict (Get p) = Get $ P.char 'd' *> p <* P.anyChar
{-# INLINE getDict #-}

class FromBEncode b a | a -> b where
  get :: Get b a

instance FromBEncode BValue Integer where
  get = getIntegral getSigned
  {-# INLINE get #-}

instance FromBEncode BValue Int where
  get = getIntegral getSigned

instance FromBEncode BValue Bool where
  get = Get $ do
    i <- runGet get
    case i :: Integer of
      0 -> pure False
      1 -> pure True
      _ -> fail "unable to parse bool"

instance FromBEncode BValue ByteString where
  get = getByteString
  {-# INLINE get #-}

instance FromBEncode BValue a => FromBEncode BValue [a] where
  get = getList get

req :: FromBEncode BValue a => BKey -> Get BDict a
req = getLookup
{-# INLINE req #-}

opt :: FromBEncode BValue a => BKey -> Get BDict (Maybe a)
opt = getLookupMaybe
{-# INLINE opt #-}

lookAhead :: Get k a -> Get k a
lookAhead = undefined

decode' :: FromBEncode BValue a => ByteString -> Either String a
decode' = P.parseOnly (runGet get)
