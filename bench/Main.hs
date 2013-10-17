{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where

import Control.Applicative
import Control.DeepSeq
import Data.Attoparsec.ByteString as Atto
import           Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List as L
import Data.Maybe
import Data.Typeable
import System.Environment

import Criterion.Main
import GHC.Generics

import "bencode"   Data.BEncode     as A
import             Data.AttoBencode as B
import             Data.AttoBencode.Parser as B
import "bencoding" Data.BEncode     as C
import "bencoding" Data.BEncode.Internal as C
import "bencoding" Data.BEncode.Types    as C
import "bencoding" Data.BEncode.Get      as C


instance NFData A.BEncode where
    rnf (A.BInt    i) = rnf i
    rnf (A.BString s) = rnf s
    rnf (A.BList   l) = rnf l
    rnf (A.BDict   m) = rnf m

instance NFData B.BValue where
    rnf (B.BInt    i) = rnf i
    rnf (B.BString s) = rnf s
    rnf (B.BList   l) = rnf l
    rnf (B.BDict   d) = rnf d

getRight :: Either String a -> a
getRight = either error id

data List a = Cons a (List a) | Nil
              deriving Generic

instance C.BEncode a => C.BEncode (List a)

instance NFData a => NFData (List a) where
  rnf  Nil        = ()
  rnf (Cons x xs) = rnf (x, xs)

replicate' :: Int -> a -> List a
replicate' c x
    |   c >= 0  = go c
    | otherwise = Nil
  where
    go 0 = Nil
    go n = Cons x $ go (pred n)

{-----------------------------------------------------------------------
--  Big dicts
-----------------------------------------------------------------------}

data FileInfo = FileInfo
    { fiLength      :: !Integer
    , fiMD5sum      :: !(Maybe ByteString)
    , fiPath        :: ![ByteString]
    } deriving (Show, Read, Eq, Typeable)

instance C.ToBEncode C.BValue FileInfo where
  put FileInfo {..} = C.putDict $ do
    "length" C..=   fiLength
    "md5sum" C..=?? fiMD5sum
    "path"   C..=   fiPath

instance C.FromBEncode C.BValue FileInfo where
  get = C.getDict $ do
    FileInfo <$> "length" <*> "md5sum" <*> "path"

instance C.BEncode FileInfo where
  toBEncode FileInfo {..} = toDict $
         "length" .=!  fiLength
    C..: "md5sum" .=? fiMD5sum
    C..: "path"   .=!  fiPath
    C..: endDict

  fromBEncode = fromDict $
    FileInfo <$>! "length"
             <*>? "md5sum"
             <*>! "path"

data ContentInfo =
    SingleFile {
      ciLength       :: !Integer
    , ciMD5sum       :: !(Maybe ByteString)
    , ciName         :: !ByteString
    , ciPieceLength  :: !Int
--    , ciPieces       :: !ByteString
    , ciPrivate      :: !(Maybe Bool)
    }
  | MultiFile {
      ciFiles        :: ![FileInfo]
    , ciName         :: !ByteString
    , ciPieceLength  :: !Int
    , ciPieces       :: !ByteString
    , ciPrivate      :: !(Maybe Bool)
    } deriving (Show, Read, Eq, Typeable)

instance ToBEncode C.BValue ContentInfo where
  put SingleFile {..} = C.putDict $ do
    "length"       C..=   ciLength
    "md5sum"       C..=?? ciMD5sum
    "name"         C..=   ciName
    "piece length" C..=   ciPieceLength
--    "pieces"       C..=   ciPieces
    "private"      C..=?? ciPrivate

  put MultiFile  {..} = undefined

instance FromBEncode C.BValue ContentInfo where
  get = C.getDict $ do
      (SingleFile <$> "length"
                  <*> "md5sum"
                  <*> "name"
                  <*> "piece length"
--                  <*> "pieces"
                  <*> "private"
        )
      <|>
      (MultiFile  <$> "files" <*> "name"
                  <*> "piece length" <*> "pieces"
                  <*> "private")

instance C.BEncode ContentInfo where
  toBEncode SingleFile {..}  = toDict $
         "length"       .=! ciLength
    C..: "md5sum"       .=? ciMD5sum
    C..: "name"         .=! ciName
    C..: "piece length" .=! ciPieceLength
--    C..: "pieces"       .=! ciPieces
    C..: "private"      .=? ciPrivate
    C..: endDict

  toBEncode MultiFile {..} = toDict $
         "files"        .=! ciFiles
    C..: "name"         .=! ciName
    C..: "piece length" .=! ciPieceLength
    C..: "pieces"       .=! ciPieces
    C..: "private"      .=? ciPrivate
    C..: endDict

  fromBEncode = fromDict $
      (MultiFile  <$>! "files"
                  <*>! "name"
                  <*>! "piece length"
                  <*>! "pieces"
                  <*>? "private"
      )
      <|>
      (SingleFile <$>! "length"
                  <*>? "md5sum"
                  <*>! "name"
                  <*>! "piece length"
--                  <*>! "pieces"
                  <*>? "private"
      )

data Torrent = Torrent {
    tAnnounce     :: !ByteString
  , tAnnounceList :: !(Maybe ByteString)
  , tComment      :: !(Maybe ByteString)
  , tCreatedBy    :: !(Maybe ByteString)
  , tCreationDate :: !(Maybe ByteString)
  , tEncoding     :: !(Maybe ByteString)
  , tInfo         :: !ContentInfo
  , tPublisher    :: !(Maybe ByteString)
  , tPublisherURL :: !(Maybe ByteString)
  , tSignature    :: !(Maybe ByteString)
  } deriving (Show, Eq, Typeable)

instance NFData Torrent where
  rnf Torrent {..} = ()

instance C.ToBEncode C.BValue Torrent where
  put Torrent {..} = C.putDict $ do
    "announce"      C..=   tAnnounce
    "announce-list" C..=?? tAnnounceList
    "comment"       C..=?? tComment
    "created by"    C..=?? tCreatedBy
    "creation date" C..=?? tCreationDate
    "encoding"      C..=?? tEncoding
    "info"          C..=   tInfo
    "publisher"     C..=?? tPublisher
    "publisher-url" C..=?? tPublisherURL
    "signature"     C..=?? tSignature

instance C.FromBEncode C.BValue Torrent where
  get = C.getDict $ do
    Torrent <$> "announce"
            <*> "announce-list"
            <*> "comment"
            <*> "created by"
            <*> "creation date"
            <*> "encoding"
            <*> "info"
            <*> "publisher"
            <*> "publisherURL"
            <*> "signature"

instance C.BEncode Torrent where
  toBEncode Torrent {..} = toDict $
         "announce"      .=! tAnnounce
    C..: "announce-list" .=? tAnnounceList
    C..: "comment"       .=? tComment
    C..: "created by"    .=? tCreatedBy
    C..: "creation date" .=? tCreationDate
    C..: "encoding"      .=? tEncoding
    C..: "info"          .=!  tInfo
    C..: "publisher"     .=? tPublisher
    C..: "publisher-url" .=? tPublisherURL
    C..: "signature"     .=? tSignature
    C..: endDict



  fromBEncode = fromDict $ do
    Torrent <$>! "announce"
            <*>? "announce-list"
            <*>? "comment"
            <*>? "created by"
            <*>? "creation date"
            <*>? "encoding"
            <*>! "info"
            <*>? "publisher"
            <*>? "publisher-url"
            <*>? "signature"

{-----------------------------------------------------------------------
--  Main
-----------------------------------------------------------------------}

main :: IO ()
main = do
  (path : args) <- getArgs
  torrentFile   <- BS.readFile path
  let lazyTorrentFile = BL.fromChunks [torrentFile]

  case rnf (torrentFile, lazyTorrentFile) of
    () -> return ()

  print $ (decode' torrentFile :: Either String Torrent)

  withArgs args $
       defaultMain
       [ bench "decode/bencode"     $
           nf A.bRead                            lazyTorrentFile
       , bench "decode/AttoBencode" $
           nf (getRight . Atto.parseOnly bValue) torrentFile
       , bench "decode/bencoding"   $
           nf (getRight . C.parse)              torrentFile

       , let Just v = A.bRead lazyTorrentFile in
         bench "encode/bencode"     $ nf A.bPack v
       , let Right v = Atto.parseOnly bValue torrentFile in
         bench "encode/AttoBencode" $ nf B.encode v
       , let Right v = C.parse torrentFile in
         bench "encode/bencoding"   $ nf C.build v

       , bench "decode+encode/bencode"     $
           nf (A.bPack  . fromJust . A.bRead) lazyTorrentFile
       , bench "decode+encode/AttoBencode" $
           nf (B.encode . getRight . Atto.parseOnly bValue) torrentFile
       , bench "decode+encode/bencoding"   $
           nf (C.build . getRight . C.parse) torrentFile

       , bench "list10000int/bencode/encode" $
           nf (A.bPack . A.BList . L.map (A.BInt . fromIntegral))
              [0..10000 :: Int]

       , bench "list10000int/attobencode/encode" $
           nf B.encode [1..20000 :: Int]
       , bench "list10000int/bencoding/encode" $
           nf C.encode [1..20000 :: Int]


       , let d = A.bPack $ A.BList $
                 L.map A.BInt (L.replicate 1000 (0 :: Integer))
         in d `seq` (bench "list1000int/bencode/decode" $ nf
            (fromJust . A.bRead :: BL.ByteString -> A.BEncode) d)

       , let d = BL.toStrict (C.encode (L.replicate 10000 ()))
         in d `seq` (bench "list10000unit/bencoding/decode" $ nf
            (C.decode :: BS.ByteString -> Either String [()]) d)

       , let d = BL.toStrict $ C.encode $ L.replicate 10000 (0 :: Int)
         in d `seq` (bench "list10000int/bencoding/decode" $ nf
            (C.decode :: BS.ByteString -> Either String [Int]) d)

       , let d = L.replicate 10000 0
         in bench "list10000int/bencoding/encode>>decode" $
            nf  (getRight . C.decode . BL.toStrict . C.encode
                 :: [Int] ->  [Int] )
                d

       , let d = replicate' 10000 0
         in bench "list10000int/bencoding/encode>>decode/generic" $
            nf (getRight . C.decode . BL.toStrict . C.encode
                :: List Int -> List Int)
               d

       , let Right !be = C.parse torrentFile
             id'   x  = let t = either error id (C.decode' x)
                        in BL.toStrict $ C.encode' (t :: Torrent)
             !_ = let Right t = C.decode torrentFile
                     in if C.decode' (BL.toStrict (C.encode' t))
                           /= Right (t :: Torrent)
                        then error "invalid instance: BEncode Torrent"
                        else True

             replFn m f = go m
               where go 0 = id
                     go n = f . go (pred n)

         in bench "bigdict" $ nf (replFn (1000 :: Int) id') torrentFile

       , let fn x = let Right t = C.decode x in t :: Torrent
         in bench "torrent/decode" $ nf fn torrentFile
       ]
