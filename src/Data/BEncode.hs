-- | This module is intented to be imported qualified.
{-# LANGUAGE FlexibleInstances #-}
module Data.BEncode
       ( -- * Datatype
         BEncode(..)

         -- * Construction && Destructuring
       , BEncodable (..), dictAssoc, Result

         -- ** Dictionaries
         -- *** Building
       , (-->), (-->?), fromAssocs, fromAscAssocs

         -- *** Extraction
       , reqKey, optKey, (>--), (>--?)

         -- * Serialization
       , encode, decode

         -- * Extra
       , builder, parser, decodingError, printPretty

         -- * Predicates
       , isInteger, isString, isList, isDict
       ) where


import Control.Applicative
import Data.Int
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Foldable (foldMap)
import Data.Traversable (traverse)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP ()
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Text.PrettyPrint.ANSI.Leijen (Pretty, Doc, pretty, (<+>), (</>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type Dict = Map ByteString BEncode

-- | 'BEncode' is straightforward AST for b-encoded values.
--   Please note that since dictionaries are sorted, in most cases we can
--   compare BEncoded values without serialization and vice versa.
--   Lists is not required to be sorted through.
--   Also note that 'BEncode' have JSON-like instance for 'Pretty'.
--
data BEncode = BInteger Int64
             | BString  ByteString
             | BList    [BEncode]
             | BDict    Dict
               deriving (Show, Read, Eq, Ord)

type Result = Either String

class BEncodable a where
  toBEncode   :: a -> BEncode
  fromBEncode :: BEncode -> Result a
--  isEncodable :: BEncode -> Bool
--  bencoding :: Iso a
--  bencoding = Iso (Right . toBencode) fromBEncode

decodingError :: String -> Result a
decodingError s = Left ("fromBEncode: unable to decode " ++ s)
{-# INLINE decodingError #-}

instance BEncodable BEncode where
  toBEncode = id
  {-# INLINE toBEncode #-}

  fromBEncode = Right
  {-# INLINE fromBEncode #-}

instance BEncodable Int where
  toBEncode = BInteger . fromIntegral
  {-# INLINE toBEncode #-}

  fromBEncode (BInteger i) = Right (fromIntegral i)
  fromBEncode _            = decodingError "integer"
  {-# INLINE fromBEncode #-}


instance BEncodable Bool where
  toBEncode = toBEncode . fromEnum
  {-# INLINE toBEncode #-}

  fromBEncode b = do
    i <- fromBEncode b
    case i :: Int of
      0 -> return False
      1 -> return True
      _ -> decodingError "bool"
  {-# INLINE fromBEncode #-}


instance BEncodable Integer where
  toBEncode = BInteger . fromIntegral
  {-# INLINE toBEncode #-}

  fromBEncode b = fromIntegral <$> (fromBEncode b :: Result Int)
  {-# INLINE fromBEncode #-}


instance BEncodable ByteString where
  toBEncode = BString
  {-# INLINE toBEncode #-}

  fromBEncode (BString s) = Right s
  fromBEncode _           = decodingError "string"
  {-# INLINE fromBEncode #-}


instance BEncodable Text where
  toBEncode = toBEncode . T.encodeUtf8
  {-# INLINE toBEncode #-}

  fromBEncode b = T.decodeUtf8 <$> fromBEncode b
  {-# INLINE fromBEncode #-}


{-
instance BEncodable String where
  toBEncode = BString . BC.pack
  {-# INLINE toBEncode #-}

  fromBEncode (BString s) = Just (BC.unpack s)
  fromBEncode _           = Nothing
  {-# INLINE fromBEncode #-}
-}

instance BEncodable a => BEncodable [a] where
  {-# SPECIALIZE instance BEncodable [BEncode] #-}

  toBEncode = BList . map toBEncode
  {-# INLINE toBEncode #-}

  fromBEncode (BList xs) = mapM fromBEncode xs
  fromBEncode _          = decodingError "list"
  {-# INLINE fromBEncode #-}

instance BEncodable a => BEncodable (Map ByteString a) where
  {-# SPECIALIZE instance BEncodable (Map ByteString BEncode) #-}

  toBEncode = BDict . M.map toBEncode
  {-# INLINE toBEncode #-}

  fromBEncode (BDict d) = traverse fromBEncode d
  fromBEncode _         = decodingError "dictionary"
  {-# INLINE fromBEncode #-}

dictAssoc :: [(ByteString, BEncode)] -> BEncode
dictAssoc = BDict . M.fromList
{-# INLINE dictAssoc #-}

------------------------------------- Building dictionaries --------------------
data Assoc = Required ByteString BEncode
           | Optional ByteString (Maybe BEncode)

(-->) :: BEncodable a => ByteString -> a -> Assoc
key --> val = Required key (toBEncode val)
{-# INLINE (-->) #-}

(-->?) :: BEncodable a => ByteString -> Maybe a -> Assoc
key -->? mval = Optional key (toBEncode <$> mval)
{-# INLINE (-->?) #-}

fromAssocs :: [Assoc] -> BEncode
fromAssocs = BDict . M.fromList . mapMaybe unpackAssoc
  where
    unpackAssoc (Required n v)        = Just (n, v)
    unpackAssoc (Optional n (Just v)) = Just (n, v)
    unpackAssoc (Optional _ Nothing)  = Nothing

-- | A faster version of 'fromAssocs'.
--   Should be used only when keys are sorted by ascending.
fromAscAssocs :: [Assoc] -> BEncode
fromAscAssocs = error "fromAscAssocs"

------------------------------------ Extraction --------------------------------
reqKey :: BEncodable a => Map ByteString BEncode -> ByteString -> Result a
reqKey d key
  | Just b <- M.lookup key d = fromBEncode b
  |        otherwise         = Left ("required field `" ++ BC.unpack key ++ "' not found")

optKey :: BEncodable a => Map ByteString BEncode -> ByteString -> Result (Maybe a)
optKey d key
  | Just b <- M.lookup key d
  , Right r <- fromBEncode b = return (Just r)
  | otherwise                = return Nothing

(>--) :: BEncodable a => Map ByteString BEncode -> ByteString -> Result a
(>--) = reqKey
{-# INLINE (>--) #-}

(>--?) :: BEncodable a => Map ByteString BEncode -> ByteString -> Result (Maybe a)
(>--?) = optKey
{-# INLINE (>--?) #-}

------------------------------------- Predicates -------------------------------
isInteger :: BEncode -> Bool
isInteger (BInteger _) = True
isInteger _            = False
{-# INLINE isInteger #-}

isString :: BEncode -> Bool
isString (BString _) = True
isString _           = False
{-# INLINE isString #-}

isList :: BEncode -> Bool
isList (BList _) = True
isList _         = False
{-# INLINE isList #-}

isDict :: BEncode -> Bool
isDict (BList _) = True
isDict _         = False
{-# INLINE isDict #-}

encode :: BEncode -> Lazy.ByteString
encode = B.toLazyByteString . builder

decode :: ByteString -> Either String BEncode
decode = P.parseOnly parser


builder :: BEncode -> B.Builder
builder = go
    where
      go (BInteger i) = B.word8 (c2w 'i') <>
                        B.intDec (fromIntegral i) <> -- TODO FIXME
                        B.word8 (c2w 'e')
      go (BString  s) = buildString s
      go (BList    l) = B.word8 (c2w 'l') <>
                        foldMap go l <>
                        B.word8 (c2w 'e')
      go (BDict    d) = B.word8 (c2w 'd') <>
                        foldMap mkKV (M.toAscList d) <>
                        B.word8 (c2w 'e')
          where
            mkKV (k, v) = buildString k <> go v

      buildString s = B.intDec (B.length s) <>
                      B.word8 (c2w ':') <>
                      B.byteString s
      {-# INLINE buildString #-}


parser :: Parser BEncode
parser = valueP
  where
    valueP = do
      mc <- P.peekChar
      case mc of
        Nothing -> fail "end of input"
        Just c  ->
            case c of
              -- if we have digit it always should be string length
              di | di <= '9' -> BString <$> stringP
              'i' -> P.anyChar *> ((BInteger <$> integerP)    <* P.anyChar)
              'l' -> P.anyChar *> ((BList    <$> many valueP) <* P.anyChar)
              'd' -> do
                     P.anyChar
                     (BDict . M.fromDistinctAscList <$> many ((,) <$> stringP <*> valueP))
                        <* P.anyChar
              _   -> fail ""

    stringP :: Parser ByteString
    stringP = do
      n <- P.decimal :: Parser Int
      P.char ':'
      P.take n
    {-# INLINE stringP #-}

    integerP :: Parser Int64
    integerP = negate <$> (P.char8 '-' *> P.decimal)
           <|> P.decimal
    {-# INLINE integerP #-}


printPretty :: BEncode -> IO ()
printPretty = print . pretty

ppBS :: ByteString -> Doc
ppBS = PP.string . map w2c . B.unpack

instance Pretty BEncode where
    pretty (BInteger i) = PP.integer (fromIntegral i)
    pretty (BString  s) = ppBS s
    pretty (BList    l) = PP.lbracket <+>
                   PP.hsep (PP.punctuate PP.comma (map PP.pretty l)) <+>
                          PP.rbracket
    pretty (BDict    d) =
      PP.align $ PP.lbrace <+>
           PP.vsep (PP.punctuate PP.comma (map ppKV (M.toAscList d))) </>
          PP.rbrace
      where
        ppKV (k, v) = ppBS k <+> PP.colon <+> PP.pretty v
