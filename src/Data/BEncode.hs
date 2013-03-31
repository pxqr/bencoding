-- | This module is intented to be imported qualified.
module Data.BEncode
       ( -- ^ Datatype
         BEncode(..)

         -- ^ Construction
       , string, integer, list, dict
       , int, charstring, dictAssoc

         -- ^ Destructuring
         -- ^ Serialization
       , encode, decode

         -- ^ Extra
       , builder, parser, printPretty

         -- ^ Predicates
       , isInteger, isString, isList, isDict
       ) where


import Control.Applicative
import Data.Int
import Data.Foldable
import Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP ()
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

integer :: Integer -> BEncode
integer = BInteger . fromIntegral
{-# INLINE integer #-}

string :: ByteString -> BEncode
string = BString
{-# INLINE string #-}

list :: [BEncode] -> BEncode
list = BList
{-# INLINE list #-}

dict :: Dict -> BEncode
dict = BDict
{-# INLINE dict #-}


int :: Int -> BEncode
int = integer . fromIntegral
{-# INLINE int #-}

charstring :: String -> BEncode
charstring = string . B.pack . map (toEnum . fromEnum)
{-# INLINE charstring #-}

dictAssoc :: [(ByteString, BEncode)] -> BEncode
dictAssoc = dict . M.fromList
{-# INLINE dictAssoc #-}


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
