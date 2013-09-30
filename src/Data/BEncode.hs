-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   This module provides convinient and fast way to serialize,
--   deserealize and construct/destructure Bencoded values with
--   optional fields.
--
--   It supports four different types of values:
--
--     * byte strings — represented as 'ByteString';
--
--     * integers     — represented as 'Integer';
--
--     * lists        - represented as ordinary lists;
--
--     * dictionaries — represented as 'Map';
--
--    To serialize any other types we need to make conversion.  To
--    make conversion more convenient there is type class for it:
--    'BEncodable'. Any textual strings are considered as UTF8 encoded
--    'Text'.
--
--    The complete Augmented BNF syntax for bencoding format is:
--
--
--    > <BE>    ::= <DICT> | <LIST> | <INT> | <STR>
--    >
--    > <DICT>  ::= "d" 1 * (<STR> <BE>) "e"
--    > <LIST>  ::= "l" 1 * <BE>         "e"
--    > <INT>   ::= "i"     <SNUM>       "e"
--    > <STR>   ::= <NUM> ":" n * <CHAR>; where n equals the <NUM>
--    >
--    > <SNUM>  ::= "-" <NUM> / <NUM>
--    > <NUM>   ::= 1 * <DIGIT>
--    > <CHAR>  ::= %
--    > <DIGIT> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
--
--    This module is considered to be imported qualified.
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
#endif

module Data.BEncode
       ( BValue (..)

         -- * Conversion
       , BEncode (..)

         -- * Serialization
       , encode
       , decode

         -- ** Dictionaries
         -- *** Building
       , Assoc
       , (.=!)
       , (.=?)
       , (.:)
       , endDict
       , toDict

         -- *** Extraction
       , Get
       , Result
       , decodingError
       , fromDict

       , next
       , req
       , opt
       , field

       , (<$>!)
       , (<$>?)
       , (<*>!)
       , (<*>?)
       ) where


import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Int
import Data.List as L
import Data.Monoid
import Data.Word          (Word8, Word16, Word32, Word64, Word)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           Data.Version
import qualified Text.ParserCombinators.ReadP as ReadP

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

import Data.BEncode.BDict as BD
import Data.BEncode.Internal
import Data.BEncode.Types


-- | Result used in decoding operations.
type Result = Either String

-- | This class is used to define new datatypes that could be easily
-- serialized using bencode format.
--
--   By default 'BEncodable' have a generic implementation; suppose
--   the following datatype:
--
-- > data List a = Cons { _head  :: a
-- >                    , __tail :: (List a) }
-- >             | Nil
-- >               deriving Generic
--
--   If we don't need to obey any particular specification or
--   standard, the default instance could be derived automatically
--   from the 'Generic' instance:
--
-- > instance BEncodable a => BEncodable (List a)
--
--   Example of derived 'toBEncode' result:
--
-- > > toBEncode (Cons 123 $ Cons 1 Nil)
-- > BDict (fromList [("head",BInteger 123),("tail",BList [])])
--
--  Note that '_' prefixes are omitted.
--
class BEncode a where
  -- | See an example of implementation here 'Assoc'
  toBEncode   :: a -> BValue

#if __GLASGOW_HASKELL__ >= 702
  default toBEncode
    :: Generic a
    => GBEncodable (Rep a) BValue
    => a -> BValue

  toBEncode = gto . from
#endif

  -- | See an example of implementation here 'reqKey'.
  fromBEncode :: BValue -> Result a

#if __GLASGOW_HASKELL__ >= 702
  default fromBEncode
    :: Generic a
    => GBEncodable (Rep a) BValue
    => BValue -> Result a

  fromBEncode x = to <$> gfrom x
#endif

-- | Typically used to throw an decoding error in fromBEncode; when
-- BEncode value to match expected value.
decodingError :: String -> Result a
decodingError s = Left ("fromBEncode: unable to decode " ++ s)
{-# INLINE decodingError #-}

{--------------------------------------------------------------------
  Generics
--------------------------------------------------------------------}

{- NOTE: SELECTORS FOLDING/UNFOLDING
Both List and Map are monoids:

* if fields are named, we fold record to the map;
* otherwise we collect fields using list;

and then unify them using BDict and BList constrs.
-}

#if __GLASGOW_HASKELL__ >= 702

class GBEncodable f e where
  gto   :: f a -> e
  gfrom :: e -> Result (f a)

instance BEncode f
      => GBEncodable (K1 R f) BValue where
  {-# INLINE gto #-}
  gto = toBEncode . unK1

  {-# INLINE gfrom #-}
  gfrom x = K1 <$> fromBEncode x

instance (Eq e, Monoid e)
      => GBEncodable U1 e where
  {-# INLINE gto #-}
  gto U1 = mempty

  {-# INLINE gfrom #-}
  gfrom x
    | x == mempty = pure U1
    |   otherwise = decodingError "U1"

instance (GBEncodable a BList, GBEncodable b BList)
      => GBEncodable (a :*: b) BList where
  {-# INLINE gto #-}
  gto (a :*: b) = gto a ++ gto b

  {-# INLINE gfrom #-}
  gfrom (x : xs) = (:*:) <$> gfrom [x] <*> gfrom xs
  gfrom []       = decodingError "generic: not enough fields"

instance (GBEncodable a BDict, GBEncodable b BDict)
      => GBEncodable (a :*: b) BDict where
  {-# INLINE gto #-}
  gto (a :*: b) = gto a <> gto b

  {-# INLINE gfrom #-}
  -- Just look at this! >.<
  gfrom dict = (:*:) <$> gfrom dict <*> gfrom dict


instance (GBEncodable a e, GBEncodable b e)
      =>  GBEncodable (a :+: b) e where
  {-# INLINE gto #-}
  gto (L1 x) = gto x
  gto (R1 x) = gto x

  {-# INLINE gfrom #-}
  gfrom x = case gfrom x of
    Right lv -> return (L1 lv)
    Left  le -> do
      case gfrom x of
        Right rv -> return (R1 rv)
        Left  re -> decodingError $ "generic: both" ++ le ++ " " ++ re

selRename :: String -> String
selRename = dropWhile ('_'==)

gfromM1S :: forall c. Selector c
         => GBEncodable f BValue
         => BDict -> Result (M1 i c f p)
gfromM1S dict
  | Just va <- BD.lookup (BC.pack (selRename name)) dict = M1 <$> gfrom va
  | otherwise = decodingError $ "generic: Selector not found " ++ show name
  where
    name = selName (error "gfromM1S: impossible" :: M1 i c f p)

instance (Selector s, GBEncodable f BValue)
       => GBEncodable (M1 S s f) BDict where
  {-# INLINE gto #-}
  gto s @ (M1 x) = BC.pack (selRename (selName s)) `BD.singleton` gto x

  {-# INLINE gfrom #-}
  gfrom = gfromM1S

-- TODO DList
instance GBEncodable f BValue
      => GBEncodable (M1 S s f) BList where
  {-# INLINE gto #-}
  gto (M1 x) = [gto x]

  gfrom [x] = M1 <$> gfrom x
  gfrom _   = decodingError "generic: empty selector"
  {-# INLINE gfrom #-}

instance (Constructor c, GBEncodable f BDict, GBEncodable f BList)
       => GBEncodable (M1 C c f) BValue where
  {-# INLINE gto #-}
  gto con @ (M1 x)
      | conIsRecord con = BDict (gto x)
      |    otherwise    = BList (gto x)

  {-# INLINE gfrom #-}
  gfrom (BDict a) = M1 <$> gfrom a
  gfrom (BList a) = M1 <$> gfrom a
  gfrom _         = decodingError "generic: Constr"

instance GBEncodable f e
      => GBEncodable (M1 D d f) e where
  {-# INLINE gto #-}
  gto (M1 x) = gto x

  {-# INLINE gfrom #-}
  gfrom x = M1 <$> gfrom x

#endif

{--------------------------------------------------------------------
--  Native instances
--------------------------------------------------------------------}

instance BEncode BValue where
  toBEncode = id
  {-# INLINE toBEncode #-}

  fromBEncode = pure
  {-# INLINE fromBEncode #-}

instance BEncode BInteger where
  toBEncode = BInteger
  {-# INLINE toBEncode #-}

  fromBEncode (BInteger i) = pure i
  fromBEncode _            = decodingError "BInteger"
  {-# INLINE fromBEncode #-}

instance BEncode BString where
  toBEncode = BString
  {-# INLINE toBEncode #-}

  fromBEncode (BString s) = pure s
  fromBEncode _           = decodingError "BString"
  {-# INLINE fromBEncode #-}

{- NOTE: those overlap with instance BEncodable a => BEncodable [a]

instance BEncodable BList where
  toBEncode = BList
  {-# INLINE toBEncode #-}

  fromBEncode (BList xs) = pure xs
  fromBEncode _          = decodingError "BList"
  {-# INLINE fromBEncode #-}

-}

instance BEncode BDict where
  toBEncode   = BDict
  {-# INLINE toBEncode #-}

  fromBEncode (BDict d) = pure d
  fromBEncode _         = decodingError "BDict"
  {-# INLINE fromBEncode #-}

{--------------------------------------------------------------------
--  Integral instances
--------------------------------------------------------------------}

{- NOTE: instance Integral a => BEncodable a
   requires -XUndecidableInstances, so we avoid it
-}

toBEncodeIntegral :: Integral a => a -> BValue
toBEncodeIntegral = BInteger . fromIntegral
{-# INLINE toBEncodeIntegral #-}

fromBEncodeIntegral :: forall a. Typeable a => Integral a => BValue -> Result a
fromBEncodeIntegral (BInteger i) = pure (fromIntegral i)
fromBEncodeIntegral _
  = decodingError $ show $ typeOf (error "fromBEncodeIntegral: imposible" :: a)
{-# INLINE fromBEncodeIntegral #-}


instance BEncode Word8 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Word16 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Word32 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Word64 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Word where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Int8 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Int16 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Int32 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Int64 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Int where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

{--------------------------------------------------------------------
--  Derived instances
--------------------------------------------------------------------}

instance BEncode Bool where
  toBEncode = toBEncode . fromEnum
  {-# INLINE toBEncode #-}

  fromBEncode b = do
    i <- fromBEncode b
    case i :: Int of
      0 -> return False
      1 -> return True
      _ -> decodingError "Bool"
  {-# INLINE fromBEncode #-}

instance BEncode Text where
  toBEncode = toBEncode . T.encodeUtf8
  {-# INLINE toBEncode #-}

  fromBEncode b = T.decodeUtf8 <$> fromBEncode b
  {-# INLINE fromBEncode #-}

instance BEncode a => BEncode [a] where
  {-# SPECIALIZE instance BEncode BList #-}
  toBEncode = BList . L.map toBEncode
  {-# INLINE toBEncode #-}

  fromBEncode (BList xs) = mapM fromBEncode xs
  fromBEncode _          = decodingError "list"
  {-# INLINE fromBEncode #-}

{-
instance BEncode a => BEncode (Map BKey a) where
  {-# SPECIALIZE instance BEncode (Map BKey BValue) #-}
  toBEncode = BDict .  -- BD.map toBEncode
  {-# INLINE toBEncode #-}

  fromBEncode (BDict d) = traverse fromBEncode d
  fromBEncode _         = decodingError "dictionary"
  {-# INLINE fromBEncode #-}

instance (Eq a, BEncode a) => BEncode (Set a) where
  {-# SPECIALIZE instance BEncode (Set BValue)  #-}
  toBEncode = BList . map toBEncode . S.toAscList
  {-# INLINE toBEncode #-}

  fromBEncode (BList xs) = S.fromAscList <$> traverse fromBEncode xs
  fromBEncode _          = decodingError "Data.Set"
  {-# INLINE fromBEncode #-}
-}
instance BEncode Version where
  toBEncode = toBEncode . BC.pack . showVersion
  {-# INLINE toBEncode #-}

  fromBEncode (BString bs)
    | [(v, _)] <- ReadP.readP_to_S parseVersion (BC.unpack bs)
    = return v
  fromBEncode _ = decodingError "Data.Version"
  {-# INLINE fromBEncode #-}

{--------------------------------------------------------------------
--  Tuple instances
--------------------------------------------------------------------}

instance BEncode () where
  toBEncode () = BList []
  {-# INLINE toBEncode #-}

  fromBEncode (BList []) = Right ()
  fromBEncode _          = decodingError "Unable to decode unit value"
  {-# INLINE fromBEncode #-}

instance (BEncode a, BEncode b) => BEncode (a, b) where
  {-# SPECIALIZE instance (BEncode b) => BEncode (BValue, b) #-}
  {-# SPECIALIZE instance (BEncode a) => BEncode (a, BValue) #-}
  {-# SPECIALIZE instance BEncode (BValue, BValue) #-}
  toBEncode (a, b) = BList [toBEncode a, toBEncode b]
  {-# INLINE toBEncode #-}

  fromBEncode (BList [a, b]) = (,) <$> fromBEncode a <*> fromBEncode b
  fromBEncode _              = decodingError "Unable to decode a pair."
  {-# INLINE fromBEncode #-}

instance (BEncode a, BEncode b, BEncode c) => BEncode (a, b, c) where
  toBEncode (a, b, c) = BList [toBEncode a, toBEncode b, toBEncode c]
  {-# INLINE toBEncode #-}

  fromBEncode (BList [a, b, c]) =
    (,,) <$> fromBEncode a <*> fromBEncode b <*> fromBEncode c
  fromBEncode _ = decodingError "Unable to decode a triple"
  {-# INLINE fromBEncode #-}

instance (BEncode a, BEncode b, BEncode c, BEncode d)
         => BEncode (a, b, c, d) where
  toBEncode (a, b, c, d) = BList [ toBEncode a, toBEncode b
                                 , toBEncode c, toBEncode d
                                 ]
  {-# INLINE toBEncode #-}

  fromBEncode (BList [a, b, c, d]) =
    (,,,) <$> fromBEncode a <*> fromBEncode b
          <*> fromBEncode c <*> fromBEncode d
  fromBEncode _ = decodingError "Unable to decode a tuple4"
  {-# INLINE fromBEncode #-}

instance (BEncode a, BEncode b, BEncode c, BEncode d, BEncode e)
      =>  BEncode (a, b, c, d, e) where
  toBEncode (a, b, c, d, e) = BList [ toBEncode a, toBEncode b
                                 , toBEncode c, toBEncode d
                                 , toBEncode e
                                 ]
  {-# INLINE toBEncode #-}

  fromBEncode (BList [a, b, c, d, e]) =
    (,,,,) <$> fromBEncode a <*> fromBEncode b
           <*> fromBEncode c <*> fromBEncode d <*> fromBEncode e
  fromBEncode _ = decodingError "Unable to decode a tuple5"
  {-# INLINE fromBEncode #-}

{--------------------------------------------------------------------
  Building dictionaries
--------------------------------------------------------------------}

-- | /Assoc/ used to easily build dictionaries with required and
-- optional keys. Suppose we have we following datatype we want to
-- serialize:
--
--  > data FileInfo = FileInfo
--  >   { fileLength :: Integer
--  >   , fileMD5sum :: Maybe ByteString
--  >   , filePath   :: [ByteString]
--  >   , fileTags   :: Maybe [Text]
--  >   } deriving (Show, Read, Eq)
--
-- We need to make /instance BEncodable FileInfo/, though we don't
-- want to check the both /maybes/ manually. The more declarative and
-- convenient way to define the 'toBEncode' method is to use
-- dictionary builders:
--
--  > instance BEncodable FileInfo where
--  >   toBEncode FileInfo {..} = fromAssocs
--  >     [ "length" -->  fileLength
--  >     , "md5sum" -->? fileMD5sum
--  >     , "path"   -->  filePath
--  >     , "tags"   -->? fileTags
--  >     ]
--  >     ...
--
data Assoc = Some !BKey BValue
           | None

-- TODO better name
-- | Make required key value pair.
(.=!) :: BEncode a => BKey -> a -> Assoc
(!k) .=! v = Some k (toBEncode v)
{-# INLINE (.=!) #-}

infix 6 .=!

-- | Like (.=!) but if the value is not present then the key do not
-- appear in resulting bencoded dictionary.
--
(.=?) :: BEncode a => BKey -> Maybe a -> Assoc
_ .=? Nothing = None
k .=? Just v  = Some k (toBEncode v)
{-# INLINE (.=?) #-}

infix 6 .=?

(.:) :: Assoc -> BDict -> BDict
None     .: d = d
Some k v .: d = Cons k v d
{-# INLINE (.:) #-}

infixr 5 .:

-- | Build BEncode dictionary using key -> value description.

-- | A faster version of 'fromAssocs'. Should be used only when keys
-- in builder list are sorted by ascending.
--
toDict :: BDict -> BValue
toDict = BDict
{-# INLINE toDict #-}

endDict :: BDict
endDict = Nil
{-# INLINE endDict #-}

{--------------------------------------------------------------------
--  Dictionary extraction
--------------------------------------------------------------------}

-- | Dictionary extractor are similar to dictionary builders, but play
-- the opposite role: they are used to define 'fromBEncode' method in
-- declarative style. Using the same /FileInfo/ datatype 'fromBEncode'
-- looks like:
--
--  > instance BEncodable FileInfo where
--  >   ...
--  >   fromBEncode (BDict d) =
--  >     FileInfo <$> d >--  "length"
--  >              <*> d >--? "md5sum"
--  >              <*> d >--  "path"
--  >              <*> d >--? "tags"
--  >   fromBEncode _ = decodingError "FileInfo"
--
--  The /reqKey/ is used to extract required key — if lookup is failed
--  then whole destructuring fail.
--
newtype Get a = Get { runGet :: StateT BDict Result a }
  deriving (Functor, Applicative, Alternative)

next :: Get BValue
next = Get (StateT go)
  where
    go  Nil          = throwError "no next"
    go (Cons _ v xs) = pure (v, xs)

req :: BKey -> Get BValue
req !key = Get (StateT search)
  where
    search  Nil          = Left msg
    search (Cons k v xs) =
      case compare k key of
        EQ -> Right (v, xs)
        LT -> search xs
        GT -> Left msg

    msg = "required field `" ++ BC.unpack key ++ "' not found"
{-# INLINE req #-}

opt :: BKey -> Get (Maybe BValue)
opt = optional . req
{-# INLINE opt #-}

{-# SPECIALIZE field :: Get BValue -> Get BValue #-}
field :: BEncode a => Get BValue -> Get a
field m = Get $ do
  v <- runGet m
  either throwError pure $ fromBEncode v

(<$>!) :: BEncode a => (a -> b) -> BKey -> Get b
f <$>! k = f <$> field (req k)
{-# INLINE (<$>!) #-}

(<$>?) :: BEncode a => (Maybe a -> b) -> BKey -> Get b
f <$>? k = f <$> optional (field (req k))
{-# INLINE (<$>?) #-}

(<*>!) :: BEncode a => Get (a -> b) -> BKey -> Get b
f <*>! k = f <*> field (req k)
{-# INLINE (<*>!) #-}

(<*>?) :: BEncode a => Get (Maybe a -> b) -> BKey -> Get b
f <*>? k = f <*> optional (field (req k))
{-# INLINE (<*>?) #-}

fromDict :: forall a. Typeable a => Get a -> BValue -> Result a
fromDict m (BDict d) = evalStateT (runGet m) d
fromDict _  _        = decodingError (show (typeOf inst))
  where
    inst = error "fromDict: impossible" :: a

{--------------------------------------------------------------------
  Encoding
--------------------------------------------------------------------}

-- | The same as 'decode' but returns any bencodable value.
decode :: BEncode a => ByteString -> Result a
decode = parse >=> fromBEncode

-- | The same as 'encode' but takes any bencodable value.
encode :: BEncode a => a -> Lazy.ByteString
encode = build . toBEncode
