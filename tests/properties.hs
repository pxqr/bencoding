{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS  -fno-warn-unused-binds #-}
module Main (main) where

import Control.Applicative
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.List as L
import GHC.Generics
import Test.QuickCheck
import Test.Hspec

import Data.BEncode
import qualified Data.BEncode.BDict as BE


instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary

instance Arbitrary a => Arbitrary (BE.BDictMap a) where
  arbitrary = frequency
    [ (90, pure BE.Nil)
    , (10, BE.Cons <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

instance Arbitrary BValue where
  arbitrary = frequency
    [ (30, BInteger <$> arbitrary)
    , (30, BString  <$> arbitrary)
    , (20, BList    <$> (arbitrary `suchThat` ((10 >) . L.length)))
    , (20, BDict    <$> arbitrary)
    ]

data List a = Cons a (List a) | Nil
              deriving (Show, Eq, Generic)

instance BEncode a => BEncode (List a)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency
    [ (90, pure Nil)
    , (10, Cons <$> arbitrary <*> arbitrary)
    ]

data FileInfo = FileInfo
  { fiLength :: !Integer
  , fiPath   :: [BS.ByteString]
  , fiMD5Sum :: BS.ByteString
  } deriving (Show, Eq, Generic)

instance BEncode FileInfo

instance Arbitrary FileInfo where
  arbitrary = FileInfo <$> arbitrary <*> arbitrary <*> arbitrary

data T a = T

prop_bencodable :: Eq a => BEncode a => T a -> a -> Bool
prop_bencodable _ x = decode (BL.toStrict (encode x)) == Right x

main :: IO ()
main = hspec $ do
  describe "BValue" $ do
    it "properly encoded" $ property $
       prop_bencodable (T :: T BValue)

  describe "BEncode" $ do
    it "generic recordless" $ property $
      prop_bencodable (T :: T (List Int))

    it "generic records" $ property $
      prop_bencodable (T :: T FileInfo)
