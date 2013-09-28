{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS  -fno-warn-unused-binds #-}
module Main (main) where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import GHC.Generics

import Data.BEncode


instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary

instance Arbitrary BValue where
    arbitrary = frequency
                [ (50, BInteger <$> arbitrary)
                , (40, BString  <$> arbitrary)
                , (5,  BList    <$> (arbitrary `suchThat` ((10 >) . length)))
                ]


prop_EncDec :: BValue -> Bool
prop_EncDec x = case decode (L.toStrict (encode x)) of
                  Left _   -> False
                  Right x' -> x == x'

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
  , fiPath   :: [B.ByteString]
  , fiMD5Sum :: B.ByteString
  } deriving (Show, Eq, Generic)

instance BEncode FileInfo

instance Arbitrary FileInfo where
  arbitrary = FileInfo <$> arbitrary <*> arbitrary <*> arbitrary

data T a = T

prop_bencodable :: Eq a => BEncode a => T a -> a -> Bool
prop_bencodable _ x = decoded (L.toStrict (encoded x)) == Right x

-- All tests are (encode >>> decode = id)
main :: IO ()
main = defaultMain
       [ testProperty "BEncode" prop_EncDec

       , testProperty "generic recordless" $
            prop_bencodable (T :: T (List Int))

       , testProperty "generic records" $
            prop_bencodable (T :: T FileInfo)
       ]