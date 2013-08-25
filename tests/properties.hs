{-# LANGUAGE DeriveGeneric #-}
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

instance Arbitrary BEncode where
    arbitrary = frequency
                [ (50, BInteger <$> arbitrary)
                , (40, BString  <$> arbitrary)
                , (5,  BList    <$> (arbitrary `suchThat` ((10 >) . length)))
                ]


prop_EncDec :: BEncode -> Bool
prop_EncDec x = case decode (L.toStrict (encode x)) of
                  Left _   -> False
                  Right x' -> x == x'

data List a = Cons a (List a) | Nil
              deriving (Show, Eq, Generic)

instance BEncodable a => BEncodable (List a)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency
    [ (90, pure Nil)
    , (10, Cons <$> arbitrary <*> arbitrary)
    ]

data T a = T

prop_bencodable :: Eq a => BEncodable a => T a -> a -> Bool
prop_bencodable _ x = decoded (L.toStrict (encoded x)) == Right x

main :: IO ()
main = defaultMain
       [ testProperty "encode >>> decode = id" prop_EncDec
       , testProperty "generic encode >>> decode = id" $
            prop_bencodable (T :: T (List Int))
       ]