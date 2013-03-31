module Main (main) where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

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

main :: IO ()
main = defaultMain
       [ testProperty "encode <-> decode" prop_EncDec
       ]