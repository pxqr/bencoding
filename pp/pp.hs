module Main (main) where

import Data.BEncode
import qualified Data.ByteString as B
import System.IO
import System.Environment

main :: IO ()
main = do
  path : _ <- getArgs
  content  <- B.readFile path
  case decode content of
    Left  e  -> hPutStrLn stderr e
    Right be -> print $ ppBEncode be