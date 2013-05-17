{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import System.Environment
import Control.Applicative
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Attoparsec
import Crypto.Hash
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as A

main :: IO ()
main = do
    filePath <- unwords <$> getArgs
    numbers <- runResourceT $ sourceFile filePath $$ sinkParser parser
    let gcds = uncurry igcd <$> numbers
        concatenatedGcds = B.pack $ concatMap show gcds
        flag = hash concatenatedGcds :: Digest SHA1
    print flag

igcd :: (Integral a) => a -> a -> a
igcd a b
  | b == 0    = a
  | otherwise = gcd b $ a `mod` b

parser :: A.Parser [(Integer, Integer)]
parser = ((,) <$> A.decimal <* A.string " : " <*> A.decimal) `A.sepBy` A.char '\n'
