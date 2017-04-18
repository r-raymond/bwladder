module Common
    ( readUtf8
    , Race(..)
    , nth
    ) where

import           Protolude

import           Data.Text.IO (hGetContents)
import           System.IO    (hSetEncoding, utf8)

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (x:xs) = Just x
nth n (x:xs) = nth (n-1) xs

readUtf8 :: [Char] -> IO Text
readUtf8 s = do
    h <- openFile s ReadMode
    hSetEncoding h utf8
    hGetContents h

data Race = Protoss | Terran | Zerg
    deriving (Show, Read)

