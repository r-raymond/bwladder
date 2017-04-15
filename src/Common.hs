module Common
    ( readUtf8
    , Race
    ) where

import           Protolude

import           Data.Text.IO (hGetContents)
import           System.IO    (hSetEncoding, utf8)


readUtf8 :: [Char] -> IO Text
readUtf8 s = do
    h <- openFile s ReadMode
    hSetEncoding h utf8
    hGetContents h

data Race = Protoss | Terran | Zerg
    deriving (Show, Read)

