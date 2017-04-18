module Main where

import           Protolude

import           Data.Text                   (lines, split, strip, toLower,
                                              words)
import           Data.Text.IO                (hGetContents)
import           System.IO                   (hSetEncoding, utf8)

import           Data.String                 (fromString)
import           Data.Time.Clock
import           Data.Time.Format
import           Network.HTTP
import           System.Directory
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.Utf8    (renderMarkup)
import           Text.HTML.DOM
import           Text.XML.Cursor

import           Common
import           Fish
import           Liquipedia
import           Render

main :: IO ()
main = do

    fish <- parseFishLadder

    putStrLn ("Parsed " ++ (show (length fish)) ++ " fish files")

    let sortedFish = sortBy (\(x, _) (y, _) -> compare y x) fish
        (Just freshest) = head sortedFish
        (Just secondFreshest) = nth 1 sortedFish

    putStrLn ("Newest from " ++ (show (fst freshest)))

    liquiepedia <- simpleHTTP (getRequest "http://wiki.teamliquid.net/starcraft/Fish_Server")
                   >>= getResponseBody

    let fpi = parseLiquipedia (toS liquiepedia)

    putStrLn ("Parsed " ++ (show (length fpi)) ++ " liquipedia columns")

    writeFile "docs/index.html" (toS $ renderMarkup $ renderPage fpi secondFreshest freshest)
    putStrLn ("Wrote website to docs/index.html" :: Text)

    return ()
