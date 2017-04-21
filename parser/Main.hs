module Main where

import           Protolude

import qualified Data.Text                   as T
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

    putStrLn ("Newest from " ++ (show (fst freshest)))

    liquipedia <- fmap toS $ simpleHTTP (getRequest "http://wiki.teamliquid.net/starcraft/Fish_Server")
                                     >>= getResponseBody

    let all = parseLiquipedia liquipedia
        (_, tempP) = T.breakOn "List of Foreigner Usernames on Fish" liquipedia
        (_, lastP) = T.breakOn "List of Foreigner Usernames on Fish" (T.drop 1 tempP)
        foreigner = parseLiquipedia lastP

    putStrLn ("Parsed " ++ (show (length all)) ++ " liquipedia columns, " ++ (show $ length foreigner) ++ " foreigners")

    let pages = [minBound .. maxBound] :: [Page]
        pag = \p -> do
                writeFile ("docs/" ++ show p ++ ".html") (toS $ renderMarkup $ renderPage all foreigner sortedFish p)
                putStrLn ("Wrote website to docs/" ++ (show p))

    sequence_ $ fmap pag pages
