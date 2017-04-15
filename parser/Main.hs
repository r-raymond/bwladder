module Main where

import           Protolude

import           Data.Text                (lines, split, strip, words, toLower)
import           Data.Text.IO             (hGetContents)
import           System.IO                (hSetEncoding, utf8)

import Data.String (fromString)
import qualified Text.Blaze.Html5         as H
import qualified Text.Blaze.Html5.Attributes         as A
import           Text.Blaze.Renderer.Utf8 (renderMarkup)
import           Text.HTML.DOM
import           Text.XML.Cursor

import Common
import Fish
import Liquipedia
import Render

main :: IO ()
main = do
    [file, f2] <- getArgs

    con <- readUtf8 file

    let (Just p) = parseFish con

    li <- readUtf8 f2

    let fpi = parseLiquipedia li

    writeFile "test.txt" (show fpi)

    putStrLn $ renderMarkup $ renderPage fpi p

    return ()
