module Liquipedia
    ( LiquipediaEntry(..)
    , parseLiquipedia
    ) where

import           Protolude

import           Text.HTML.DOM
import           Data.Text       (lines, split, strip, toLower, words)
import           Text.XML        (Node (..))
import           Text.XML.Cursor

import           Common

data LiquipediaEntry
    = LiquipediaEntry
    { race      :: Race
    , player    :: Text
    , profile   :: Text
    , usernames :: [Text]
    } deriving Show

nodeFilter :: Node -> Bool
nodeFilter (NodeElement _) = True
nodeFilter _               = False

findRows :: Cursor -> [Cursor]
findRows = element "table"
        >=> attributeIs "class" "sortable wikitable"
        >=> child
        >=> checkNode nodeFilter

findRowInfo :: Cursor -> [Cursor]
findRowInfo = element "td"

processUserNames :: [Cursor] -> Maybe [Text]
processUserNames curs = do
    f <- nth 2 curs
    let cont = f $// content
    user <- head cont
    return $ fmap strip (split (== ',') user)

processID :: [Cursor] -> Maybe Text
processID curs = do
    let links = concat $ fmap (\x -> x $// element "a") curs
    l <- head links

    let names = l $// content
    head names

processRace :: [Cursor] -> Maybe Race
processRace curs = do
    let img = concat $ fmap (\x -> x $// element "img" >=> attribute "alt") curs
    i <- head img
    readMaybe $ toS i

processLink :: [Cursor] -> Maybe Text
processLink curs = do
    let link = concat $ fmap (\x -> x $// element "a" >=> attribute "href") curs
    l <- head link
    return ("http://wiki.teamliquid.net" <> l)

processInfo :: [Cursor] -> Maybe LiquipediaEntry
processInfo curs = do
    name <- processID curs
    usernames <- processUserNames curs
    race <- processRace curs
    link <- processLink curs

    return $ LiquipediaEntry race name link usernames

parseLiquipedia :: Text -> [LiquipediaEntry]
parseLiquipedia t = fpi
  where
    doc = parseLT $ toS t
    cur = fromDocument doc
    table = cur $// findRows
    rows = fmap (\x -> x $// findRowInfo) table
    pi = fmap processInfo rows
    fpi = concat $ (fmap toList pi)

