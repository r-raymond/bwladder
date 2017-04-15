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
import           Text.XML                 (Node (..))
import           Text.XML.Cursor

data RankEntry
    = RankEntry
    { rank   :: Int
    , nick   :: Text
    , wins   :: Int
    , losses :: Int
    , points :: Int
    } deriving (Show)

parse :: [Text] -> Maybe RankEntry
parse [r, _, n, w, l, p] = do
    ri <- readMaybe $ toS r
    wi <- readMaybe $ toS w
    li <- readMaybe $ toS l
    pi <- readMaybe $ toS p
    return $ RankEntry ri n wi li pi
parse _ = Nothing

parseFish :: Text -> Maybe [RankEntry]
parseFish t = sequence $ fmap parse (fmap words (lines t))

readUtf8 :: [Char] -> IO Text
readUtf8 s = do
    h <- openFile s ReadMode
    hSetEncoding h utf8
    hGetContents h

data Race = Protoss | Terran | Zerg
    deriving (Show, Read)

data LiquipediaEntry
    = LiquipediaEntry
    { race      :: Race
    , player    :: Text
    , profile   :: Text
    , usernames :: [Text]
    } deriving Show

parseLiquipedia :: Text -> Maybe [LiquipediaEntry]
parseLiquipedia t = do
    undefined

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

processUserNames :: Text -> [Cursor] -> Maybe [Text]
processUserNames name curs = do
    let cont = concat $ fmap (\x -> x $// content) curs
        usernames = filter (\x -> (x /= "")
                               && (x /= "\n")
                               && (x /= name)) cont
    user <- head usernames
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
    usernames <- processUserNames name curs
    race <- processRace curs
    link <- processLink curs

    return $ LiquipediaEntry race name link usernames

isPlayer :: Text -> LiquipediaEntry -> Bool
isPlayer n (LiquipediaEntry _ _ _ us) = (toLower n) `elem` (fmap toLower us)

findPlayer :: Text -> [LiquipediaEntry] -> Maybe LiquipediaEntry
findPlayer n l = head $ filter (isPlayer n) l

renderColumn :: [LiquipediaEntry] -> RankEntry -> H.Html
renderColumn l (RankEntry r id wins losses points) = do
    H.tr $ do
        H.td $ (H.toHtml r)
        case findPlayer id l of
            Just (LiquipediaEntry race n p _) -> do
                H.td $
                    H.a H.! A.href (fromString $ toS p) $
                        H.toHtml n
                H.td (H.toHtml $ ((show race) :: Text))
            Nothing -> do
                H.td "Unknown"
                H.td "Unknown"
        H.td $ (H.toHtml id)
        H.td $ (H.toHtml ((show wins) :: Text))
        H.td $ (H.toHtml ((show losses) :: Text))
        H.td $ (H.toHtml ((show points) :: Text))

renderPage :: [LiquipediaEntry] -> [RankEntry] -> H.Html
renderPage l r = do
    H.head $ do
        H.title "Fish Server Rankings"
        H.link H.! A.rel "stylesheet"
               H.! A.href "https://unpkg.com/purecss@0.6.2/build/pure-min.css"
    H.body $ do
        H.div H.! A.class_ "pure-g" $ do
            H.div H.! A.class_ "pure-u-1-1" $ do
                H.table H.! A.class_ "pure-table pure-table-striped" $ do
                    H.thead $
                        H.tr $ do
                            H.th $ "Rank"
                            H.th $ "Player"
                            H.th $ "Race"
                            H.th $ "Player ID"
                            H.th $ "Wins"
                            H.th $ "Losses"
                            H.th $ "Points"
                    H.tbody $
                        sequence_ $ fmap (renderColumn l) r


main :: IO ()
main = do
    [file, f2] <- getArgs

    con <- readUtf8 file

    let (Just p) = parseFish con

    li <- readUtf8 f2
    let doc = parseLT $ toS li
        cur = fromDocument doc
        table = cur $// findRows
        rows = fmap (\x -> x $// findRowInfo) table
        pi = fmap processInfo rows
        fpi = concat $ (fmap toList pi)


    putStrLn $ renderMarkup $ renderPage fpi p

    return ()

