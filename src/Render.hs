module Render
    ( renderPage
    , Page(..)
    ) where

import           Protolude

import           Data.String                 (fromString)
import           Data.Text                   (lines, split, strip, toLower,
                                              words)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze

import           Common
import           Fish
import           Liquipedia

isPlayer :: Text -> LiquipediaEntry -> Bool
isPlayer n (LiquipediaEntry _ _ _ us) = (toLower n) `elem` (fmap toLower us)

findPlayer :: Text -> [LiquipediaEntry] -> Maybe LiquipediaEntry
findPlayer n l = head $ filter (isPlayer n) l

displayRace :: Race -> H.Html
displayRace Protoss = H.img H.! A.src "png/Picon_small_bw.png"
displayRace Terran  = H.img H.! A.src "png/Ticon_small_bw.png"
displayRace Zerg    = H.img H.! A.src "png/Zicon_small_bw.png"

data Page = Top100 | Top250 | Top2000 | Foreigner | Clans | Stats | About
    deriving (Bounded, Enum, Eq, Show)

header :: H.Html
header = H.head $ do
            H.title "Fish Server Rankings"
            H.link H.! A.rel "stylesheet"
                   H.! A.href "https://unpkg.com/purecss@0.6.2/build/pure-min.css"
            H.link H.! A.rel "stylesheet"
                   H.! A.href "css/layouts/side-menu.css"
            H.link H.! A.rel "stylesheet"
                   H.! A.href "font-awesome-4.7.0/css/font-awesome.min.css"


renderPage :: [LiquipediaEntry] -> [LiquipediaEntry] -> [FishLadder] -> Page -> H.Html
renderPage liqui foreigner ladders page =
    H.docTypeHtml $ do
        header
        H.body $ do
            H.div H.! A.id "layout" $ do
                menu page
                H.div H.! A.id "main" $ do
                    renderContent liqui foreigner ladders page

renderTable :: Int
            -> [LiquipediaEntry]
            -> [FishLadder]
            -> ([LiquipediaEntry] -> RankEntry -> Bool)     -- ^ Should this column be rendered?
            -> H.Html
renderTable n l f p = do
    let (Just (t, a)) = nth 0 f
        (Just (_, b)) = nth 1 f
    H.table H.! A.class_ "pure-table pure-table-striped" $ do
            H.thead $
                H.tr $ do
                    H.th $ "Pos"
                    H.th $ "Rank"
                    H.th $ "Diff"
                    H.th $ "Player"
                    H.th $ "Race"
                    H.th $ "Player ID"
                    H.th $ "Wins"
                    H.th $ "Losses"
                    H.th $ "Points"
            H.tbody $
                sequence_ $ fmap (renderColumn l b) (filter (p l) (take n a))
    H.p $ "Ladder data from https://www.fishserver.net, (C) Rank system by fish system development team"
    H.p $ "Nick data from http://wiki.teamliquid.net/starcraft/Fish_Server CC-BY-SA"

renderContent :: [LiquipediaEntry] -> [LiquipediaEntry] -> [FishLadder] -> Page -> H.Html
renderContent l _ f Top100 = do
    let (Just (t, a)) = nth 0 f
        (Just (_, b)) = nth 1 f
    H.div H.! A.class_ "header" $ do
        H.h1 "Starcraft Broodwar Ranking"
        H.h2 (H.toHtml $ "Last updated on " ++ show t)
    H.div H.! A.class_ "content" $ do
        renderTable 100 l f (\_ _ -> True)

renderContent l _ f Top250 = do
    let (Just (t, a)) = nth 0 f
        (Just (_, b)) = nth 1 f
    H.div H.! A.class_ "header" $ do
        H.h1 "Starcraft Broodwar Ranking"
        H.h2 (H.toHtml $ "Last updated on " ++ show t)
    H.div H.! A.class_ "content" $ do
        renderTable 250 l f (\_ _ -> True)

renderContent l _ f Top2000 = do
    let (Just (t, a)) = nth 0 f
        (Just (_, b)) = nth 1 f
    H.div H.! A.class_ "header" $ do
        H.h1 "Starcraft Broodwar Ranking"
        H.h2 (H.toHtml $ "Last updated on " ++ show t)
    H.div H.! A.class_ "content" $ do
        renderTable 2000 l f (\_ _ -> True)

renderContent _ l f Foreigner = do
    let (Just (t, a)) = nth 0 f
        (Just (_, b)) = nth 1 f
    H.div H.! A.class_ "header" $ do
        H.h1 "Starcraft Broodwar Ranking"
        H.h2 (H.toHtml $ "Last updated on " ++ show t)
    H.div H.! A.class_ "content" $ do
        renderTable 2000 l f (\x r -> isJust $ findPlayer (nick r) x)


renderContent _ _ _ _ = "TODO"


ranking :: Int -> H.Html
ranking x
    | x <= 800  = H.td H.! A.class_ "centered ranking f-rank" $ "F-"
    | x <= 1099 = H.td H.! A.class_ "centered ranking f-rank" $ "F"
    | x <= 1399 = H.td H.! A.class_ "centered ranking e-rank" $ "E"
    | x <= 1699 = H.td H.! A.class_ "centered ranking d-rank" $ "D"
    | x <= 1999 = H.td H.! A.class_ "centered ranking c-rank" $ "C"
    | x <= 2299 = H.td H.! A.class_ "centered ranking b-rank" $ "B"
    | x <= 2599 = H.td H.! A.class_ "centered ranking a-rank" $ "A"
    | otherwise = H.td H.! A.class_ "centered ranking s-rank" $ "S"

findRankEntry :: Text -> [RankEntry] -> Maybe RankEntry
findRankEntry id rks = head (filter (\x -> nick x == id) rks)

displayChange :: Int -> H.Html
displayChange x
    | x < 0 = H.td H.! A.class_ "centered decrease" $ H.i H.! A.class_ "fa fa-arrow-down" $ H.toHtml ("  (" ++ show x ++ ")")
    | x > 0 = H.td H.! A.class_ "centered increase" $ H.i H.! A.class_ "fa fa-arrow-up" $ H.toHtml ("  (+" ++ show x ++ ")")
    | x == 0 = H.td H.! A.class_ "centered steady"  $ H.i H.! A.class_ "fa fa-arrow-left" $ ""

renderColumn :: [LiquipediaEntry] -> [RankEntry] -> RankEntry -> H.Html
renderColumn l old_ranks (RankEntry r id wins losses points) = do
    H.tr $ do
        H.td H.! A.class_ "centered" $ (H.toHtml r)
        ranking points
        case findRankEntry id old_ranks of
            (Just (RankEntry oldr _ _ _ _)) ->
                displayChange (oldr - r)
            Nothing ->
                H.td $ "-"
        case findPlayer id l of
            Just (LiquipediaEntry race n p _) -> do
                H.td $
                    H.a H.! A.href (fromString $ toS p) $
                        H.toHtml n
                H.td H.! A.class_ "centered" $ (displayRace race)
            Nothing -> do
                H.td "Unknown"
                H.td H.! A.class_ "centered" $ "?"
        H.td $ (H.toHtml id)
        H.td H.! A.class_ "centered" $ (H.toHtml ((show wins) :: Text))
        H.td H.! A.class_ "centered" $ (H.toHtml ((show losses) :: Text))
        H.td H.! A.class_ "centered" $ (H.toHtml ((show points) :: Text))

menuAtt :: Page -> Page -> H.AttributeValue
menuAtt x y
    | x == y = "pure-menu-item pure-menu-selected"
    | otherwise = "pure-menu-item"

menu :: Page -> H.Html
menu page = do
    let pages = [minBound .. maxBound]
        item p = H.ul H.! A.class_ "pure-menu-list" $ do
                    H.li H.! A.class_ (menuAtt p page) $
                        H.a H.! A.class_ "pure-menu-link"
                            H.! A.href (H.toValue $ show p ++ ".html") $
                            H.toHtml ((show p) :: Text)
    H.div H.! A.id "menu" $
        H.div H.! A.class_ "pure-menu" $ do
                H.a H.! A.class_ "pure-menu-heading"
                    H.! A.href "index.html" $
                        "BWladder"
                sequence_ (fmap item pages)
