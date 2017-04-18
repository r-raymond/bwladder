module Render
    ( renderPage
    ) where

import           Protolude

import           Data.String                 (fromString)
import           Data.Text                   (lines, split, strip, toLower,
                                              words)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Common
import           Fish
import           Liquipedia

isPlayer :: Text -> LiquipediaEntry -> Bool
isPlayer n (LiquipediaEntry _ _ _ us) = (toLower n) `elem` (fmap toLower us)

findPlayer :: Text -> [LiquipediaEntry] -> Maybe LiquipediaEntry
findPlayer n l = head $ filter (isPlayer n) l

displayRace :: Race -> H.Html
displayRace Protoss = "P"
displayRace Terran  = "T"
displayRace Zerg    = "Z"

ranking :: Int -> H.AttributeValue
ranking x
    | x <= 1149 = "f-rank"
    | x <= 1399 = "e-rank"
    | x <= 1699 = "d-rank"
    | x <= 2199 = "c-rank"
    | otherwise = "b-rank"

findRankEntry :: Text -> [RankEntry] -> Maybe RankEntry
findRankEntry id rks = head (filter (\x -> nick x == id) rks)

displayChange :: Int -> H.Html
displayChange x
    | x < 0 = H.td H.! A.class_ "decrease" $ H.i H.! A.class_ "fa fa-arrow-down" $ H.toHtml ("  (" ++ show x ++ ")")
    | x > 0 = H.td H.! A.class_ "increase" $ H.i H.! A.class_ "fa fa-arrow-up" $ H.toHtml ("  (+" ++ show x ++ ")")
    | x == 0 = H.td H.! A.class_ "steady"  $ H.i H.! A.class_ "fa fa-arrow-left" $ ""

renderColumn :: [LiquipediaEntry] -> [RankEntry] -> RankEntry -> H.Html
renderColumn l old_ranks (RankEntry r id wins losses points) = do
    H.tr H.! A.class_ (ranking points) $ do
        H.td $ (H.toHtml r)
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
                H.td (displayRace race)
            Nothing -> do
                H.td "Unknown"
                H.td "?"
        H.td $ (H.toHtml id)
        H.td $ (H.toHtml ((show wins) :: Text))
        H.td $ (H.toHtml ((show losses) :: Text))
        H.td $ (H.toHtml ((show points) :: Text))

renderPage :: [LiquipediaEntry] -> FishLadder -> FishLadder -> H.Html
renderPage l (_, old_ranks) (time, r) = do
    H.docTypeHtml $ do
        H.head $ do
            H.title "Fish Server Rankings"
            H.link H.! A.rel "stylesheet"
                   H.! A.href "https://unpkg.com/purecss@0.6.2/build/pure-min.css"
            H.link H.! A.rel "stylesheet"
                   H.! A.href "css/layouts/side-menu.css"
            H.link H.! A.rel "stylesheet"
                   H.! A.href "font-awesome-4.7.0/css/font-awesome.min.css"
        H.body $ do
            H.div H.! A.id "layout" $ do
                --menu
                H.div H.! A.id "main" $ do
                    H.div H.! A.class_ "header" $ do
                        H.h1 "Starcraft Broodwar Ranking"
                        H.h2 (H.toHtml $ "Last updated on " ++ show time)
                    H.div H.! A.class_ "content" $ do
                        H.table H.! A.class_ "pure-table pure-table-striped" $ do
                            H.thead $
                                H.tr $ do
                                    H.th $ "Rank"
                                    H.th $ "Diff"
                                    H.th $ "Player"
                                    H.th $ "Race"
                                    H.th $ "Player ID"
                                    H.th $ "Wins"
                                    H.th $ "Losses"
                                    H.th $ "Points"
                            H.tbody $
                                sequence_ $ fmap (renderColumn l old_ranks) r
                        H.p $ "Ladder data from https://www.fishserver.net, (C) Rank system by fish system development team"
                        H.p $ "Nick data from http://wiki.teamliquid.net/starcraft/Fish_Server CC-BY-SA"
                H.script H.! A.src "js/ui.js" $ ""

menu :: H.Html
menu = do
    H.a H.! A.href "#menu"
        H.! A.id "menuLink"
        H.! A.class_ "menu-link" $
            H.span $ ""
    H.div H.! A.id "menu" $
        H.div H.! A.class_ "pure-menu" $ do
                H.a H.! A.class_ "pure-menu-heading"
                    H.! A.href "index.html" $
                        "BWladder"
                H.ul H.! A.class_ "pure-menu-list" $ do
                    H.li H.! A.class_ "pure-menu-item" $
                         H.a H.! A.href "index.html"
                             H.! A.class_ "pure-menu-link" $
                            "Ladder"
                    H.li H.! A.class_ "pure-menu-item" $
                         H.a H.! A.href "index.html"
                             H.! A.class_ "pure-menu-link" $
                            "About"
