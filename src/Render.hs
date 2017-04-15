module Render
    ( renderPage
    ) where

import           Protolude

import           Data.String                 (fromString)
import           Data.Text                   (lines, split, strip, toLower,
                                              words)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Fish
import           Liquipedia


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

