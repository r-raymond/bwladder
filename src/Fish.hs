module Fish
    ( RankEntry(..)
    , parseFish
    ) where

import           Protolude

import           Data.Text (lines, split, strip, toLower, words)

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

