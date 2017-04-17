module Fish
    ( RankEntry(..)
    , FishLadder
    , parseFishLadder
    ) where

import           Protolude

import           Data.Text        (lines, split, strip, toLower, words)
import           Data.Time.Clock
import           System.Directory
import           Data.Time.Format

import Common

type FishLadder = (UTCTime, [RankEntry])

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

parseFishLadder :: IO [FishLadder]
parseFishLadder = do
    files <- listDirectory "fish"
    let go f = do
            t <- parseTimeM True defaultTimeLocale rfc822DateFormat f :: IO UTCTime
            c <- readUtf8 ("fish/" ++ f)
            let (Just p) = parseFish c
            return (t, p)
    sequence $ fmap go files

