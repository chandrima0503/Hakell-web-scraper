{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Types
Description : Contains the data types used for this project
License     : GPL-3
-}


module Types (
    Game (..),
    Team (..),
    GameForParseArr (..),
    TeamForParse (..),
    GameForParse (..),
    SpecificGame (..)
) where

import GHC.Generics
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

data Game = Game {
    date_ :: String,
    home_team_score_ :: Int,
    visitor_team_score_ :: Int,
    period_ :: Int,
    season_ :: Int,
    status_ :: String,
    home_team_fk_ :: Int,
    visitor_team_fk_ :: Int
} deriving (Show, Generic)

data Team = Team {
    team_id_ :: Int,
    abbreviation_ :: String,
    city_ :: String,
    conference_ :: String,
    division_ :: String,
    full_name_ :: String,
    name_ :: String
} deriving (Show, Generic)

data TeamForParse = TeamForParse {
    _id :: Int,
    abbreviation :: String,
    city :: String,
    conference :: String,
    division :: String,
    full_name :: String,
    name :: String
} deriving (Show, Generic)

data GameForParse = GameForParse {
    date :: String,
    period :: Int,
    season :: Int,
    postseason :: Bool,
    status :: String,
    time :: String,
    home_team :: TeamForParse,
    visitor_team :: TeamForParse,
    home_team_score :: Int,
    visitor_team_score :: Int
} deriving (Show, Generic)

data GameForParseArr = GameForParseArr {
    games :: [GameForParse]
} deriving (Show, Generic)

data SpecificGame = SpecificGame {
    full_name__ :: String,
    name__ :: String
} deriving (Show, Generic)


instance FromRow Game where
    fromRow = Game <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Game where
    toRow (Game date_ home_team_score_ visitor_team_score_ period_ season_ status_ home_team_fk_ visitor_team_fk_)
        = toRow (date_, period_, home_team_score_, visitor_team_score_, season_, status_, home_team_fk_, visitor_team_fk_)

instance FromRow Team where
    fromRow = Team <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Team where
    toRow (Team team_id_ abbreviation_ city_ conference_ division_ full_name_ name_)
        = toRow (team_id_, abbreviation_, city_, conference_, division_, full_name_, name_)

instance FromRow SpecificGame where
    fromRow = SpecificGame <$> field <*> field

instance ToRow SpecificGame where
    toRow (SpecificGame full_name__ name__)
        = toRow (full_name__, name__) 

