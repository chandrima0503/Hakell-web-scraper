{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Database
Description : Handles database creation along with queries for insertion and fetching data
License     : GPL-3
-}

module Database
    ( 
        initialiseDB,
        saveFetchedGames,
        queryAllGames,
        queryAllTeams,
        querySelectedGames
    ) where

import Database.SQLite.Simple
import Types


-- | Initialises the SQLite DB and creates table Games and Teams if not exists
initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "NBA.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS teams (\
            \id INTEGER PRIMARY KEY,\
            \abbreviation VARCHAR(20),\
            \city VARCHAR(50),\
            \conference VARCHAR(50),\
            \division VARCHAR(50),\
            \full_name VARCHAR(50),\
            \name VARCHAR(50)\
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS games (\
            \id INTEGER PRIMARY KEY,\
            \date VARCHAR(40) NOT NULL, \
            \home_team_score INTEGER,\
            \visitor_team_score INTEGER,\
            \period INTEGER,\
            \season INTEGER,\
            \status VARCHAR(50),\
            \home_team_fk INT,\
            \visitor_team_fk INT,\
            \UNIQUE(home_team_fk,visitor_team_fk,date)\
            \)"
        return conn


-- | Gets the team data if already present or creates and insert the given team
getOrCreateTeam :: Connection -> TeamForParse -> IO ()
getOrCreateTeam conn team = do
    let team_data = Team {
        team_id_ = _id team,
        abbreviation_ = abbreviation team,
        city_ = city team,
        conference_ = conference team,
        division_ = division team,
        full_name_ = full_name team,
        name_ = name team
    }
    execute conn "INSERT OR REPLACE INTO teams VALUES (?, ?, ?, ?, ?, ?, ?)" (team_data)


-- | Creates and insert a game entry in DB 
createGame :: Connection -> GameForParse -> IO ()
createGame conn game = do
    let home_team_data = home_team game
    let visitor_team_data = visitor_team game
    home_team_created <- getOrCreateTeam conn home_team_data
    visitor_team_created <- getOrCreateTeam conn visitor_team_data
    let game_data = Game {
        date_ = date game,
        home_team_score_ = home_team_score game,
        visitor_team_score_ = visitor_team_score game,
        period_ = period game,
        season_ = season game,
        status_ = status game,
        home_team_fk_ = _id home_team_data,
        visitor_team_fk_ = _id visitor_team_data
    }

    execute conn "INSERT OR REPLACE INTO games VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?)" (
        date_ game_data,
        home_team_score_ game_data,
        visitor_team_score_ game_data,
        period_ game_data,
        season_ game_data,
        status_ game_data,
        home_team_fk_ game_data,
        visitor_team_fk_ game_data)


-- | Gets all the games
queryAllGames :: Connection -> String -> IO [Game]
queryAllGames conn season = do
    results <- queryNamed conn "SELECT date,home_team_score,visitor_team_score,period,season,status,home_team_fk,visitor_team_fk FROM games WHERE season = :season" [":season" := (season)]   
    if length results == 0 then do
        putStrLn "No games found! Try downloading the data for the particular season. Returning to menu..."
        return results
    else
        return results


-- | Gets all the teams
queryAllTeams :: Connection -> IO [Team]
queryAllTeams conn = do
    results <- query_ conn "SELECT * FROM teams"
    if length results == 0 then do
        putStrLn "No teams found! Try downloading the data for any season. Returning to menu..."
        return results
    else
        return results


-- | Saves games
saveFetchedGames :: Connection -> GameForParseArr -> IO ()
saveFetchedGames conn parsedGames = do
    let list_of_games = games parsedGames
    if length list_of_games > 1 then do
        let game_to_save:other_games = list_of_games
        createGame conn game_to_save
        saveFetchedGames conn (GameForParseArr other_games)
    else do
        let game_to_save = head list_of_games
        createGame conn game_to_save
        print ("Saved all games to DB.")

-- | Fetches fullname and name for teams whose matches id of home teams in games
querySelectedGames :: Connection -> String -> IO [SpecificGame]
querySelectedGames conn team_num = do
    query conn "SELECT full_name, name from teams inner join games on teams.team_id_ == games.home_team_fk where home_team_fk = ?" (Only (team_num :: String))
