{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Creates interactive CLI for various tasks.
License     : GPL-3
-}

module Main (main) where

import Text.Show.Unicode
import Main.Utf8(withUtf8)
import System.IO
import Database
import Types
import Fetch
import Parse
import WebAPI

import Database.SQLite.Simple

-- | Download, Parse and Save data to DB from NBA games API
downloadNBAData :: Connection -> String -> IO ()
downloadNBAData conn season = do
    print "fetching.."
    -- url for getting NBA games from API
    jsonified <- getData $ "https://balldontlie.io/api/v1/games?seasons%5B%5D=" ++ season
    case (parseGame jsonified) of
                Left err -> print err
                Right recs -> do
                    print "Parsing Done"
                    print "Saving Data to DB..."
                    saveFetchedGames conn recs
                    print "Writing data to file"
                    writeGameToFile recs
                    print "Created NBA.json successfully"

-- | Fetches from DB and prints all the games
showAllGames :: Connection -> String -> IO ()
showAllGames conn season = do
    __game <- queryAllGames conn season
    mapM_ uprint __game

-- | Fetches from DB and prints all the teams
showAllTeams :: Connection  -> IO ()
showAllTeams conn = do
    __game <- queryAllTeams conn
    mapM_ uprint __game

-- | Main function
main :: IO ()
main = do
    putStrLn "------------------------------------------------------------------"
    putStrLn "  Welcome to the NBA App                                          "
    putStrLn "  [1] Download or refresh all Games and Teams for a season        "
    putStrLn "  [2] View all saved Games for a season                           "
    putStrLn "  [3] View all saved Teams                                        "
    putStrLn "  [4] Start API Server                                            "
    putStrLn "  [5] Exit                                                        "
    putStrLn "------------------------------------------------------------------"
    hSetBuffering stdout NoBuffering
    putStr "Choose an option > "
    option <- readLn :: IO Int


    -- Initialises DB and creates Tables If not exists already
    conn <- initialiseDB
 
    -- Handles the option selected by the User
    case option of
        1 -> do
            putStrLn ("Which season? [e.g. 2022] ")
            season <- getLine
            downloadNBAData conn season
            main
        2 -> do
            putStrLn ("Which season? [e.g. 2022] ")
            season <- getLine
            showAllGames conn season
            main
        3 -> do
            showAllTeams conn
            main
        4 -> do
            startAPI
            main
        5-> putStrLn "Thank you for using our NBA App."
        otherwise -> print "You've chosen an invalid option!"