{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : WebAPI
Description : API using Scotty
License     : GPL-3
-}

module WebAPI
    ( 
        startAPI
    ) where


import Database
import Types
import Fetch
import Parse
import Web.Scotty
import Control.Monad.IO.Class

-- | starts scotty server in port 3000 for method 'GET' url '/games/:season'
startAPI :: IO()
startAPI = do
    putStrLn "Starting Server..."
    conn <- initialiseDB
    scotty 3000 $ do
        get "/games/:season" $ do
            season <- param "season"
            games <- liftIO $ queryAllGames conn season
            json games

