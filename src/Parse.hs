{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Parse
Description : Handles parsing the JSON data to haskell data type and vice-versa
License     : GPL-3
-}


module Parse (
    parseGame,
    writeGameToFile
) where


import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as BS
import Control.Monad

renameGameFields "games" = "data"
renameGameFields other = other

renameTeamFields "_id" = "id"
renameTeamFields other = other

customGameOptions = defaultOptions {
    fieldLabelModifier = renameGameFields
}

customTeamOptions = defaultOptions {
    fieldLabelModifier = renameTeamFields
}

instance FromJSON TeamForParse where
    parseJSON = genericParseJSON customTeamOptions

instance FromJSON GameForParse

instance FromJSON SpecificGame

instance FromJSON GameForParseArr where
    parseJSON = genericParseJSON customGameOptions

instance ToJSON TeamForParse where
    toJSON = genericToJSON customTeamOptions

instance ToJSON GameForParse

instance ToJSON Game

instance ToJSON SpecificGame

instance ToJSON GameForParseArr where
    toJSON = genericToJSON customGameOptions




-- | Parses the given data to GAME data type
parseGame :: L8.ByteString -> Either String GameForParseArr
parseGame jsn = eitherDecode jsn :: Either String GameForParseArr

-- | Encodes the data and writes the GAME data to file
writeGameToFile :: ToJSON a => a -> IO ()
writeGameToFile recs = BS.writeFile "NBA.json" (encode recs)
