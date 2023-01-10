{-|
Module: Fetch
Description: GET data (JSON) from a given URL
License     : GPL-3
-}

module Fetch (
    getData
) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Network.HTTP.Simple

getData :: String -> IO BS.ByteString
getData url =
    do
        request <- parseRequest url
        response <- httpLbs request
        return $ getResponseBody response