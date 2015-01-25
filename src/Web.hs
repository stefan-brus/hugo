{-# LANGUAGE OverloadedStrings #-}

-- Web commands module

module Web where

import Control.Monad
import Control.Lens

import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as B

import Network (withSocketsDo)
import Network.Wreq

import qualified Config

-----------
-- TYPES --
-----------

data Weather = Weather {
  temperature :: Double
} deriving (Show)

instance FromJSON Weather where
  parseJSON (Object o) = do
    t <- parseTemp =<< (o .: "main")
    return $ Weather { temperature = t }
    where
      parseTemp (Object o') = do
        res <- (o' .: "temp")
        return res
      parseTemp _ = mzero

  parseJSON _ = mzero

---------------------------
-- WEATHER API FUNCTIONS --
---------------------------

-- Get the weather for the given location
getWeather :: String -> IO (Either String Weather)
getWeather loc = do
  r <- getResponse (Config.weatherURL ++ loc)
  return $ eitherDecode r

----------------------
-- HELPER FUNCTIONS --
----------------------

-- Get the HTTP response for the given URL string
getResponse :: String -> IO B.ByteString
getResponse url = do
  r <- withSocketsDo $ get url
  return $ r ^. responseBody
