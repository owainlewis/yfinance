{-# LANGUAGE OverloadedStrings #-}
module Network.Yahoo.HistoricalQuote where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (mzero)
import           Data.Aeson

data HistoricalQuote = HistoricalQuote {
    symbol        :: String
  , date          :: String
  , open          :: String
  , high          :: String
  , low           :: String
  , close         :: String
  , volume        :: String
  , adjustedClose :: String
} deriving ( Show )

instance FromJSON HistoricalQuote where
    parseJSON (Object o) =
        HistoricalQuote <$> o .: "Symbol"
                        <*> o .: "Date"
                        <*> o .: "Open"
                        <*> o .: "High"
                        <*> o .: "Low"
                        <*> o .: "Close"
                        <*> o .: "Volume"
                        <*> o .: "Adj_Close"
    parseJSON _ = mzero
