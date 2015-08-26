{-# LANGUAGE OverloadedStrings #-}
module Network.Yahoo.StockQuote where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (mzero)
import           Data.Aeson

data StockQuote = StockQuote {
    name     :: String
  , symbol   :: String
  , price    :: String
  , change   :: String
  , dayHigh  :: String
  , dayLow   :: String
  , yearHigh :: String
  , yearLow  :: String
} deriving ( Show, Ord, Eq )

instance FromJSON StockQuote where
    parseJSON (Object o) =
        StockQuote <$> o .: "Name"
                   <*> o .: "Symbol"
                   <*> o .: "LastTradePriceOnly"
                   <*> o .: "Change"
                   <*> o .: "DaysHigh"
                   <*> o .: "DaysLow"
                   <*> o .: "YearHigh"
                   <*> o .: "YearLow"
    parseJSON _ = mzero
