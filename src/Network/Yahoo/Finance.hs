{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Yahoo.Finance
-- Copyright   :  2015 Owain Lewis
-- License     :  public domain
--
-- Maintainer  :  owain@owainlewis.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module for extracting stock price information from Yahoo Finance
--
-----------------------------------------------------------------------------
module Network.Yahoo.Finance
  ( getStockQuote
  , StockQuote(..)
  , toDouble
  ) where

import           Control.Applicative           ((<$>), (<*>))
import           Control.Lens
import           Control.Monad                 (mzero)
import           Data.Aeson
import           Data.ByteString.Lazy          as LBS hiding (intercalate, map)
import           Data.List                     (intercalate)
import qualified Data.Map                      as M
import           Data.Monoid                   (Monoid, mconcat, (<>))
import           Data.String                   (IsString)
import qualified Data.Text                     as T
import           Network.Wreq
import           Network.Yahoo.HistoricalQuote
import           Network.Yahoo.Interpolate     (interpolate)
import           Network.Yahoo.StockQuote

toDouble :: String -> Double
toDouble s = read s :: Double

newtype StockQuoteList = StockQuoteList { stocks :: [StockQuote] } deriving ( Show )

-- | Polymorphic type for Yahoo finance repsonses
--
data YahooResponse a = YahooResponse {
    quote :: a
} deriving ( Show )

instance (FromJSON a) => FromJSON (YahooResponse a) where
    parseJSON (Object o) =
        YahooResponse <$> ((o .: "query") >>= (.: "results") >>= (.: "quote"))
    parseJSON _ = mzero

-- | Runs a YQL query
runRequest ::
  T.Text ->
  IO ByteString
runRequest yql = do
  let yahoo = "https://query.yahooapis.com/v1/public/yql"
      datatable = "store://datatables.org/alltableswithkeys"
      opts = defaults & param "q"      .~ [yql]
                      & param "env"    .~ [datatable]
                      & param "format" .~ ["json"]
  r <- getWith opts yahoo
  return $ r ^. responseBody

run :: String -> IO (Maybe (YahooResponse StockQuote))
run query = decode <$> runRequest (T.pack query)

runMany :: String -> IO (Maybe [(YahooResponse StockQuote)])
runMany query = decode <$> runRequest (T.pack query)

quoteString :: (Monoid m, IsString m) => m -> m
quoteString s = "\"" <> s <> "\""

buildStockQuery :: [String] -> String
buildStockQuery xs = mconcat ["(", stocks, ")"]
    where stocks = (intercalate ", ") . (map quoteString) $ xs

-- | Generates a YQL query for 1 or more stock symbols
--
generateYQLQuery :: [String] -> String
generateYQLQuery stocks =
    "select * from yahoo.finance.quote where symbol in " <> buildStockQuery stocks

-- | Fetch a stock quote from Yahoo Finance
--
-- >> getStockQuote (T.pack "GOOGL")
--
getStockQuote :: String -> IO (Maybe StockQuote)
getStockQuote symbol =
    fmap quote <$> (run $ generateYQLQuery [symbol])

-- | Historical data
--
-- Generates a query to retrieve all historical data for 1 or many stocks
-- Dates for YQL in format YYYY-MM-DD
historicalYQLQuery ::
    String ->
    String ->
    String ->
    String
historicalYQLQuery stock startDate endDate =
    interpolate query [("x", quoteString stock), ("y", quoteString startDate), ("z", quoteString endDate)]
    where
          query = mconcat [ "select * from yahoo.finance.historicaldata where "
                          , "symbol=#{x} and startDate=#{y} and endDate=#{z}"
                          ]

-- | Get historical prices for one or many stocks
--
--   Dates should be in the form YYYY-MM-DD
--
historicalPrices ::
     String ->     -- ^ a stock to fetch
     String ->     -- ^ start date in the form YYYY-MM-DD
     String ->     -- ^ end date in the form YYYY-MM-DD
     IO ByteString -- ^ response body
historicalPrices stock startDate endDate =
    runRequest . T.pack $ historicalYQLQuery stock startDate endDate

-- | Fetch historical price information for a stock
--
historical :: String -> String -> String -> IO (Maybe (YahooResponse [HistoricalQuote]))
historical symbol startDate endDate =
    decode <$> historicalPrices symbol startDate endDate
