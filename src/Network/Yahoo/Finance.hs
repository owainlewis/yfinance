{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Yahoo.Finance
  ( getStockQuote
  , StockQuote(..)
  , toDouble
  ) where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Lens
import           Control.Monad             (mzero)
import           Data.Aeson
import           Data.ByteString.Lazy      as LBS hiding (intercalate, map)
import           Data.List                 (intercalate)
import qualified Data.Map                  as M
import           Data.Monoid               (Monoid, mconcat, (<>))
import           Data.String               (IsString)
import qualified Data.Text                 as T
import           Network.Wreq
import           Network.Yahoo.Interpolate (interpolate)

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

toDouble :: String -> Double
toDouble s = read s :: Double

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

newtype StockQuoteList = StockQuoteList {
    stocks :: [StockQuote]
} deriving ( Show )

newtype YahooResponse = YahooResponse {
    quote :: StockQuote
} deriving ( Show )

instance FromJSON YahooResponse where
    parseJSON (Object o) =
        YahooResponse <$> ((o .: "query") >>= (.: "results") >>= (.: "quote"))
    parseJSON _ = mzero

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

run :: String -> IO (Maybe YahooResponse)
run query = decode <$> runRequest (T.pack query)

runMany :: String -> IO (Maybe [YahooResponse])
runMany query = decode <$> runRequest (T.pack query)

-- TODO move to util functions ns

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

-- | Fetch a stock quote from Yahoo Finance eg. getStockQuote (T.pack "GOOGL")
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
historicalYQLQuery stocks startDate endDate =
    interpolate query [("x", normalizedStocks), ("y", quoteString startDate), ("z", quoteString endDate)]
    where normalizedStocks = stocks
          query = mconcat [ "select * from yahoo.finance.historicaldata where "
                          , "symbol #{x} and startDate = #{y} and endDate = #{z}"
                          ]

-- | Get historical prices for one or many stocks
--
--   Dates should be in the form 2009-09-11
--
historicalPrices ::
    [String] ->     -- ^ a list of stocks to fetch
     String ->     -- ^ start date in the form YYYY-MM-DD
     String ->     -- ^ end date in the form YYYY-MM-DD
     IO ByteString -- ^ response body
historicalPrices stocks startDate endDate =
    runRequest . T.pack $ historicalYQLQuery (buildStockQuery stocks) startDate endDate
