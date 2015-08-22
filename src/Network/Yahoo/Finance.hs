{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Yahoo.Finance
  ( getStockQuote
  , StockQuote(..)
  , nasdaq
  , sp500
  , ftse
  , aim
  ) where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Lens
import           Control.Monad        (mzero)
import           Data.Aeson
import           Data.ByteString.Lazy as LBS hiding (intercalate, map)
import           Data.List            (intercalate)
import qualified Data.Map             as M
import           Data.Monoid          (Monoid, mconcat, (<>))
import           Data.String          (IsString)
import qualified Data.Text            as T
import           Network.Wreq

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

quoteString :: (Monoid m, IsString m) => m -> m
quoteString s = "\"" <> s <> "\""

-- | Generates a YQL query for 1 or more stock symbols
--
generateYQLQuery :: [String] -> String
generateYQLQuery xs = "select * from yahoo.finance.quote where symbol in " <> query
    where stocks = (intercalate ", ") . (map quoteString) $ xs
          query = mconcat ["(", stocks, ")"]

-- | Fetch a stock quote from Yahoo Finance eg. getStockQuote (T.pack "GOOGL")
getStockQuote :: String -> IO (Maybe StockQuote)
getStockQuote symbol = fmap quote <$> (run $ generateYQLQuery [symbol])

-- Indexes

nasdaq :: IO (Maybe StockQuote)
nasdaq = getStockQuote "^NDX"

sp500 :: IO (Maybe StockQuote)
sp500 = getStockQuote "^GSPC"

ftse :: IO (Maybe StockQuote)
ftse = getStockQuote "^FTSE"

aim :: IO (Maybe StockQuote)
aim = getStockQuote "^FTAI"
