{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Yahoo.Finance
  ( getStockQuote
  , fetchQuote
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
import           Data.ByteString.Lazy as LBS
import qualified Data.Map             as M
import           Data.Monoid          ((<>))
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

run :: T.Text -> IO (Maybe YahooResponse)
run query = decode <$> runRequest query

stockQuote :: T.Text -> T.Text
stockQuote symbol = "select * from yahoo.finance.quote where symbol in (\"" <> symbol <> "\")"

-- | Fetch a stock quote from Yahoo Finance eg. getStockQuote (T.pack "GOOGL")
getStockQuote :: T.Text -> IO (Maybe StockQuote)
getStockQuote symbol = fmap quote <$> (run . stockQuote $ symbol)

fetchQuote :: String -> IO (Maybe StockQuote)
fetchQuote quote = getStockQuote $ T.pack quote

-- US Indexes

nasdaq :: IO (Maybe StockQuote)
nasdaq = fetchQuote "^NDX"

sp500 :: IO (Maybe StockQuote)
sp500 = fetchQuote "^GSPC"

-- EU Indexes

-- UK Indexes

-- FTSE 100 (^FTSE) -FTSE
ftse :: IO (Maybe StockQuote)
ftse = fetchQuote "^FTSE"

-- FTSE AIM Index
aim :: IO (Maybe StockQuote)
aim = fetchQuote "^FTAI"
