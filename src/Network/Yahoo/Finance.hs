{-# LANGUAGE OverloadedStrings #-}
module Network.Yahoo.Finance where

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
    name   :: String
  , symbol :: String
  , price  :: String
} deriving ( Show )

instance FromJSON StockQuote where
    parseJSON (Object o) =
        StockQuote <$> o .: "Name"
                   <*> o .: "Symbol"
                   <*> o .: "LastTradePriceOnly"
    parseJSON _ = mzero

data YahooResponse = YahooResponse {
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
run query = (\r -> return $ decode r :: IO (Maybe YahooResponse)) =<< runRequest query

stockQuote symbol = "select * from yahoo.finance.quote where symbol in (\"" <> symbol <> "\")"

getStockQuote = run . stockQuote
