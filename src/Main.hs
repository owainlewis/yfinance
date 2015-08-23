module Main where

import           Network.Yahoo.Finance

nasdaq :: IO (Maybe StockQuote)
nasdaq = getStockQuote "^NDX"

sp500 :: IO (Maybe StockQuote)
sp500 = getStockQuote "^GSPC"

ftse :: IO (Maybe StockQuote)
ftse = getStockQuote "^FTSE"

aim :: IO (Maybe StockQuote)
aim = getStockQuote "^FTAI"
