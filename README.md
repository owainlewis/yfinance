# YFinance

A Haskell interface to Yahoo Finance

```haskell

module Example where

import qualified Data.Text as T
import Network.Yahoo.Finance

getStockQuote (T.pack "YHOO")
-- Just (YahooResponse {quote = StockQuote {name = "Yahoo! Inc.", symbol = "YHOO", price = "36.11"}})
