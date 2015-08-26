# YFinance

A Haskell library for querying real time stock market data including indexes such as NASDAQ and the S&P500

```haskell

module Example where

import qualified Data.Text as T
import Network.Yahoo.Finance

getStockQuote "YHOO"

-- λ> getStockQuote "YHOO"
-- Just (StockQuote {name = "Yahoo! Inc.", symbol = "YHOO", price = "32.93", change = "-1.17",
--                   dayHigh = "34.02", dayLow = "32.91", yearHigh = "52.62", yearLow = "32.91"})

```

## Get the current stock price for a company

Yahoo finance responses are all strings so you will need to cast the response to a double.

```haskell
-- Get the current price for Google stock
fmap (toDouble . price) <$> getStockQuote "GOOGL"
-- Just 644.03
```

## Popular stock indexes

There are some helper methods for accessing popular indexes such as the S&P500 or NASDAQ

```haskell
module Example where

import Network.Yahoo.Finance

-- λ> nasdaq
-- Just (StockQuote {name = "NASDAQ-100",
--                   symbol = "^NDX",
--                   price = "4197.27",
--                   change = "-187.86",
--                   dayHigh = "4361.32",
--                   dayLow = "4197.27",
--                   yearHigh = "4694.13",
--                   yearLow = "3700.23"})
```


## Historical Prices

This example shows how to fetch the Stock prices for GOOGL between 2014 and 2015

```haskell
module Example where

import Network.Yahoo.Finance

-- λ> historical "GOOGL" "2014-01-01" "2015-01-01"

```
