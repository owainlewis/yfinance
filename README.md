# YFinance

A Haskell library for querying real time stock market data including indexes such as NASDAQ and the S&P500

```haskell

module Example where

import qualified Data.Text as T
import Network.Yahoo.Finance

getStockQuote "YHOO"

```

## Get the current stock price for a company

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

--λ> nasdaq
-- Just (StockQuote {name = "NASDAQ-100",
--                   symbol = "^NDX",
--                   price = "4197.27",
--                   change = "-187.86",
--                   dayHigh = "4361.32",
--                   dayLow = "4197.27",
--                   yearHigh = "4694.13",
--                   yearLow = "3700.23"})
```
