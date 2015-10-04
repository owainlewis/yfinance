module Main where

import           Network.Yahoo.Finance

type SQ = IO (Maybe StockQuote)

-- Indexes

nasdaq :: SQ
nasdaq = getStockQuote "^NDX"

sp500 :: SQ
sp500 = getStockQuote "^GSPC"

ftse :: SQ
ftse = getStockQuote "^FTSE"

ftse250 :: SQ
ftse250 = getStockQuote "^FTMC"

aim :: SQ
aim = getStockQuote "^FTAI"

dax :: SQ
dax = getStockQuote "^GDAXI"

-- Commodities

oil :: SQ
oil = getStockQuote "BZX15.NYM"

gold :: SQ
gold = getStockQuote "GCZ15.CMX"

silver :: SQ
silver = getStockQuote "SIZ15.CMX"

copper :: SQ
copper = getStockQuote "HGZ15.CMX"
