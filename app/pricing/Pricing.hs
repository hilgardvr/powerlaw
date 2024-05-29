{-# LANGUAGE OverloadedStrings #-}

module Pricing 
( daysFromGenesis
, formula
, annualised
, PriceDTO(..)
, PricesDTO(..)
) where

import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError, diffUTCTime, timeToDaysAndTimeOfDay)
import CoinGeckoClient (Price)

data PricesDTO = PricesDTO { prices :: [PriceDTO] }

data PriceDTO = PriceDTO
 { year :: Integer
 , price :: Price
 , totalChange :: Double
 , annualisedChange :: Double
 }

annualised :: Double -> Double -> Integer -> Double
annualised formulaPrice exchangePrice  years =
    ((formulaPrice / exchangePrice) ** (recip $ fromIntegral years) - 1) * 100


formula :: Integer -> Double
formula daysSinceGenesis = 
    (10 ** (-16.45)) * (fromIntegral daysSinceGenesis ** 5.67)

daysFromGenesis :: UTCTime -> Integer
daysFromGenesis = dayTimingDiff genesisBlock

dayTimingDiff :: UTCTime -> UTCTime -> Integer
dayTimingDiff from to = 
    let
        diff = diffUTCTime to from
        (days, timeOfDay) = timeToDaysAndTimeOfDay diff
    in days

genesisBlock :: UTCTime
genesisBlock =
    let dateSting = "2009 Jan 03"
    in parseTimeOrError True defaultTimeLocale "%Y %b %-d" dateSting :: UTCTime

