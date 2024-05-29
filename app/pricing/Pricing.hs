{-# LANGUAGE OverloadedStrings #-}

module Pricing 
( buildPrices
, PriceDTO(..)
, PricesDTO(..)
) where

import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError, diffUTCTime, timeToDaysAndTimeOfDay)
import CoinGeckoClient (Price)

data PricesDTO = PricesDTO 
    { prices :: ![PriceDTO] 
    , livePrice :: !Price
    }

data PriceDTO = PriceDTO
 { year :: !Integer
 , price :: !Price
 , totalChange :: !Double
 , annualisedChange :: !Double
 }

type Year = Integer

yearDays :: Integer
yearDays = 365

annualised :: Double -> Double -> Integer -> Double
annualised formulaPrice exchangePrice  years =
    ((formulaPrice / exchangePrice) ** (recip $ fromIntegral years) - 1) * 100

percentageChange :: Price -> Price -> Double
percentageChange modelPrice marketPrice = modelPrice / marketPrice * 100 - 100

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


buildPrices :: Price -> UTCTime -> [Year] -> PricesDTO
buildPrices p now ys  = 
    let daysNow = daysFromGenesis now
        ps' = map (\y -> 
            let fp = formula (daysNow + y * yearDays)
            in PriceDTO 
                { price = fp
                , year = y
                , totalChange = percentageChange fp p 
                , annualisedChange = annualised fp p y
                }
            ) ys
    in PricesDTO 
            { prices = ps' 
            , livePrice = p
            }
