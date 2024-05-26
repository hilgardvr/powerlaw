{-# LANGUAGE OverloadedStrings #-}

module Main where

import Env 
import CoinGeckoClient (Price, getPrice)
import Data.Time (getCurrentTime, UTCTime)
import Pricing (daysFromGenesis, formula, annualised)
import Data.List

yearDays :: Integer
yearDays = 365

main :: IO ()
main = do
    env <- getLocalEnv
    p <- getPrice env
    now <- getCurrentTime
    print $ "Current Price: " ++ show p
    mapM_ (print . intercalate "  -  ")  
        [ outputYearFromNowComparison 0 p now
        , outputYearFromNowComparison 1 p now
        , outputYearFromNowComparison 2 p now
        , outputYearFromNowComparison 3 p now
        , outputYearFromNowComparison 4 p now
        , outputYearFromNowComparison 5 p now
        , outputYearFromNowComparison 8 p now
        , outputYearFromNowComparison 16 p now
        ]
    

getYearFromNowComparison :: Integer -> Price -> IO ()
getYearFromNowComparison years exchangePrice = do
    now <- getCurrentTime
    let daysNow = daysFromGenesis now
        formulaPrice = formula (daysNow + years * yearDays)
    print $ show years ++ " years: $" ++  show formulaPrice
    print $ "Current price $" ++  show exchangePrice
    print $ "Implied total gain/(loss) in " ++ show years ++ " years %" ++ show (formulaPrice / exchangePrice * 100 - 100)
    if years > 0
    then print $ "Implied annualised gain/(loss) over " ++ show years ++ " years %" ++ show (annualised formulaPrice exchangePrice years)
    else return ()
    print $ "----"

outputYearFromNowComparison :: Integer -> Price -> UTCTime -> [String]
outputYearFromNowComparison years exchangePrice now =
    let daysNow = daysFromGenesis now
        formulaPrice = formula (daysNow + years * yearDays)
        implied = 
            if years > 0
            then "Implied annualised gain/(loss) over " ++ show years ++ " years %" ++ show (annualised formulaPrice exchangePrice years)
            else ""
    in
        [ ("Model price in " ++ show years ++ " years: $" ++  show formulaPrice)
        , "Current price $" ++  show exchangePrice
        , "Implied total gain/(loss) in " ++ show years ++ " years %" ++ show (formulaPrice / exchangePrice * 100 - 100)
        , implied
        ]
