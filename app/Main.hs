{-# LANGUAGE OverloadedStrings #-}

module Main where

import Env 
import CoinGeckoClient (Price, getPrice)
import Data.Time (getCurrentTime, UTCTime)
import Pricing (daysFromGenesis, formula, annualised)
import Data.List
import Text.Printf (printf)

yearDays :: Integer
yearDays = 365

main :: IO ()
main = do
    env <- getLocalEnv
    p <- getPrice env
    now <- getCurrentTime
    let years = take 15 (0 : [ 2^i | i <- [0..] ]) 
        disp = map (outputYearFromNowComparison p now)  years
    print $ "---- Current Price: " ++ show p ++ " ----"
    mapM_ (print . intercalate "  -  ")  disp
        --[ outputYearFromNowComparison p now 0
        --, outputYearFromNowComparison p now 1
        --, outputYearFromNowComparison p now 2
        --, outputYearFromNowComparison p now 3
        --, outputYearFromNowComparison p now 4
        --, outputYearFromNowComparison p now 5
        --, outputYearFromNowComparison p now 8
        --, outputYearFromNowComparison p now 16
        --]

outputYearFromNowComparison :: Price -> UTCTime -> Integer -> [String] 
outputYearFromNowComparison exchangePrice now years =
    let daysNow = daysFromGenesis now
        formulaPrice = formula (daysNow + years * yearDays)
        implied = 
            if years > 0
            then "Implied annualised gain/(loss) over " ++ show years ++ " years %" ++ printf "%.2f" (annualised formulaPrice exchangePrice years)
            else ""
    in
        [ ("Model price in " ++ show years ++ " years: $" ++  printf "%.2f" formulaPrice)
        --, "Current price $" ++  printf "%.2f" exchangePrice
        , "Implied total gain/(loss) in " ++ show years ++ " years %" ++ printf "%.2f" (formulaPrice / exchangePrice * 100 - 100)
        , implied
        ]
