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
    let years = take 10 (0 : [ 2^i | i <- [0..] ]) 
        disp = map (outputYearFromNowComparison p now)  years
    print $ "---- Current Price: " ++ printf "%.2f" p ++ " ----"
    mapM_ (print . intercalate "  -  ")  disp

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
        , "Implied total gain/(loss) in " ++ show years ++ " years %" ++ printf "%.2f" (formulaPrice / exchangePrice * 100 - 100)
        , implied
        ]
