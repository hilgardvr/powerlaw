{-# LANGUAGE OverloadedStrings #-}

module Main where

import Env 
import CoinGeckoClient (Price, getPrice)
import Data.Time (getCurrentTime, UTCTime)
import Pricing (daysFromGenesis, formula, annualised, PriceDTO (PriceDTO, price, year, totalChange, annualisedChange), PricesDTO (prices, PricesDTO))
import Data.List as DL
import Text.Printf (printf)
import Web.Scotty (scotty, get, html)
import Control.Monad.IO.Class (MonadIO(liftIO))
import View (renderIndex)
import qualified Data.Text.Lazy as TL 

yearDays :: Integer
yearDays = 365

type Year = Integer

main :: IO ()
main = do
    env <- getLocalEnv
    p <- getPrice env
    now <- getCurrentTime
    let years = take 8 (0 : [ 2^i | i <- [0..] ]) 
        disp = map (outputYearFromNowComparison p now)  years
        prices = buildPrices p now years
    print $ "---- Current Price: " ++ printf "%.2f" p ++ " ----"
    mapM_ (print . intercalate "  -  ")  disp
    scotty 3000 $ do 
        get "/" $ do
            t <- liftIO $ renderIndex prices
            html $ TL.fromStrict t

buildPrices :: Price -> UTCTime -> [Year] -> PricesDTO
buildPrices p now ys  = 
    let daysNow = daysFromGenesis now
        ps' = map (\y -> 
            let fp = formula (daysNow + y * yearDays)
            in PriceDTO 
                    { price = fp
                    , year = y
                    , totalChange = fp / p * 100 - 100
                    , annualisedChange = annualised fp p y
                    }
            ) ys
    in PricesDTO { prices = ps' }
        

outputYearFromNowComparison :: Price -> UTCTime -> Integer -> [String] 
outputYearFromNowComparison exchangePrice now years =
    let daysNow = daysFromGenesis now
        formulaPrice = formula (daysNow + years * yearDays)
        implied = 
            if years > 0
            then "Model annualised gain/(loss) over " ++ show years ++ " years %" ++ printf "%.2f" (annualised formulaPrice exchangePrice years)
            else ""
    in
        [ "Model price in " ++ show years ++ " years: $" ++  printf "%.2f" formulaPrice
        , "Model total gain/(loss) in " ++ show years ++ " years %" ++ printf "%.2f" (formulaPrice / exchangePrice * 100 - 100)
        , implied
        ]
