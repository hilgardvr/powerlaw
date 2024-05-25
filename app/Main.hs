{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError, diffUTCTime, timeToDaysAndTimeOfDay, getCurrentTime)
import Network.HTTP.Client.Conduit (parseRequest, Request (method, requestHeaders), Response (responseBody))
import Network.HTTP.Simple (httpBS)
import qualified Data.Aeson
import Data.Aeson (FromJSON, (.:), withObject)
import qualified Data.ByteString.Lazy as BSL
import System.Environment (setEnv, getEnv)
import qualified Data.String as DS

type Price = Double

yearDays :: Integer
yearDays = 365

main :: IO ()
main = do
    setLocalEnv
    p <- getPrice
    print $ "price: " ++ show p
    getYearFromNowComparison 0 p
    getYearFromNowComparison 1 p
    getYearFromNowComparison 2 p
    getYearFromNowComparison 3 p
    getYearFromNowComparison 4 p
    getYearFromNowComparison 5 p
    getYearFromNowComparison 8 p
    getYearFromNowComparison 16 p
    

getYearFromNowComparison :: Integer -> Price -> IO ()
getYearFromNowComparison years exchangePrice = do
    now <- getCurrentTime
    let daysNow = daysFromGenesis now
        formulaPrice = formula (daysNow + years * yearDays)
    print $ "Model price in " ++ show years ++ " years: $" ++  show formulaPrice
    print $ "Current price $" ++  show exchangePrice
    print $ "Implied total gain/(loss) in " ++ show years ++ " years %" ++ show (formulaPrice / exchangePrice * 100 - 100)
    if years > 0
    then print $ "Implied annualised gain/(loss) over " ++ show years ++ " years %" ++ show (annualised formulaPrice exchangePrice years)
    else return ()
    print $ "----"

annualised :: Double -> Double -> Integer -> Double
annualised formulaPrice exchangePrice  years =
    ((formulaPrice / exchangePrice) ** (recip $ fromIntegral years) - 1) * 100


formula :: Integer -> Double
formula daysSinceGenesis = 
    (10 ** (-16.45)) * (fromIntegral daysSinceGenesis ** 5.67)

genesisBlock :: UTCTime
genesisBlock =
    let dateSting = "2009 Jan 03"
    in parseTimeOrError True defaultTimeLocale "%Y %b %-d" dateSting :: UTCTime

daysFromGenesis :: UTCTime -> Integer
daysFromGenesis = dayTimingDiff genesisBlock

dayTimingDiff :: UTCTime -> UTCTime -> Integer
dayTimingDiff from to = 
    let
        diff = diffUTCTime to from
        (days, timeOfDay) = timeToDaysAndTimeOfDay diff
    in days

data BitcoinPriceResponse = BitcoinPriceResponse
    { bitcoin :: CurrencyResponse }

instance FromJSON BitcoinPriceResponse where
    parseJSON = withObject "BitcoinPriceResponse" $ \v -> BitcoinPriceResponse
        <$> v .: "bitcoin"

data CurrencyResponse = CurrencyResponse 
    { usd :: Price }

instance FromJSON CurrencyResponse where
    parseJSON = withObject "CurrencyResponse" $ \v -> CurrencyResponse
        <$> v .: "usd"

splitStringAt :: String -> Char -> [String]
splitStringAt [] _ = []
splitStringAt s c = 
    if head s /= c
    then 
        let str = takeWhile (/= c) s
        in str : splitStringAt (dropWhile (/= c) s) c
    else 
        splitStringAt (dropWhile (== c) s) c
    

setLocalEnv :: IO ()
setLocalEnv = do
    f <- readFile ".env"
    let ls = lines f
    mapM_ (\v -> 
        let spl = splitStringAt v '='
        in do
            let key = head spl
                value = spl!!1
            print $ "k: " ++ key ++ " v: " ++ value
            setEnv key value
        ) ls

getPrice :: IO Price
getPrice = do
    key <- getEnv "API_KEY"
    print key
    initReq <- parseRequest  "https://coingecko.p.rapidapi.com/simple/price?vs_currencies=usd&ids=bitcoin" 
    let req = initReq
            { method = "GET"
            , requestHeaders = 
                [ ("X-RapidAPI-Key", DS.fromString key)
                , ("X-RapidAPI-Host", "coingecko.p.rapidapi.com")
                ]
            }
    res <- httpBS req
    let price = (Data.Aeson.decode $ (BSL.fromStrict $ responseBody res)) :: (Maybe BitcoinPriceResponse)
    case price of
        Nothing -> error $ "Error decoding json from: " ++ show res
        Just p -> return (usd $ bitcoin p)
