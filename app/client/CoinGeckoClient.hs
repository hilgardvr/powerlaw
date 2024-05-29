{-# LANGUAGE OverloadedStrings #-}

module CoinGeckoClient
( getPrice
, Price
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), decode)
import Env (Env (apiKey))
import Network.HTTP.Client.Conduit (Request(method, requestHeaders), parseRequest, Response (responseBody))
import qualified Data.String as DS
import Network.HTTP.Simple (httpBS)
import qualified Data.ByteString.Lazy as BSL

type Price = Double

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


getPrice :: Env -> IO Price
getPrice env = do
    initReq <- parseRequest  "https://coingecko.p.rapidapi.com/simple/price?vs_currencies=usd&ids=bitcoin" 
    let req = initReq
            { method = "GET"
            , requestHeaders = 
                [ ("X-RapidAPI-Key", DS.fromString (apiKey env))
                , ("X-RapidAPI-Host", "coingecko.p.rapidapi.com")
                ]
            }
    res <- httpBS req
    let price = (Data.Aeson.decode $ (BSL.fromStrict $ responseBody res)) :: (Maybe BitcoinPriceResponse)
    case price of
        Nothing -> error $ "Error decoding json from: " ++ show res
        Just p -> return (usd $ bitcoin p)
    --pure 67000
