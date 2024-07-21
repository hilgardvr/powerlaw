{-# LANGUAGE OverloadedStrings #-}

module CoinGeckoClient
( CoinGeckoClient(..)
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), decode)
import Env (Env (apiKey, conn))
import Network.HTTP.Client.Conduit (Request(method, requestHeaders), parseRequest, Response (responseBody))
import qualified Data.String as DS
import Network.HTTP.Simple (httpBS, getResponseStatusCode, getResponseBody)
import qualified Data.ByteString.Lazy as BSL
import Pricing (Price)
import PricingClient (PricingClient (getBTCPrice))
import Repo (getCachedPrice, cachePrice)

newtype CoinGeckoClient = CoinGeckoClient { env :: Env }

instance PricingClient CoinGeckoClient where
    getBTCPrice (CoinGeckoClient env) = getPrice env

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

getPrice :: Env -> IO (Either String Price)
getPrice env = do
    cachedPrice <- getCachedPrice (conn env)
    putStrLn $ "got cachedPrice of: " ++ show cachedPrice
    case cachedPrice of
        Nothing -> do
            putStrLn "hitting api"
            initReq <- parseRequest  "https://coingecko.p.rapidapi.com/simple/price?vs_currencies=usd&ids=bitcoin" 
            let req = initReq
                    { method = "GET"
                    , requestHeaders = 
                        [ ("X-RapidAPI-Key", DS.fromString (apiKey env))
                        , ("X-RapidAPI-Host", "coingecko.p.rapidapi.com")
                        ]
                    }
            res <- httpBS req
            if getResponseStatusCode res == 200 
            then do
                print $ "response: " ++ show res
                let price = Data.Aeson.decode (BSL.fromStrict $ responseBody res) :: (Maybe BitcoinPriceResponse)
                case price of
                    Nothing -> error $ "Error decoding json from: " ++ show res
                    Just p -> do
                        cachePrice (conn env) (usd $ bitcoin p)
                        return $ Right (usd $ bitcoin p)
            else return $ Left $ "Failed to get response - status " ++ show (getResponseStatusCode res) ++ " body: " ++ show (getResponseBody res)
        Just c -> pure $ Right c

