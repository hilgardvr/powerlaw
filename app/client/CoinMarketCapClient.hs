{-# LANGUAGE OverloadedStrings #-}

module CoinMarketCapClient
( CoinMarketCapClient(..)
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), decode)
import Env (Env (cmcApiKey))
import Network.HTTP.Client.Conduit (Request(method, requestHeaders), parseRequest, Response (responseBody))
import qualified Data.String as DS
import Network.HTTP.Simple (httpBS, getResponseStatusCode, getResponseBody)
import qualified Data.ByteString.Lazy as BSL
import Pricing (Price)
import PricingClient (PricingClient (getBTCPrice))

newtype CoinMarketCapClient = CoinMarketCapClient { env :: Env }

instance PricingClient CoinMarketCapClient where
    getBTCPrice (CoinMarketCapClient env) = getPrice env

data BitcoinPriceResponse = BitcoinPriceResponse
    { status :: StatusResponse 
    , rData :: [ResponseData] }

instance FromJSON BitcoinPriceResponse where
    parseJSON = withObject "BitcoinPriceResponse" $ \v -> BitcoinPriceResponse
        <$> v .: "status"
        <*> v .: "data"

data StatusResponse = StatusResponse 
    { -- timestamp :: LocalTime
     error_code :: Int
    , error_message :: Maybe String
    , credit_count :: Int
    , notice :: Maybe String
    , total_count :: Int
    }

instance FromJSON StatusResponse where
    parseJSON = withObject "StatusResponse" $ \v -> StatusResponse
        -- <$> v .: "timestamp"
        <$> v .: "error_code"
        <*> v .: "error_message"
        <*> v .: "credit_count"
        <*> v .: "notice"
        <*> v .: "total_count"

data ResponseData = ResponseData 
    { id :: Integer
    , name :: String
    , symbol :: String
    , max_supply :: Integer
    , circulating_supply :: Integer
    , quote :: ResponseQuote
    }

instance FromJSON ResponseData where
    parseJSON = withObject "ResponseData" $ \v -> ResponseData
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "symbol"
        <*> v .: "max_supply"
        <*> v .: "circulating_supply"
        <*> v .: "quote"

data ResponseQuote = ResponseQuote 
    { usd :: CurrencyResponseQuote }

instance FromJSON ResponseQuote where
    parseJSON = withObject "ResponseQuote" $ \v -> ResponseQuote
        <$> v .: "USD"

data CurrencyResponseQuote = CurrencyResponseQuote
    { price :: Double 
    , market_cap :: Double
    , market_cap_dominance :: Double
    , fully_diluted_market_cap :: Double
    }

instance FromJSON CurrencyResponseQuote where
    parseJSON = withObject "CurrencyResponseQuote" $ \v -> CurrencyResponseQuote
        <$> v .: "price"
        <*> v .: "market_cap"
        <*> v .: "market_cap_dominance"
        <*> v .: "fully_diluted_market_cap"

getPrice :: Env -> IO (Either String Price)
getPrice env = do
    putStrLn "hitting api"
    initReq <- parseRequest  "https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest?market_cap_min=500000000000"
    let req = initReq
            { method = "GET"
            , requestHeaders = 
                [ ("X-CMC_PRO_API_KEY", DS.fromString (cmcApiKey env))
                , ("Accept", "application/json")
                ]
            }
    res <- httpBS req
    if getResponseStatusCode res == 200 
    then do
        let response = Data.Aeson.decode (BSL.fromStrict $ responseBody res) :: (Maybe BitcoinPriceResponse)
        case response of
            Nothing -> do
                print $ "Error decoding json from: " ++ show res
                error $ "Error decoding json from: " ++ show res
            Just res -> do
                return $ Right (findBitcoinPrice (rData res))
    else do
        print res
        return $ Left $ "Failed to get response - status " ++ show (getResponseStatusCode res) ++ " body: " ++ show (getResponseBody res)


findBitcoinPrice :: [ResponseData] -> Price
findBitcoinPrice [] = error "Bitcoin response not found"
findBitcoinPrice (h:t) =
    if symbol h == "BTC"
    then price (usd (quote h))
    else findBitcoinPrice t
