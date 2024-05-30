{-# LANGUAGE OverloadedStrings #-}

module CoinGeckoClient
( getPrice
) where
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), decode)
import Env (Env (apiKey, conn))
import Network.HTTP.Client.Conduit (Request(method, requestHeaders), parseRequest, Response (responseBody))
import qualified Data.String as DS
import Network.HTTP.Simple (httpBS)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Database.SQLite.Simple (query_, Query (Query), ToRow (toRow), execute, Connection, FromRow (fromRow), field)
import Control.Exception (try, SomeException)
import Pricing (Price)

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

data CachedPrice = CachedPrice
    { id :: !(Maybe Integer)
    , price :: !Price
    , added :: !UTCTime
    } deriving Show

instance FromRow CachedPrice where
    fromRow = CachedPrice <$> field <*> field <*> field

instance ToRow CachedPrice where
    toRow (CachedPrice i p a) = toRow (p, a)

getPrice :: Env -> IO Price
getPrice env = do
    cachedPrice <- getCachedPrice (conn env)
    print $ "got cachedPrice of: " ++ show cachedPrice
    case cachedPrice of
        Nothing -> do
            print "hitting api"
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
                Just p -> do
                    cachePrice (conn env) (usd $ bitcoin p)
                    return (usd $ bitcoin p)
        Just c -> pure c

cachePrice :: Connection -> Price -> IO ()
cachePrice conn p = do
    print $ "trying to insert price into cache" ++ show p
    now <- getCurrentTime
    print $ "now" ++ show now
    either <- try $ execute conn (Query $ T.pack "insert into prices (price, added) values (?,?)") (CachedPrice Nothing p now) :: IO (Either SomeException ())
    case either of
        Left e -> print $ "error fetching from db: " ++ show e
        Right r -> print $ "done inserting  price into cache" ++ show r

getCachedPrice :: Connection -> IO (Maybe Price)
getCachedPrice conn = do
    print "trying to get price into cache"
    let q = Query (T.pack "select id, price, added from prices order by id desc limit 1;")
    print $ "query: " ++ show q
    res <- try $  query_ conn q :: IO (Either SomeException [CachedPrice])
    print $ "res: " ++ show res
    case res of
        Left e -> do
            print $ "error fetching from db: " ++ show e
            pure Nothing
        Right r -> 
            case r of 
                [] -> do
                    print $ "no cached prices"
                    pure Nothing
                (h:_) -> do
                    now <- getCurrentTime
                    let diff = diffUTCTime now (added h) 
                    print $ "cached timing diff seconds: " ++ show diff
                    if diff > (15 * 60)
                    then pure Nothing 
                    else pure $ Just $ price h
