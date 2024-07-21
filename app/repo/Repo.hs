module Repo
( CachedPrice(..)
, cachePrice
, getCachedPrice
) where
import Pricing (Price)
import Database.SQLite.Simple (Connection, FromRow (fromRow), ToRow (toRow), field, Query (Query), execute, query_)
import Control.Exception (SomeException, try)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.Text as T


data CachedPrice = CachedPrice
    { id :: !(Maybe Integer)
    , price :: !Price
    , added :: !UTCTime
    } deriving Show

instance FromRow CachedPrice where
    fromRow = CachedPrice <$> field <*> field <*> field

instance ToRow CachedPrice where
    toRow (CachedPrice i p a) = toRow (p, a)

cachePrice :: Connection -> Price -> IO ()
cachePrice conn p = do
    putStrLn $ "trying to insert price into cache" ++ show p
    now <- getCurrentTime
    putStrLn $ "now" ++ show now
    either <- try $ execute conn (Query $ T.pack "insert into prices (price, added) values (?,?)") (CachedPrice Nothing p now) :: IO (Either SomeException ())
    case either of
        Left e -> putStrLn $ "error fetching from db: " ++ show e
        Right r -> putStrLn $ "done inserting  price into cache" ++ show r

getCachedPrice :: Connection -> IO (Maybe Price)
getCachedPrice conn = do
    putStrLn "trying to get price into cache"
    let q = Query (T.pack "select id, price, added from prices order by id desc limit 1;")
    putStrLn $ "query: " ++ show q
    res <- try $  query_ conn q :: IO (Either SomeException [CachedPrice])
    putStrLn $ "res: " ++ show res
    case res of
        Left e -> do
            putStrLn $ "error fetching from db: " ++ show e
            pure Nothing
        Right r -> 
            case r of 
                [] -> do
                    putStrLn $ "no cached prices"
                    pure Nothing
                (h:_) -> do
                    now <- getCurrentTime
                    let diff = diffUTCTime now (added h) 
                    putStrLn $ "cached timing diff seconds: " ++ show diff
                    if diff > (15 * 60)
                    then pure Nothing 
                    else pure $ Just $ price h
