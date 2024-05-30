module Env
( Env(..)
, getLocalEnv
) where
import System.Environment (setEnv, getEnv)
import Database.SQLite.Simple (Connection, open, execute_, Query (Query))
import qualified Data.Text as T

data Env = Env 
    { apiKey :: String
    , conn :: Connection
    }

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
            print $ key ++ ": " ++ take 3 value
            setEnv key value
        ) ls

getLocalEnv :: IO Env
getLocalEnv = do
    setLocalEnv
    conn <- open "powerlaw-cache" 
    execute_ conn (Query $ T.pack "CREATE TABLE IF NOT EXISTS prices (id INTEGER PRIMARY KEY, price REAL, added TEXT);")
    val <- getEnv "API_KEY"
    return Env 
        { apiKey = val 
        , conn = conn
        }
