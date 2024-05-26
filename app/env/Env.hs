module Env
( Env(..)
, getLocalEnv
) where
import System.Environment (setEnv, getEnv)

data Env = Env 
    { apiKey :: String
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
            print $ "k: " ++ key ++ " v: " ++ value
            setEnv key value
        ) ls

getLocalEnv :: IO Env
getLocalEnv = do
    setLocalEnv
    val <- getEnv "API_KEY"
    return Env 
        { apiKey = val }
