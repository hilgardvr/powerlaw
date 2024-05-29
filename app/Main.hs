{-# LANGUAGE OverloadedStrings #-}

module Main where

import Env 
import CoinGeckoClient (getPrice)
import Data.Time (getCurrentTime)
import Pricing (buildPrices)
import Web.Scotty (scotty, get, html)
import Control.Monad.IO.Class (MonadIO(liftIO))
import View (renderIndex)
import qualified Data.Text.Lazy as TL 

main :: IO ()
main = do
    env <- getLocalEnv
    scotty 3000 $ do 
        get "/" $ do
            p <- liftIO $ getPrice env
            now <- liftIO getCurrentTime
            let years = take 8 (0 : [ 2^i | i <- [0..] ]) 
                ps = buildPrices p now years
            t <- liftIO $ renderIndex ps
            html $ TL.fromStrict t

