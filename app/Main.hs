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
import GHC.IO.Handle (hSetBuffering, BufferMode (LineBuffering))
import GHC.IO.StdHandles (stdout)

main :: IO ()
main = do
    env <- getLocalEnv
    hSetBuffering stdout LineBuffering
    scotty 3000 $ do 
        get "/" $ do
            let years = take 8 ([ 2^i | i <- [0..] ])
            now <- liftIO $ getCurrentTime
            p <- liftIO $ getPrice env
            let prices = buildPrices p now years
            t <- liftIO $ renderIndex prices
            html $ TL.fromStrict t
