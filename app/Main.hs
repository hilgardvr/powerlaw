{-# LANGUAGE OverloadedStrings #-}

module Main where

import Env 
import Data.Time (getCurrentTime)
import Pricing (buildPrices)
import Web.Scotty (scotty, get, html)
import Control.Monad.IO.Class (MonadIO(liftIO))
import View (renderIndex)
import qualified Data.Text.Lazy as TL
import GHC.IO.Handle (hSetBuffering, BufferMode (LineBuffering))
import GHC.IO.StdHandles (stdout)
import CoinGeckoClient (CoinGeckoClient(..))
import PricingClient (PricingClient(getBTCPrice))
import Repo (getCachedPrice)

main :: IO ()
main = do
    env <- getLocalEnv
    let pricingClient = CoinGeckoClient { env = env }
    hSetBuffering stdout LineBuffering
    scotty 3000 $ do
        get "/" $ do
            let years = take 8 ([ 2^i | i <- [0..] ])
            now <- liftIO $ getCurrentTime
            cachedPrice <- liftIO $ getCachedPrice (conn env)
            price <- case cachedPrice of
                Nothing -> do
                    clientPrice <- liftIO $ getBTCPrice pricingClient
                    case clientPrice of
                        Left err -> error err
                        Right p -> return p
                Just p -> return p
            let prices = buildPrices price now years
            t <- liftIO $ renderIndex prices
            html $ TL.fromStrict t
