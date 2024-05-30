{-# LANGUAGE OverloadedStrings #-}

module View 
( renderIndex 
) where
import Text.Mustache (automaticCompile, Template, substitute, ToMustache (toMustache), object, (~>))
import Pricing (PricesDTO (prices, livePrice), PriceDTO (price, totalChange, annualisedChange, year))
import qualified Data.Text as DT
import Text.Printf (printf)

templateDir :: FilePath
templateDir = "./app/templates"

data PricesUI = PricesUI 
    { ps :: ![PriceUI] 
    , lp :: !String
    }

instance ToMustache PricesUI where
    toMustache (PricesUI p lp) = object 
        [ "prices" ~> p 
        , "livePrice" ~> lp
        ]

data PriceUI = PriceUI
    { y :: !Integer
    , p :: !String
    , t :: !String
    , a :: !String
    }

instance ToMustache PriceUI where
    toMustache (PriceUI y p t a) = object 
        [ "year" ~> y
        , "price" ~> p
        , "totalChange" ~> t
        , "annualisedChange" ~> a
        ]

template :: IO Template
template = do
    ts <- automaticCompile [templateDir] "index.mustache"
    case ts of
        Left e -> error $ show e
        Right t -> pure t

formatPrice :: Double -> String
formatPrice d = 
    let str = printf "%.2f" d
        dec = dropWhile (/= '.') str
        full = takeWhile (/= '.') str
        rev = reverse full

        spl :: String -> String
        spl [] = []
        spl s = 
            let th = take 3 s
            in th ++ "," ++ spl (drop 3 s)
    in (drop 1 $ reverse $ spl rev) ++ dec

renderIndex :: PricesDTO -> IO DT.Text
renderIndex ps = do
    let 
        uiPrices :: [PriceUI]
        uiPrices = map (\e -> 
            PriceUI 
                { y = year e
                , p = formatPrice $ price e
                , t = formatPrice $ totalChange e
                , a = formatPrice $ annualisedChange e
                }
            ) (prices ps)
    t <- template
    pure $ substitute t (
        PricesUI
            { ps = uiPrices
            , lp = formatPrice $ livePrice ps 
            }
        )
