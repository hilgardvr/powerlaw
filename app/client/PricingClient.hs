module PricingClient
( PricingClient
, getBTCPrice
) where 
import Pricing (Price)

class PricingClient a where
    getBTCPrice :: a -> IO (Either String Price)
