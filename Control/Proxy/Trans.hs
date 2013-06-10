{-| You can define your own proxy extensions by writing your own \"proxy
    transformers\".  Proxy transformers are monad transformers that also
    correctly lift all proxy operations from the base proxy type to the
    extended proxy type.  Stack multiple proxy transformers to chain features
    together.
-}
    
module Control.Proxy.Trans (
    -- * Proxy Transformers
    ProxyTrans(..)
    ) where

import Control.Proxy.Class (Proxy)

-- | Uniform interface to lifting proxies
class ProxyTrans t where
    liftP :: (Monad m, Proxy p) => p a' a b' b m r -> t p a' a b' b m r
