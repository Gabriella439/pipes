-- | Entry point for the Control.Proxy.Prelude hierarchy

module Control.Proxy.Prelude (
    -- $modules
    module Control.Proxy.Prelude.Base,
    module Control.Proxy.Prelude.IO,
    module Control.Proxy.Prelude.Kleisli
    ) where

import Control.Proxy.Prelude.Base
import Control.Proxy.Prelude.IO
import Control.Proxy.Prelude.Kleisli

{- $modules
    "Control.Proxy.Prelude.Base" provides pure utility proxies.

    "Control.Proxy.Prelude.IO" provides proxies for simple 'IO'.

    "Control.Proxy.Prelude.Kleisli" provides convenience functions for working
    with Kleisli arrows.
-}
