-- | Top-level import for the "Control.Proxy" hierarchy

module Control.Proxy (
    -- * Modules
    -- $modules
    module Control.Proxy.Class,
    module Control.Proxy.Core,
    module Control.Proxy.Pipe,
    module Control.Proxy.Trans
    ) where

import Control.Proxy.Class
import Control.Proxy.Core
import Control.Proxy.Pipe
import Control.Proxy.Trans

{- $modules
    "Control.Proxy.Core" provides the core type and primitives.

    Consult "Control.Proxy.Tutorial" for an extended tutorial.
-}
