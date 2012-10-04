-- | Default imports for the "Control.Proxy" hierarchy

module Control.Proxy (
    -- * Modules
    -- $modules
    module Control.Proxy.Class,
    module Control.Proxy.Core,
    module Control.Proxy.Pipe,
    module Control.Proxy.Trans,
    module Control.Proxy.Util
    ) where

import Control.Proxy.Class
import Control.Proxy.Core
import Control.Proxy.Pipe
import Control.Proxy.Trans
import Control.Proxy.Util

{- $modules
    "Control.Proxy.Core" provides the core 'Proxy' type.

    "Control.Proxy.Class" provides the abstract interface to 'Proxy' operations.

    "Control.Proxy.Trans" provides proxy transformers.

    "Control.Proxy.Pipe" provides a backwards-compatible re-implementation of
    'Pipe's.

    "Control.Proxy.Util" provides utility proxies for common tasks

    Consult "Control.Proxy.Tutorial" for an extended tutorial.
-}
