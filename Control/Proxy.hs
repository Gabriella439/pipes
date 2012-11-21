-- | Default imports for the "Control.Proxy" hierarchy

module Control.Proxy (
    -- * Modules
    -- $modules
    module Control.Proxy.Class,
    module Control.Proxy.Core.Fast,
    module Control.Proxy.Synonym,
    module Control.Proxy.Trans,
    module Control.Proxy.Trans.Identity,
    module Control.Proxy.Prelude
    ) where

import Control.Proxy.Class
import Control.Proxy.Core.Fast
import Control.Proxy.Synonym
import Control.Proxy.Trans
import Control.Proxy.Trans.Identity
import Control.Proxy.Prelude

{- $modules
    "Control.Proxy.Core.Fast" provides a fast 'Proxy' implementation.

    "Control.Proxy.Class" type-classes proxy operations.

    "Control.Proxy.Synonym" defines type synonyms for proxies that don't use all
    of their inputs or outputs.

    "Control.Proxy.Trans" defines the proxy transformer type class.

    "Control.Proxy.Prelude" provides a standard library of proxies.

    Consult "Control.Proxy.Tutorial" for an extended tutorial.
-}
