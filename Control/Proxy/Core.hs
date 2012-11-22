-- | Default imports for the "Control.Proxy" hierarchy

module Control.Proxy.Core (
    -- * Modules
    -- $modules
    module Control.Proxy.Class,
    module Control.Proxy.Synonym,
    module Control.Proxy.Trans,
    module Control.Proxy.Trans.Identity,
    module Control.Proxy.Prelude,
    module Data.Closed
    ) where

import Control.Proxy.Class
import Control.Proxy.Synonym
import Control.Proxy.Trans
import Control.Proxy.Trans.Identity
import Control.Proxy.Prelude
import Data.Closed

{- $modules
    "Control.Proxy.Class" defines the 'ProxyP' type class so that you can
    program generically over proxy implementations and their transformers.

    "Control.Proxy.Synonym" defines type synonyms for proxies that don't use all
    of their inputs or outputs, such as 'Pipe's, 'Producer's, and 'Server's.

    "Control.Proxy.Trans" defines the proxy transformer type class that lets you
    write your own proxy extensions.

    "Control.Proxy.Prelude" provides a standard library of proxies.

    Consult "Control.Proxy.Tutorial" for an extended tutorial.
-}
