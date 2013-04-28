-- | Default imports for the "Control.Proxy" hierarchy

module Control.Proxy.Core (
    -- * Modules
    -- $modules
    module Control.Proxy.Class,
    module Control.Proxy.Prelude,
    module Control.Proxy.Trans,
    module Control.Proxy.Trans.Identity,
    module Control.Proxy.ListT,
    module Control.Proxy.Morph,
    module Control.Monad,
    module Control.Monad.Trans.Class,
    module Control.Monad.Morph,
    ) where

import Control.Monad.Morph (MFunctor(hoist), MMonad(embed))
import Control.Monad (forever, (>=>), (<=<))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class
import Control.Proxy.ListT
import Control.Proxy.Morph
import Control.Proxy.Trans
import Control.Proxy.Trans.Identity
import Control.Proxy.Prelude

{- $modules
    "Control.Proxy.Class" defines the 'Proxy' type class that lets you program
    generically over proxy implementations and their transformers.

    "Control.Proxy.Synonym" defines type synonyms for proxies that don't use all
    of their inputs or outputs, such as 'Pipe's, 'Producer's, and 'Server's.

    "Control.Proxy.Prelude" provides a standard library of proxies.

    "Control.Proxy.Trans" defines the 'ProxyTrans' type class that lets you
    write your own proxy extensions.

    "Control.Proxy.Trans.Identity" exports 'runIdentityP', which substantially
    eases writing completely polymorphic proxies.

    "Control.Proxy.ListT" defines a generalized @ListT@ monad transformer that
    can interconvert with proxies.

    "Control.Proxy.Morph" exports 'hoistP'.

    "Control.Monad" exports 'forever', ('>=>'), and ('<=<').

    "Control.Monad.Trans.Class" exports 'lift'.

    "Control.Monad.Morph" exports 'hoist'.
-}
