-- | Default imports for the "Control.Proxy" hierarchy

{-# LANGUAGE Safe #-}

module Control.Proxy.Core (
    -- * Modules
    -- $modules
    module Control.Proxy.Class,
    module Control.Proxy.Synonym,
    module Control.Proxy.Prelude,
    module Control.Proxy.Trans,
    module Control.Proxy.Trans.Identity,
    module Control.Monad,
    module Control.Monad.Trans.Class,
    module Control.MFunctor,
    module Control.PFunctor
    ) where

import Control.MFunctor
import Control.Monad (forever, (>=>), (<=<))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class
import Control.Proxy.Synonym
import Control.Proxy.Trans
import Control.Proxy.Trans.Identity
import Control.Proxy.Prelude
import Control.PFunctor

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

    "Control.Monad" exports 'forever', ('>=>'), and ('<=<').

    "Control.Monad.Trans.Class" exports 'lift'.

    "Control.MFunctor" exports 'hoist'.

    "Control.PFunctor" exports 'hoistP'.
-}
