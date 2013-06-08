-- | Default imports for the "Control.Proxy" hierarchy

module Control.Proxy.Core (
    -- * Modules
    -- $modules
    module Control.Proxy.Class,
    module Control.Proxy.Prelude,
    module Control.Proxy.Trans,
    module Control.Proxy.Trans.Identity,
    module Control.Proxy.Trans.Codensity,
    module Control.Proxy.Trans.Either,
    module Control.Proxy.Trans.Maybe,
    module Control.Proxy.Trans.Reader,
    module Control.Proxy.Trans.State,
    module Control.Proxy.Trans.Writer,
    module Control.Proxy.Morph,
    module Control.Monad,
    module Control.Monad.Trans.Class,
    module Control.Monad.Morph,
    ) where

import Control.Monad (forever, (>=>), (<=<))
import Control.Monad.Morph (MFunctor(hoist), MMonad(embed))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class
import Control.Proxy.Morph
import Control.Proxy.Prelude
import Control.Proxy.Trans
import Control.Proxy.Trans.Identity
import Control.Proxy.Trans.Codensity (runCodensityP, runCodensityK)
import Control.Proxy.Trans.Either (runEitherP, runEitherK)
import Control.Proxy.Trans.Maybe (runMaybeP, runMaybeK)
import Control.Proxy.Trans.Reader (runReaderP, runReaderK)
import Control.Proxy.Trans.State (
    runStateP, runStateK, evalStateP, evalStateK, execStateP, execStateK )
import Control.Proxy.Trans.Writer (
    runWriterP, runWriterK, execWriterP, execWriterK )

{- $modules
    "Control.Proxy.Class" defines the 'Proxy' type class that lets you program
    generically over proxy implementations and their transformers.

    "Control.Proxy.Prelude" provides a standard library of proxies.

    "Control.Proxy.Trans" defines the 'ProxyTrans' type class that lets you
    write your own proxy extensions.

    "Control.Proxy.Trans.Identity" exports 'runIdentityP', which substantially
    eases writing completely polymorphic proxies.

    The remaining @Control.Proxy.Trans.*@ modules export run functions for every
    proxy transformer.

    "Control.Proxy.Morph" exports 'hoistP'.

    "Control.Monad" exports 'forever', ('>=>'), and ('<=<').

    "Control.Monad.Trans.Class" exports 'lift'.

    "Control.Monad.Morph" exports 'hoist'.
-}
