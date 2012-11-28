{-| This modules imports other useful supporting modules from other libraries
    for people who desire shorter import lists.

    Advantages:

    * Convenient

    * Shortens import lists

    * Great for @ghci@ demos

    Disadvantages:

    * Harder for people to track down the origin of imported functions

    * More tedious to resolve namespace collisions

    * Leads to significant scope creep for this module

    I recommend that you use "Control.Proxy" as a stable import for production
    code.  This convenience module is not very stable and may grow or shrink in
    exports depending on user feedback.  This module is primarily for rapid
    prototyping and demonstrations. -}
module Control.Proxy.Extra (
    -- $modules
    module Control.MFunctor,
    module Control.Monad,
    module Control.Monad.Trans.Class,
    module Control.Monad.Trans.Writer.Strict,
    module Control.Proxy
    ) where

import Control.MFunctor
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
import Control.Proxy
import Data.Monoid

{- $modules
    "Control.Proxy" includes all recommended functionality from this library.

    @Control.Monad@ provides 'Monad' utilities, since all proxies are monads.

    @Control.Monad.Trans.Class@ provides 'MonadTrans', since all proxies
    are monad transformers.

    @Control.Monad.Trans.Writer.Strict@ provides the 'WriterT' monad
    transformer, which all proxy folds use for accumulations.

    @Data.Monoid@ lets you access the result of 'WriterT' folds.

    "Control.MFunctor" includes the 'MFunctor' class for mixing proxies with
    different base monads. -}
