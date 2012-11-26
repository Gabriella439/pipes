-- | Recommended entry import for this library

module Control.Proxy (
    -- * Modules
    -- $default
    module Control.Proxy.Core,
    module Control.Proxy.Core.Fast
    ) where

import Control.Proxy.Core
import Control.Proxy.Core.Fast hiding (Request, Respond, M, Pure)

{- $default
    Read "Control.Proxy.Tutorial" for an extended proxy tutorial.

    "Control.Proxy.Core" exports everything except 'runProxy'.

    This library provides two base proxy implementations, each of which export
    their own 'runProxy' function:

    * "Control.Proxy.Core.Fast": This runs faster for code that is not
      'IO'-bound, but it only obeys the monad transformer laws modulo safe
      observation functions.

    * "Control.Proxy.Core.Correct": This trades speed on pure code segments, but
       strictly preserves the monad transformer laws.

    This module selects the currently recommended implementation (Fast).

    You can switch to the correct implementation by importing
    "Control.Proxy.Core" and "Control.Proxy.Core.Correct".

    You can lock in the fast implementation (in case I change the recommended
    default) by importing "Control.Proxy.Core" and "Control.Proxy.Core.Fast".
-}
