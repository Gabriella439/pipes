-- | Recommended entry import for this library

module Control.Proxy (
    -- * Modules
    -- $default
    module Control.Proxy.Fast
    ) where

import Control.Proxy.Fast

{- $default
    This library defines two 'Proxy' implementations:

    * "Control.Proxy.Core.Fast": This module runs faster for code that is not
      'IO' bound, but it only obeys the monad transformer laws modulo safe
      observation functions (like 'runProxy').

    * "Control.Proxy.Core.Correct": This module sacrifices speed on pure code
      segments, but in return strictly preserves the monad transformer laws.

    These two modules only export the 'runProxy' family of functions which
    select which 'Proxy' implementation to use.  All other code in this library
    is completely polymorphic over the 'ProxyP' class and will work
    transparently with either implementation (or any transformers thereof).

    This module exports the current recommended implementation for users that
    seek guidance and choose to defer to my judgement on which underlying
    implementation to use.  You can safely import this and not worry that the
    API will break if I switch the default recommended implementation.

    Also, if you are just defining proxy utilities that are polymorphic over
    'ProxyP' and you don't use 'runProxy', then just use this module since
    neither implementation import will affect your code at all.

    If you do use 'runProxy' and you want to explicitly specify which
    implementation to use, then import "Control.Proxy.Fast" or
    "Control.Proxy.Correct" to lock in that particular implementation.

    I currently recommend "Control.Proxy.Fast", since most users do not require
    anything beyond the safe and polymorphic API. -}
