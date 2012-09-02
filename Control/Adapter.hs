{-| 'Adapter' generalizes the 'Pipe' type by allowing requests to be
    parametrized by an argument -}

module Control.Adapter.Core (
    -- * Types
    AdapterF(..),
    Adapter,
    Closure,
    Opening,
    Program,
    -- * Build Adapters
    need,
    give,
    -- * Compose Adapters
    (<-<),
    (>->),
    idA,
    -- * Run Programs
    runProgram,
    -- * Utility functions
    discard,
    ignore,
    foreverK
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Void

-- Imports for documentation
import Control.Category
import Prelude hiding ((.), id)
import Control.Pipe (Pipe)

-- | The base functor for the 'Adapter' type
data AdapterF a' a b' b x = Need a' (a -> x) | Give b (b' -> x)

instance Functor (AdapterF a' a b' b) where
    fmap f (Give b  fb') = Give b  (f . fb')
    fmap f (Need a' fa ) = Need a' (f . fa )

{- $adapter
    An 'Adapter' wraps an underlying interface with a new exposed interface

    The type variables of @Adapter a' a b' b m r@ signify:

    * @a'@ - The argument for the underlying interface

    * @a @ - The return value of the underlying interface

    * @b'@ - The argument for the exposed interface

    * @b @ - The return value of the exposed interface

    * @m @ - The base monad

    * @r @ - The final return value -}

-- | An 'Adapter' converts one interface to another
type Adapter a' a b' b = FreeT (AdapterF a' a b' b)

{-| @Closure arg ret@ accepts values of type @arg@ and responds with values of
    type @ret@ -}
type Closure arg ret = Adapter Void  () arg  ret

-- | @Opening arg ret@ consumes a closure of type @Closure arg ret@.
type Opening arg ret = Adapter  arg ret  () Void

-- | A self-contained 'Program', ready to be run by 'runProgram'
type Program         = Adapter Void  ()  () Void

{-| Request input from upstream, passing a parameter to upstream

    @need a'@ passes @a'@ as a parameter to upstream that upstream can use to
    decide what response to return.  'need' binds upstream's response to the
    return value. -}
need :: (Monad m) => a' -> Adapter a' a b' b m a
need a' = liftF $ Need a' id

{-| Provide output downstream

    @give b@ responds to a 'need' request by supplying the value @b@.  'give'
    blocks until downstream uses 'need' to request a new value, binding the
    parameter supplied by 'need' as the return value. -}
give :: (Monad m) => b  -> Adapter a' a b' b m b'
give b  = liftF $ Give b  id

infixr 9 <-<
infixl 9 >->

{-| Compose two adapters, satisfying all 'need' requests from downstream with
    'give' responses from upstream

    Corresponds to ('<<<') from @Control.Category@ -}
(<-<) :: (Monad m)
 => (c' -> Adapter b' b c' c m r)
 -> (b' -> Adapter a' a b' b m r)
 -> (c' -> Adapter a' a c' c m r)
p1 <-< p2 = \c' -> FreeT $ do
    x1 <- runFreeT $ p1 c'
    runFreeT $ case x1 of
        Pure           r   -> return r
        Free (Give c  fc') -> wrap $ Give c (fc' <-< p2)
        Free (Need b' fb ) -> FreeT $ do
            x2 <- runFreeT $ p2 b'
            runFreeT $ case x2 of
                Pure           r   -> return r
                Free (Give b  fb') -> ((\_ -> fb b) <-< fb') c'
                Free (Need a' fa ) -> do
                    let p1' = \_ -> FreeT $ return x1
                    wrap $ Need a' $ \a -> (p1' <-< (\_ -> fa a)) c'

{-| Compose two adapters, satisfying all 'need' requests from downstream with
    'give' responses from upstream

    Corresponds to ('>>>') from @Control.Category@ -}
(>->) :: (Monad m)
 => (b' -> Adapter a' a b' b m r)
 -> (c' -> Adapter b' b c' c m r)
 -> (c' -> Adapter a' a c' c m r)
(>->) = flip (<-<)

{-| Trivial adapter that does not change interface at all

    'idA' passes all 'need' requests further upstream, and passes all 'give'
    requests further downstream.

    Corresponds to 'id' from @Control.Category@ -}
idA :: (Monad m) => a' -> Adapter a' a a' a m r
idA = \a' -> wrap $ Need a' $ \a -> wrap $ Give a idA
-- i.e. idA = foreverK $ need >=> give

{- $run
    'runProgram' ensures that the 'Adapter' passed to it does not have any
    unsatisfied 'give' or 'need' statements.

    This restriction makes 'runProgram' less polymorphic than it could be, and
    I settled on this restriction for three reasons:

    * It prevents against accidental data loss.

    * It protects against silent failures

    * It prevents wastefully draining a scarce resource by gratuitously
      driving it to completion

    * It encourages an idiomatic programming style where unfulfilled 'need' or
      'give' statements are satisfied in a structured way using composition.

    If you believe that loose 'need' or 'give' statements should be discarded or
    ignored, then you can explicitly ignore them by using 'discard' (which
    discards all input), and 'ignore' (which ignores all requests):

> runProgram $ discard <-< p <-< ignore
-}
-- | Run a self-contained 'Program', converting it back to the base monad
runProgram :: (Monad m) => (() -> Program m r) -> m r
runProgram p = runProgram' $ p ()

runProgram' p = do
    x <- runFreeT p
    case x of
        Pure          r    -> return r
        Free (Give _ fb ) -> runProgram' $ fb  ()
        Free (Need _ fa') -> runProgram' $ fa' ()

-- | Discard all responses
discard :: (Monad m) => () -> Opening () a m r
discard () = forever $ need ()

-- | Ignore all requests
ignore  :: (Monad m) => a -> Closure a () m r
ignore  _  = forever $ give ()

-- | Compose a \'K\'leisli arrow with itself forever
foreverK :: (Monad m) => (a -> m a) -> (a -> m b)
foreverK k = let r = k >=> r in r
{- foreverK uses 'let' to avoid a space leak.
   See: http://hackage.haskell.org/trac/ghc/ticket/5205 -}
