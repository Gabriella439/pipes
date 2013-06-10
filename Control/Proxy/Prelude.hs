-- | General purpose proxies

{-# LANGUAGE Rank2Types #-}

module Control.Proxy.Prelude (
    -- * I/O
    stdinS,
    readLnS,
    hGetLineS,
    stdoutC,
    hPutStrLnC,
    printC,

    -- * Maps
    mapD,
    mapMD,
    useD,
    execD,
    execU,

    -- * Filters
    takeB_,
    takeWhileD,
    dropD,
    dropWhileD,
    filterD,

    -- * Lists and Enumerations
    fromListS,
    enumFromS,
    eachS,

    -- * Folds
    foldC,
    allC,
    allC_,
    anyC,
    anyC_,
    sumC,
    productC,
    lengthC,
    headC,
    headC_,
    lastC,
    toListC,
    foldrC,
    foldlC',

    -- * ArrowChoice
    -- $choice
    leftD,
    rightD,

    -- * Zips and Merges
    zipD,
    mergeD,

    -- * Kleisli utilities
    foreverK,

    -- * Adapters
    unitD,
    unitU,
    forward,
    generalize,

    -- * Re-exports
    -- $modules
    module Data.Monoid,
    ) where

import Control.Monad (forever, replicateM_)
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class
import Control.Proxy.Morph (PFunctor(hoistP))
import Control.Proxy.Trans (ProxyTrans(liftP))
import Control.Proxy.Trans.Identity (IdentityP(IdentityP, runIdentityP))
import Control.Proxy.Trans.State (StateP(StateP), evalStateP, get, put)
import Control.Proxy.Trans.Writer (WriterP, tell)
import qualified Data.Monoid as M
import Data.Monoid (
    appEndo, getAll, getAny, getSum, getProduct, getFirst, getLast )
import qualified System.IO as IO

-- | Produces lines, using 'getLine'
stdinS :: (Proxy p) => () -> Producer p String IO r
stdinS () = runIdentityP $ forever $ do
    str <- lift getLine
    respond str
{-# INLINABLE stdinS #-}

-- | 'read' input from 'stdin' one line at a time and send \'@D@\'ownstream
readLnS :: (Read b, Proxy p) => () -> Producer p b IO r
readLnS () = runIdentityP $ forever $ do
    a <- lift readLn
    respond a
{-# INLINABLE readLnS #-}

-- | A 'Producer' that sends lines from a handle downstream
hGetLineS :: (Proxy p) => IO.Handle -> () -> Producer p String IO ()
hGetLineS h () = runIdentityP go where
    go = do
        eof <- lift $ IO.hIsEOF h
        if eof
            then return ()
            else do
                str <- lift $ IO.hGetLine h
                respond str
                go
{-# INLINABLE hGetLineS #-}

-- | Consumes 'String's, using 'putStrLn'
stdoutC :: (Proxy p) => () -> Consumer p String IO r
stdoutC () = runIdentityP $ forever $ do
    str <- request ()
    lift $ putStrLn str
{-# INLINABLE stdoutC #-}

-- | Consumes 'String's using 'hPutStrLn'
hPutStrLnC :: (Proxy p) => IO.Handle -> () -> Consumer p String IO r
hPutStrLnC handle () = runIdentityP $ forever $ do
    str <- request ()
    lift $ IO.hPutStrLn handle str
{-# INLINABLE hPutStrLnC #-}

-- | Consumes 'Show'able values using 'print'
printC :: (Show a, Proxy p) => () -> Consumer p a IO r
printC () = runIdentityP $ forever $ do
    a <- request ()
    lift $ print a
{-# INLINABLE printC #-}

{-| @(mapD f)@ applies @f@ to all values going \'@D@\'ownstream.

> mapD f1 >-> mapD f2 = mapD (f2 . f1)
>
> mapD id = pull
-}
mapD :: (Monad m, Proxy p) => (a -> b) -> () -> Pipe p a b m r
mapD f () = runIdentityP $ forever $ do
    a <- request ()
    respond (f a)
{-# INLINABLE mapD #-}

{-| @(mapMD f)@ applies the monadic function @f@ to all values going downstream

> mapMD f1 >-> mapMD f2 = mapMD (f1 >=> f2)
>
> mapMD return = pull
-}
mapMD :: (Monad m, Proxy p) => (a -> m b) -> () -> Pipe p a b m r
mapMD f () = runIdentityP $ forever $ do
    a <- request ()
    b <- lift $ f a
    respond b
{-# INLINABLE mapMD #-}

{-| @(useD f)@ executes the monadic function @f@ on all values flowing
    \'@D@\'ownstream, discarding the result

> useD f1 >-> useD f2 = useD (\a -> f1 a >> f2 a)
>
> useD (\_ -> return ()) = pull
-}
useD :: (Monad m, Proxy p) => (a -> m b) -> () -> Pipe p a a m r
useD f () = runIdentityP $ forever $ do
    a <- request ()
    _ <- lift (f a)
    respond a
    return ()
{-# INLINABLE useD #-}

{-| @(execD md)@ executes @md@ every time control flows downstream through it

> execD md1 >-> execD md2 = execD (md1 >> md2)
>
> execD (return ()) = pull
-}
execD :: (Monad m, Proxy p) => m b -> () -> Pipe p a a m r
execD md () = runIdentityP $ forever $ do
    a <- request ()
    _ <- lift md
    respond a
{-# INLINABLE execD #-}

{-| @(execU mu)@ executes @mu@ every time control flows upstream through it

> execU mu1 >-> execU mu2 = execU (mu2 >> mu1)
>
> execU (return ()) = pull
-}
execU :: (Monad m, Proxy p) => m b -> () -> Pipe p a a m r
execU mu () = runIdentityP $ forever $ do
    lift mu
    a <- request ()
    respond a
{-# INLINABLE execU #-}

-- | @(takeB_ n)@ only allows @n@ values to pass through
takeB_ :: (Monad m, Proxy p) => Int -> () -> Pipe p a a m ()
takeB_ n () = runIdentityP $ replicateM_ n $ do
    a <- request ()
    respond a
{-# INLINABLE takeB_ #-}

{-| @(takeWhileD p)@ allows values to pass downstream so long as they satisfy
    the predicate @p@.

> -- Using the "All" monoid over functions:
> mempty = \_ -> True
> (p1 <> p2) a = p1 a && p2 a
>
> takeWhileD p1 >-> takeWhileD p2 = takeWhileD (p1 <> p2)
>
> takeWhileD mempty = pull
-}
takeWhileD :: (Monad m, Proxy p) => (a -> Bool) -> () -> Pipe p a a m ()
takeWhileD predicate () = runIdentityP go where
    go = do
        a <- request ()
        if (predicate a)
            then do
                respond a
                go
            else return ()
{-# INLINABLE takeWhileD #-}

{-| @(dropD n)@ discards @n@ values going downstream

> dropD n1 >-> dropD n2 = dropD (n1 + n2)  -- n2 >= 0 && n2 >= 0
>
> dropD 0 = pull
-}
dropD :: (Monad m, Proxy p) => Int -> () -> Pipe p a a m r
dropD n () = runIdentityP $ do
    replicateM_ n $ request ()
    pull ()
{-# INLINABLE dropD #-}

{-| @(dropWhileD p)@ discards values going downstream until one violates the
    predicate @p@.

> -- Using the "Any" monoid over functions:
> mempty = \_ -> False
> (p1 <> p2) a = p1 a || p2 a
>
> dropWhileD p1 >-> dropWhileD p2 = dropWhileD (p1 <> p2)
>
> dropWhileD mempty = pull
-}
dropWhileD :: (Monad m, Proxy p) => (a -> Bool) -> () -> Pipe p a a m r
dropWhileD p () = runIdentityP go where
    go = do
        a <- request ()
        if (p a)
            then go
            else do
                respond a
                pull ()
{-# INLINABLE dropWhileD #-}

{-| @(filterD p)@ discards values going downstream if they fail the predicate
    @p@

> -- Using the "All" monoid over functions:
> mempty = \_ -> True
> (p1 <> p2) a = p1 a && p2 a
>
> filterD p1 >-> filterD p2 = filterD (p1 <> p2)
>
> filterD mempty = pull
-}
filterD :: (Monad m, Proxy p) => (a -> Bool) -> () -> Pipe p a a m r
filterD p () = runIdentityP go where
    go = do
        a <- request ()
        if (p a)
            then do
                respond a
                go
            else go
{-# INLINABLE filterD #-}

{-| Convert a list into a 'Producer'

> fromListS xs >=> fromListS ys = fromListS (xs ++ ys)
>
> fromListS [] = return
-}
fromListS :: (Monad m, Proxy p) => [b] -> () -> Producer p b m ()
fromListS xs () = runIdentityP $ mapM_ respond xs
{-# INLINABLE fromListS #-}

-- | 'Producer' version of 'enumFrom'
enumFromS :: (Enum b, Monad m, Proxy p) => b -> () -> Producer p b m r
enumFromS b0 = \_ -> runIdentityP (go b0) where
    go b = do
        _ <- respond b
        go $! succ b
{-# INLINABLE enumFromS #-}

{-| Non-deterministically choose from all values in the given list

> mappend <$> eachS xs <*> eachS ys = eachS (mappend <$> xs <*> ys)
>
> eachS (pure mempty) = pure mempty
-}
eachS :: (Monad m, Proxy p) => [b] -> ListT p m b
eachS bs = RespondT (fromListS bs ())
{-# INLINABLE eachS #-}

-- | Strict fold using the provided 'M.Monoid'
foldC
    :: (M.Monoid w, Monad m, Proxy p)
    => (a -> w) -> () -> Consumer (WriterP w p) a m r
foldC f () =  forever $ do
    a <- request ()
    tell (f a)
{-# INLINABLE foldC #-}

-- | Fold that returns whether 'M.All' values satisfy the predicate
allC
    :: (Monad m, Proxy p)
    => (a -> Bool) -> () -> Consumer (WriterP M.All p) a m r
allC predicate = foldC (M.All . predicate)
{-# INLINABLE allC #-}

{-| Fold that returns whether 'M.All' values satisfy the predicate

    'allC_' terminates on the first value that fails the predicate
-}
allC_
    :: (Monad m, Proxy p)
    => (a -> Bool) -> () -> Consumer (WriterP M.All p) a m ()
allC_ predicate () = go
  where
    go = do
        a <- request ()
        if (predicate a)
            then go
            else tell (M.All False)
{-# INLINABLE allC_ #-}

-- | Fold that returns whether 'M.Any' value satisfies the predicate
anyC
    :: (Monad m, Proxy p)
    => (a -> Bool) -> () -> Consumer (WriterP M.Any p) a m r
anyC predicate = foldC (M.Any . predicate)
{-# INLINABLE anyC #-}

{-| Fold that returns whether 'M.Any' value satisfies the predicate

    'anyC_' terminates on the first value that satisfies the predicate
-}
anyC_
    :: (Monad m, Proxy p)
    => (a -> Bool) -> () -> Consumer (WriterP M.Any p) a m ()
anyC_ predicate () = go where
    go = do
        a <- request ()
        if (predicate a)
            then tell (M.Any True)
            else go
{-# INLINABLE anyC_ #-}

-- | Compute the 'M.Sum' of all values
sumC :: (Monad m, Proxy p, Num a) => () -> Consumer (WriterP (M.Sum a) p) a m r
sumC = foldC M.Sum
{-# INLINABLE sumC #-}

-- | Compute the 'M.Product' of all values
productC
    :: (Monad m, Proxy p, Num a)
    => () -> Consumer (WriterP (M.Product a) p) a m r
productC = foldC M.Product
{-# INLINABLE productC #-}

-- | Count the number of values
lengthC :: (Monad m, Proxy p) => () -> Consumer (WriterP (M.Sum Int) p) a m r
lengthC = foldC (\_ -> M.Sum 1)
{-# INLINABLE lengthC #-}

-- | Retrieve the 'M.First' value
headC :: (Monad m, Proxy p) => () -> Consumer (WriterP (M.First a) p) a m r
headC = foldC (M.First . Just)
{-# INLINABLE headC #-}

{-| Retrieve the 'M.First' value

    'headC_' terminates on the first value it receives
-}
headC_ :: (Monad m, Proxy p) => () -> Consumer (WriterP (M.First a) p) a m ()
headC_ () = do
    a <- request ()
    tell $ M.First (Just a)
{-# INLINABLE headC_ #-}

-- | Retrieve the last value
lastC :: (Monad m, Proxy p) => () -> Consumer (WriterP (M.Last a) p) a m r
lastC = foldC (M.Last . Just)
{-# INLINABLE lastC #-}

-- | Fold the values flowing \'@D@\'ownstream into a list
toListC :: (Monad m, Proxy p) => () -> Consumer (WriterP [a] p) a m r
toListC = foldC (\x -> [x])
{-# INLINABLE toListC #-}

{-| Fold equivalent to 'foldr'

    To see why, consider this isomorphic type for 'foldr':

> foldr :: (a -> b -> b) -> [a] -> M.Endo b
-}
foldrC
    :: (Monad m, Proxy p)
    => (a -> b -> b) -> () -> Consumer (WriterP (M.Endo b) p) a m r
foldrC step = foldC (M.Endo . step)
{-# INLINABLE foldrC #-}

-- | Fold equivalent to 'foldl''.
--
-- Uses 'StateP' instead of 'WriterP' to ensure a strict accumulation
foldlC'
    :: (Monad m, Proxy p) => (s -> a -> s) -> () -> Consumer (StateP s p) a m r
foldlC' step () = go where
    go = do
        a <- request ()
        s <- get
        put $! step s a 
        go
{-# INLINABLE foldlC' #-}

{- $choice
    'leftD' and 'rightD' satisfy the 'ArrowChoice' laws using @arr = mapD@.
-}

{-| Lift a proxy to operate only on 'Left' values flowing \'@D@\'ownstream and
    forward 'Right' values
-}
leftD
    :: (Monad m, Proxy p)
    => (q -> p x a x b m r) -> (q -> p x (Either a e) x (Either b e) m r)
leftD k = runIdentityP . (up \>\ (IdentityP . k />/ dn))
  where
    dn b = respond (Left b)
    up x = do
        ma <- request x
        case ma of
            Left  a -> return a
            Right e -> do
                x2 <- respond (Right e)
                up x2
{-# INLINABLE leftD #-}

{-| Lift a proxy to operate only on 'Right' values flowing \'@D@\'ownstream and
    forward 'Left' values
-}
rightD
    :: (Monad m, Proxy p)
    => (q -> p x a x b m r) -> (q -> p x (Either e a) x (Either e b) m r)
rightD k = runIdentityP . (up \>\ (IdentityP . k />/ dn))
  where
    dn b = respond (Right b)
    up x = do
        ma <- request x
        case ma of
            Left  e -> do
                x2 <- respond (Left e)
                up x2
            Right a -> return a
{-# INLINABLE rightD #-}

-- | Zip values flowing downstream
zipD
    :: (Monad m, Proxy p1, Proxy p2, Proxy p3)
    => () -> Consumer' p1 a (Consumer' p2 b (Producer' p3 (a, b) m)) r
zipD () = runIdentityP $ hoist (runIdentityP . hoist runIdentityP) go where
    go = do
        a <- request ()
        lift $ do
            b <- request ()
            lift $ respond (a, b)
        go
{-# INLINABLE zipD #-}

-- | Interleave values flowing downstream using simple alternation
mergeD
    :: (Monad m, Proxy p1, Proxy p2, Proxy p3)
    => () -> Consumer' p1 a (Consumer' p2 a (Producer' p3 a m)) r
mergeD () = runIdentityP $ hoist (runIdentityP . hoist runIdentityP) go where
    go = do
        a1 <- request ()
        lift $ do
            lift $ respond a1
            a2 <- request ()
            lift $ respond a2
        go
{-# INLINABLE mergeD #-}

{-| Compose a \'@K@\'leisli arrow with itself forever

    Use 'foreverK' to abstract away the following common recursion pattern:

> p a = do
>     ...
>     a' <- respond b
>     p a'

    Using 'foreverK', you can instead write:

> p = foreverK $ \a -> do
>     ...
>     respond b
-}
foreverK :: (Monad m) => (a -> m a) -> (a -> m b)
foreverK k = let r = \a -> k a >>= r in r
{- foreverK uses 'let' to avoid a space leak.
   See: http://hackage.haskell.org/trac/ghc/ticket/5205
-}
{-# INLINABLE foreverK #-}

-- | Discards all values going upstream
unitD :: (Monad m, Proxy p) => q -> p x' x y' () m r
unitD _ = runIdentityP go where
    go = do
        _ <- respond ()
        go
{-# INLINABLE unitD #-}

-- | Discards all values going downstream
unitU :: (Monad m, Proxy p) => q -> p () x y' y m r
unitU _ = runIdentityP go where
    go = do
        _ <- request ()
        go
{-# INLINABLE unitU #-}

{-| Transform a 'Consumer' to a 'Pipe' that reforwards all values further
    downstream
-}
forward
    :: (Monad m, Proxy p)
    => (() -> Consumer p a   m r)
    -> (() -> Pipe     p a a m r)
forward p () = evalStateP Nothing $ do
    r <- up >\\ liftP (p ())
    ma <- get
    case ma of
        Nothing -> return ()
        Just a  -> respond a
    return r
  where
    up () = do
        ma <- get
        case ma of
            Nothing -> return ()
            Just a  -> respond a
        a <- request ()
        put (Just a)
        return a
{-# INLINABLE forward #-}

-- | Transform a unidirectional 'Pipe' to a bidirectional 'Pipe'
generalize :: (Monad m, Proxy p) => (() -> Pipe p a b m r) -> x -> p x a x b m r
generalize p x = evalStateP x $ up >\\ liftP (p ()) //> dn
  where
    up () = do
        x <- get
        request x
    dn a = do
        x <- respond a
        put x
{-# INLINABLE generalize #-}

{- $modules
    @Data.Monoid@ re-exports unwrapping functions for monoids in order to
    extract the results of folds.
-}
