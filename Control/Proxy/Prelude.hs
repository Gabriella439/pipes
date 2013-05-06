-- | General purpose proxies

{-# LANGUAGE Rank2Types #-}

module Control.Proxy.Prelude (
    -- * I/O
    stdinS,
    stdoutD,
    readLnS,
    hGetLineS,
    hPutStrLnD,
    printD,
    printU,
    printB,

    -- * Maps
    mapD,
    mapU,
    mapMD,
    mapMU,
    useD,
    useU,
    execD,
    execU,

    -- * Filters
    takeB,
    takeB_,
    takeWhileD,
    takeWhileU,
    dropD,
    dropU,
    dropWhileD,
    dropWhileU,
    filterD,
    filterU,

    -- * Lists and Enumerations
    fromListS,
    enumFromS,
    enumFromToS,
    eachS,
    rangeS,

    -- * Folds
    foldD,
    allD,
    allD_,
    anyD,
    anyD_,
    sumD,
    productD,
    lengthD,
    headD,
    headD_,
    lastD,
    toListD,
    foldrD,

    -- * ArrowChoice
    -- $choice
    leftD,
    rightD,

    -- * Zips and Merges
    zipD,
    mergeD,

    -- * Closed Adapters
    -- $open
    unitD,
    unitU,

    -- * Kleisli utilities
    foreverK,

    -- * Re-exports
    module Data.Monoid,

    -- * Deprecated
    -- $deprecate
    mapB,
    mapMB,
    useB,
    execB,
    fromListC,
    enumFromC,
    enumFromToC,
    eachC,
    rangeC,
    getLineS,
    getLineC,
    readLnC,
    putStrLnD,
    putStrLnU,
    putStrLnB,
    hGetLineC,
    hPutStrLnU,
    hPutStrLnB,
    hPrintD,
    hPrintU,
    hPrintB,
    replicateK,
    liftK,
    hoistK,
    raise,
    raiseK,
    hoistPK,
    raiseP,
    raisePK
    ) where

import Control.Monad (forever)
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Proxy.Class
import Control.Proxy.Morph (PFunctor(hoistP))
import Control.Proxy.Trans (ProxyTrans(liftP))
import Control.Proxy.Trans.Identity (
    IdentityP(IdentityP, runIdentityP), runIdentityK)
import Control.Proxy.Trans.Writer (WriterP, tell)
import Data.Monoid (
    Monoid(mempty, mappend),
    Endo(Endo, appEndo),
    All(All, getAll),
    Any(Any, getAny),
    Sum(Sum, getSum),
    Product(Product, getProduct),
    First(First, getFirst),
    Last(Last, getLast) )
import qualified System.IO as IO

{-| A 'Producer' that sends lines from 'stdin' downstream

> stdinS = hGetLineS stdin
-}
stdinS :: (Proxy p) => () -> Producer p String IO r
stdinS () = runIdentityP $ forever $ do
    str <- lift getLine
    respond str
{-# INLINABLE stdinS #-}

{-| 'putStrLn's all values flowing \'@D@\'ownstream to 'stdout'

> stdoutD = hPutStrLnD stdout
-}
stdoutD :: (Proxy p) => x -> p x String x String IO r
stdoutD = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ putStrLn a
    respond a
{-# INLINABLE stdoutD #-}

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

-- | 'putStrLn's all values flowing \'@D@\'ownstream to a 'Handle'
hPutStrLnD :: (Proxy p) => IO.Handle -> x -> p x String x String IO r
hPutStrLnD h = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ IO.hPutStrLn h a
    respond a
{-# INLINABLE hPutStrLnD #-}

-- | 'print's all values flowing \'@D@\'ownstream to 'stdout'
printD :: (Show a, Proxy p) => x -> p x a x a IO r
printD = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ print a
    respond a
{-# INLINABLE printD #-}

-- | 'print's all values flowing \'@U@\'pstream to 'stdout'
printU :: (Show a', Proxy p) => a' -> p a' x a' x IO r
printU = runIdentityK $ foreverK $ \a' -> do
    lift $ print a'
    x <- request a'
    respond x
{-# INLINABLE printU #-}

{-| 'print's all values flowing through it to 'stdout'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
printB :: (Show a', Show a, Proxy p) => a' -> p a' a a' a IO r
printB = runIdentityK $ foreverK $ \a' -> do
    lift $ do
        putStr "U: "
        print a'
    a <- request a'
    lift $ do
        putStr "D: "
        print a
    respond a
{-# INLINABLE printB #-}

{-| @(mapD f)@ applies @f@ to all values going \'@D@\'ownstream.

> mapD f1 >-> mapD f2 = mapD (f2 . f1)
>
> mapD id = pull
-}
mapD :: (Monad m, Proxy p) => (a -> b) -> x -> p x a x b m r
mapD f = runIdentityK go where
    go x = do
        a  <- request x
        x2 <- respond (f a)
        go x2
-- mapD f = runIdentityK (foreverK $ request >=> respond . f)
{-# INLINABLE mapD #-}

{-| @(mapU g)@ applies @g@ to all values going \'@U@\'pstream.

> mapU g1 >-> mapU g2 = mapU (g1 . g2)
>
> mapU id = pull
-}
mapU :: (Monad m, Proxy p) => (b' -> a') -> b' -> p a' x b' x m r
mapU g = runIdentityK go where
    go b' = do
        x   <- request (g b')
        b'2 <- respond x
        go b'2
-- mapU g = foreverK $ (request . g) >=> respond
{-# INLINABLE mapU #-}

{-| @(mapMD f)@ applies the monadic function @f@ to all values going downstream

> mapMD f1 >-> mapMD f2 = mapMD (f1 >=> f2)
>
> mapMD return = pull
-}
mapMD :: (Monad m, Proxy p) => (a -> m b) -> x -> p x a x b m r
mapMD f = runIdentityK go where
    go x = do
        a  <- request x
        b  <- lift (f a)
        x2 <- respond b
        go x2
-- mapMD f = foreverK $ request >=> lift . f >=> respond
{-# INLINABLE mapMD #-}

{-| @(mapMU g)@ applies the monadic function @g@ to all values going upstream

> mapMU g1 >-> mapMU g2 = mapMU (g2 >=> g1)
>
> mapMU return = pull
-}
mapMU :: (Monad m, Proxy p) => (b' -> m a') -> b' -> p a' x b' x m r
mapMU g = runIdentityK go where
    go b' = do
        a'  <- lift (g b')
        x   <- request a'
        b'2 <- respond x
        go b'2
-- mapMU g = foreverK $ lift . g >=> request >=> respond
{-# INLINABLE mapMU #-}

{-| @(useD f)@ executes the monadic function @f@ on all values flowing
    \'@D@\'ownstream

> useD f1 >-> useD f2 = useD (\a -> f1 a >> f2 a)
>
> useD (\_ -> return ()) = pull
-}
useD :: (Monad m, Proxy p) => (a -> m r1) -> x -> p x a x a m r
useD f = runIdentityK go where
    go x = do
        a  <- request x
        _  <- lift $ f a
        x2 <- respond a
        go x2
{-# INLINABLE useD #-}

{-| @(useU g)@ executes the monadic function @g@ on all values flowing
    \'@U@\'pstream

> useU g1 >-> useU g2 = useU (\a' -> g2 a' >> g1 a')
>
> useU (\_ -> return ()) = pull
-}
useU :: (Monad m, Proxy p) => (a' -> m r2) -> a' -> p a' x a' x m r
useU g = runIdentityK go where
    go a' = do
        lift $ g a'
        x   <- request a'
        a'2 <- respond x
        go a'2
{-# INLINABLE useU #-}

{-| @(execD md)@ executes @md@ every time values flow downstream through it.

> execD md1 >-> execD md2 = execD (md1 >> md2)
>
> execD (return ()) = pull
-}
execD :: (Monad m, Proxy p) => m r1 -> a' -> p a' a a' a m r
execD md = runIdentityK go where
    go a' = do
        a   <- request a'
        _   <- lift md
        a'2 <- respond a
        go a'2
{- execD md = foreverK $ \a' -> do
    a <- request a'
    lift md
    respond a -}
{-# INLINABLE execD #-}

{-| @(execU mu)@ executes @mu@ every time values flow upstream through it.

> execU mu1 >-> execU mu2 = execU (mu2 >> mu1)
>
> execU (return ()) = pull
-}
execU :: (Monad m, Proxy p) => m r2 -> a' -> p a' a a' a m r
execU mu = runIdentityK go where
    go a' = do
        lift mu
        a   <- request a'
        a'2 <- respond a
        go a'2
{- execU mu = foreverK $ \a' -> do
    lift mu
    a <- request a'
    respond a -}
{-# INLINABLE execU #-}

{-| @(takeB n)@ allows @n@ upstream/downstream roundtrips to pass through

> takeB n1 >=> takeB n2 = takeB (n1 + n2)  -- n1 >= 0 && n2 >= 0
>
> takeB 0 = return
-}
takeB :: (Monad m, Proxy p) => Int -> a' -> p a' a a' a m a'
takeB n0 = runIdentityK (go n0) where
    go n
        | n <= 0    = return
        | otherwise = \a' -> do
             a   <- request a'
             a'2 <- respond a
             go (n - 1) a'2
-- takeB n = runIdentityK (replicateK n $ request >=> respond)
{-# INLINABLE takeB #-}

-- | 'takeB_' is 'takeB' with a @()@ return value, convenient for composing
takeB_ :: (Monad m, Proxy p) => Int -> a' -> p a' a a' a m ()
takeB_ n0 = runIdentityK (go n0) where
    go n
        | n <= 0    = \_ -> return ()
        | otherwise = \a' -> do
            a   <- request a'
            a'2 <- respond a
            go (n - 1) a'2
-- takeB_ n = fmap void (takeB n)
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
takeWhileD :: (Monad m, Proxy p) => (a -> Bool) -> a' -> p a' a a' a m ()
takeWhileD p = runIdentityK go where
    go a' = do
        a <- request a'
        if (p a)
            then do
                a'2 <- respond a
                go a'2
            else return ()
{-# INLINABLE takeWhileD #-}

{-| @(takeWhileU p)@ allows values to pass upstream so long as they satisfy the
    predicate @p@.

> takeWhileU p1 >-> takeWhileU p2 = takeWhileU (p1 <> p2)
>
> takeWhileD mempty = pull
-}
takeWhileU :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> p a' a a' a m ()
takeWhileU p = runIdentityK go where
    go a' =
        if (p a')
            then do
                a   <- request a'
                a'2 <- respond a
                go a'2
            else return_P ()
{-# INLINABLE takeWhileU #-}

{-| @(dropD n)@ discards @n@ values going downstream

> dropD n1 >-> dropD n2 = dropD (n1 + n2)  -- n2 >= 0 && n2 >= 0
>
> dropD 0 = pull
-}
dropD :: (Monad m, Proxy p) => Int -> () -> Pipe p a a m r
dropD n0 = \() -> runIdentityP (go n0) where
    go n
        | n <= 0    = pull ()
        | otherwise = do
            _ <- request ()
            go (n - 1)
{- dropD n () = do
    replicateM_ n $ request ()
    pull () -}
{-# INLINABLE dropD #-}

{-| @(dropU n)@ discards @n@ values going upstream

> dropU n1 >-> dropU n2 = dropU (n1 + n2)  -- n2 >= 0 && n2 >= 0
>
> dropU 0 = pull
-}
dropU :: (Monad m, Proxy p) => Int -> a' -> CoPipe p a' a' m r
dropU n0 = runIdentityK (go n0) where
    go n
        | n <= 0    = pull
        | otherwise = \_ -> do
            a' <- respond ()
            go (n - 1) a'
{-# INLINABLE dropU #-}

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
                x <- respond a
                pull x
{-# INLINABLE dropWhileD #-}

{-| @(dropWhileU p)@ discards values going upstream until one violates the
    predicate @p@.

> dropWhileU p1 >-> dropWhileU p2 = dropWhileU (p1 <> p2)
>
> dropWhileU mempty = pull
-}
dropWhileU :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> CoPipe p a' a' m r
dropWhileU p = runIdentityK go where
    go a' =
        if (p a')
            then do
                a2 <- respond ()
                go a2
            else pull a'
{-# INLINABLE dropWhileU #-}

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
filterD p = \() -> runIdentityP go where
    go = do
        a <- request ()
        if (p a)
            then do
                respond a
                go
            else go
{-# INLINABLE filterD #-}

{-| @(filterU p)@ discards values going upstream if they fail the predicate @p@

> filterU p1 >-> filterU p2 = filterU (p1 <> p2)
>
> filterU mempty = pull
-}
filterU :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> CoPipe p a' a' m r
filterU p = runIdentityK go where
    go a' =
        if (p a')
        then do
            request a'
            a'2 <- respond ()
            go a'2
        else do
            a'2 <- respond ()
            go a'2
{-# INLINABLE filterU #-}

{-| Convert a list into a 'Producer'

> fromListS xs >=> fromListS ys = fromListS (xs ++ ys)
>
> fromListS [] = return
-}
fromListS :: (Monad m, Proxy p) => [b] -> () -> Producer p b m ()
fromListS xs = \_ -> foldr (\e a -> respond e ?>= \_ -> a) (return_P ()) xs
-- fromListS xs _ = mapM_ respond xs
{-# INLINABLE fromListS #-}

-- | 'Producer' version of 'enumFrom'
enumFromS :: (Enum b, Monad m, Proxy p) => b -> () -> Producer p b m r
enumFromS b0 = \_ -> runIdentityP (go b0) where
    go b = do
        _ <- respond b
        go $! succ b
{-# INLINABLE enumFromS #-}

-- | 'Producer' version of 'enumFromTo'
enumFromToS
    :: (Enum b, Ord b, Monad m, Proxy p) => b -> b -> () -> Producer p b m ()
enumFromToS b1 b2 _ = runIdentityP (go b1) where
    go b
        | b > b2    = return ()
        | otherwise = do
            _ <- respond b
            go $! succ b
{-# INLINABLE enumFromToS #-}

{-| Non-deterministically choose from all values in the given list

> mappend <$> eachS xs <*> eachS ys = eachS (mappend <$> xs <*> ys)
>
> eachS (pure mempty) = pure mempty
-}
eachS :: (Monad m, Proxy p) => [b] -> ProduceT p m b
eachS bs = RespondT (fromListS bs ())
{-# INLINABLE eachS #-}

-- | Non-deterministically choose from all values in the given range
rangeS :: (Enum b, Ord b, Monad m, Proxy p) => b -> b -> ProduceT p m b
rangeS b1 b2 = RespondT (enumFromToS b1 b2 ())
{-# INLINABLE rangeS #-}

{-| Strict fold over values flowing \'@D@\'ownstream.

> foldD f >-> foldD g = foldD (f <> g)
>
> foldD mempty = idPull
-}
foldD
    :: (Monad m, Proxy p, Monoid w) => (a -> w) -> x -> WriterP w p x a x a m r
foldD f = go where
    go x = do
        a <- request x
        tell (f a)
        x2 <- respond a
        go x2
{-# INLINABLE foldD #-}

{-| Fold that returns whether 'All' values flowing \'@D@\'ownstream satisfy the
    predicate
-}
allD :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP All p x a x a m r
allD predicate = foldD (All . predicate)
{-# INLINABLE allD #-}

{-| Fold that returns whether 'All' values flowing \'@D@\'ownstream satisfy the
    predicate

    'allD_' terminates on the first value that fails the predicate
-}
allD_ :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP All p x a x a m ()
allD_ predicate = go where
    go x = do
        a <- request x
        if (predicate a)
            then do
                x2 <- respond a
                go x2
            else tell (All False)
{-# INLINABLE allD_ #-}

{-| Fold that returns whether 'Any' value flowing \'@D@\'ownstream satisfies the
    predicate
-}
anyD :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP Any p x a x a m r
anyD predicate = foldD (Any . predicate)
{-# INLINABLE anyD #-}

{-| Fold that returns whether 'Any' value flowing \'@D@\'ownstream satisfies the
    predicate

    'anyD_' terminates on the first value that satisfies the predicate
-}
anyD_ :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP Any p x a x a m ()
anyD_ predicate = go where
    go x = do
        a <- request x
        if (predicate a)
            then tell (Any True)
            else do
                x2 <- respond a
                go x2
{-# INLINABLE anyD_ #-}

-- | Compute the 'Sum' of all values that flow \'@D@\'ownstream
sumD :: (Monad m, Proxy p, Num a) => x -> WriterP (Sum a) p x a x a m r
sumD = foldD Sum
{-# INLINABLE sumD #-}

-- | Compute the 'Product' of all values that flow \'@D@\'ownstream
productD :: (Monad m, Proxy p, Num a) => x -> WriterP (Product a) p x a x a m r
productD = foldD Product
{-# INLINABLE productD #-}

-- | Count how many values flow \'@D@\'ownstream
lengthD :: (Monad m, Proxy p) => x -> WriterP (Sum Int) p x a x a m r
lengthD = foldD (\_ -> Sum 1)
{-# INLINABLE lengthD #-}

-- | Retrieve the first value going \'@D@\'ownstream
headD :: (Monad m, Proxy p) => x -> WriterP (First a) p x a x a m r
headD = foldD (First . Just)
{-# INLINABLE headD #-}

{-| Retrieve the first value going \'@D@\'ownstream

    'headD_' terminates on the first value it receives
-}
headD_ :: (Monad m, Proxy p) => x -> WriterP (First a) p x a x a m ()
headD_ x = do
    a <- request x
    tell $ First (Just a)
{-# INLINABLE headD_ #-}

-- | Retrieve the last value going \'@D@\'ownstream
lastD :: (Monad m, Proxy p) => x -> WriterP (Last a) p x a x a m r
lastD = foldD (Last . Just)
{-# INLINABLE lastD #-}

-- | Fold the values flowing \'@D@\'ownstream into a list
toListD :: (Monad m, Proxy p) => x -> WriterP [a] p x a x a m r
toListD = foldD (\x -> [x])
{-# INLINABLE toListD #-}

{-| Fold equivalent to 'foldr'

    To see why, consider this isomorphic type for 'foldr':

> foldr :: (a -> b -> b) -> [a] -> Endo b
-}
foldrD
    :: (Monad m, Proxy p)
    => (a -> b -> b) -> x -> WriterP (Endo b) p x a x a m r
foldrD step = foldD (Endo . step)
{-# INLINABLE foldrD #-}

{- $choice
    'leftD' and 'rightD' satisfy the 'ArrowChoice' laws using @arr = mapD@.
-}

{-| Lift a proxy to operate only on 'Left' values flowing \'@D@\'ownstream and
    forward 'Right' values
-}
leftD
    :: (Monad m, Proxy p)
    => (q -> p x a x b m r) -> (q -> p x (Either a e) x (Either b e) m r)
leftD k = runIdentityK (up \>\ (IdentityP . k />/ dn))
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
rightD k = runIdentityK (up \>\ (IdentityP . k />/ dn))
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
    => () -> Consumer p1 a (Consumer p2 b (Producer p3 (a, b) m)) r
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
    => () -> Consumer p1 a (Consumer p2 a (Producer p3 a m)) r
mergeD () = runIdentityP $ hoist (runIdentityP . hoist runIdentityP) go where
    go = do
        a1 <- request ()
        lift $ do
            lift $ respond a1
            a2 <- request ()
            lift $ respond a2
        go
{-# INLINABLE mergeD #-}

{- $open
    Use the @unit@ functions when you need to embed a proxy with a closed end
    within an open proxy.  For example, the following code will not type-check
    because @fromListS [1..]@  is a 'Producer' and has a closed upstream end,
    which conflicts with the 'request' statement preceding it:

> p () = do
>     request ()
>     fromList [1..] ()

    You fix this by composing 'unitD' upstream of it, which replaces its closed
    upstream end with an open polymorphic end:

> p () = do
>     request ()
>     (fromList [1..] <-< unitD) ()

-}

-- | Compose 'unitD' with a closed upstream end to create a polymorphic end
unitD :: (Monad m, Proxy p) => q -> p x' x y' () m r
unitD _ = runIdentityP go where
    go = do
        _ <- respond ()
        go
{-# INLINABLE unitD #-}

-- | Compose 'unitU' with a closed downstream end to create a polymorphic end
unitU :: (Monad m, Proxy p) => q -> p () x y' y m r
unitU _ = runIdentityP go where
    go = do
        _ <- request ()
        go
{-# INLINABLE unitU #-}

{- $modules
    These modules help you build, run, and extract folds
-}

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

{- $deprecate
    To be removed in version @4.0.0@
-}

mapB :: (Monad m, Proxy p) => (a -> b) -> (b' -> a') -> b' -> p a' a b' b m r
mapB f g = runIdentityK go where
    go b' = do
        a   <- request (g b')
        b'2 <- respond (f a )
        go b'2
{-# INLINABLE mapB #-}
{-# DEPRECATED mapB "Combine 'mapD' and 'mapU' instead" #-}

mapMB
    :: (Monad m, Proxy p) => (a -> m b) -> (b' -> m a') -> b' -> p a' a b' b m r
mapMB f g = runIdentityK go where
    go b' = do
        a'  <- lift (g b')
        a   <- request a'
        b   <- lift (f a )
        b'2 <- respond b
        go b'2
{-# INLINABLE mapMB #-}
{-# DEPRECATED mapMB "Combine 'mapMD' and 'mapMU' instead" #-}

useB
    :: (Monad m, Proxy p)
    => (a -> m r1) -> (a' -> m r2) -> a' -> p a' a a' a m r
useB f g = runIdentityK go where
    go a' = do
        lift $ g a'
        a   <- request a'
        lift $ f a
        a'2 <- respond a
        go a'2
{-# INLINABLE useB #-}
{-# DEPRECATED useB "Combined 'useD' and 'useU' instead" #-}

execB :: (Monad m, Proxy p) => m r1 -> m r2 -> a' -> p a' a a' a m r
execB md mu = runIdentityK go where
    go a' = do
        lift mu
        a   <- request a'
        lift md
        a'2 <- respond a
        go a'2
{-# INLINABLE execB #-}
{-# DEPRECATED execB "Combine 'execD' and 'execU' instead" #-}

fromListC :: (Monad m, Proxy p) => [a'] -> () -> CoProducer p a' m ()
fromListC xs = \_ -> foldr (\e a -> request e ?>= \_ -> a) (return_P ()) xs
-- fromListC xs _ = mapM_ request xs
{-# INLINABLE fromListC #-}
{-# DEPRECATED fromListC "Use 'turn . fromListS xs' instead" #-}

enumFromC :: (Enum a', Monad m, Proxy p) => a' -> () -> CoProducer p a' m r
enumFromC a'0 = \_ -> runIdentityP (go a'0) where
    go a' = do
        request a'
        go $! succ a'
{-# INLINABLE enumFromC #-}
{-# DEPRECATED enumFromC "Use 'turn . enumFromS n' instead" #-}

enumFromToC
    :: (Enum a', Ord a', Monad m, Proxy p)
    => a' -> a' -> () -> CoProducer p a' m ()
enumFromToC a1 a2 _ = runIdentityP (go a1) where
    go n
        | n > a2 = return ()
        | otherwise = do
            request n
            go $! succ n
{-# INLINABLE enumFromToC #-}
{-# DEPRECATED enumFromToC "Use 'turn . enumFromToS n1 n2' instead" #-}

eachC :: (Monad m, Proxy p) => [a'] -> CoProduceT p m a'
eachC a's = RequestT (fromListC a's ())
{-# INLINABLE eachC #-}
{-# DEPRECATED eachC "Use 'RequestT $ turn $ fromListS xs ()' instead" #-}

rangeC
    :: (Enum a', Ord a', Monad m, Proxy p) => a' -> a' -> CoProduceT p m a'
rangeC a'1 a'2 = RequestT (enumFromToC a'1 a'2 ())
{-# INLINABLE rangeC #-}
{-# DEPRECATED rangeC "Use 'RequestT $ turn $ enumFromToS n1 n2 ()' instead" #-}

getLineS :: (Proxy p) => () -> Producer p String IO r
getLineS () = runIdentityP $ forever $ do
    str <- lift getLine
    respond str
{-# INLINABLE getLineS #-}
{-# DEPRECATED getLineS "Use 'stdinS' instead" #-}

getLineC :: (Proxy p) => () -> CoProducer p String IO r
getLineC () = runIdentityP $ forever $ do
    str <- lift getLine
    request str
{-# INLINABLE getLineC #-}
{-# DEPRECATED getLineC "Use 'turn . stdinS' instead" #-}

readLnC :: (Read a', Proxy p) => () -> CoProducer p a' IO r
readLnC () = runIdentityP $ forever $ do
    a <- lift readLn
    request a
{-# INLINABLE readLnC #-}
{-# DEPRECATED readLnC "Use 'turn . readLnC' instead" #-}

putStrLnD :: (Proxy p) => x -> p x String x String IO r
putStrLnD = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ putStrLn a
    respond a
{-# INLINABLE putStrLnD #-}
{-# DEPRECATED putStrLnD "Use 'stdoutD' instead" #-}

putStrLnU :: (Proxy p) => String -> p String x String x IO r
putStrLnU = runIdentityK $ foreverK $ \a' -> do
    lift $ putStrLn a'
    x <- request a'
    respond x
{-# INLINABLE putStrLnU #-}
{-# DEPRECATED putStrLnU "Use 'execU putStrLn' instead" #-}

putStrLnB :: (Proxy p) => String -> p String String String String IO r
putStrLnB = runIdentityK $ foreverK $ \a' -> do
    lift $ do
        putStr "U: "
        putStrLn a'
    a <- request a'
    lift $ do
        putStr "D: "
        putStrLn a
    respond a
{-# INLINABLE putStrLnB #-}
{-# DEPRECATED putStrLnB "Not that useful" #-}

hGetLineC :: (Proxy p) => IO.Handle -> () -> CoProducer p String IO ()
hGetLineC h () = runIdentityP go where
    go = do
        eof <- lift $ IO.hIsEOF h
        if eof
            then return ()
            else do
                str <- lift $ IO.hGetLine h
                request str
                go
{-# INLINABLE hGetLineC #-}
{-# DEPRECATED hGetLineC "Use 'turn . hGetLineS h'" #-}

-- | 'print's all values flowing \'@D@\'ownstream to a 'Handle'
hPrintD :: (Show a, Proxy p) => IO.Handle -> x -> p x a x a IO r
hPrintD h = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ IO.hPrint h a
    respond a
{-# INLINABLE hPrintD #-}
{-# DEPRECATED hPrintD "Not that useful" #-}

-- | 'print's all values flowing \'@U@\'pstream to a 'Handle'
hPrintU :: (Show a', Proxy p) => IO.Handle -> a' -> p a' x a' x IO r
hPrintU h = runIdentityK $ foreverK $ \a' -> do
    lift $ IO.hPrint h a'
    x <- request a'
    respond x
{-# INLINABLE hPrintU #-}
{-# DEPRECATED hPrintU "Not that useful" #-}

hPrintB :: (Show a, Show a', Proxy p) => IO.Handle -> a' -> p a' a a' a IO r
hPrintB h = runIdentityK $ foreverK $ \a' -> do
    lift $ do
        IO.hPutStr h "U: "
        IO.hPrint h a'
    a <- request a'
    lift $ do
        IO.hPutStr h "D: "
        IO.hPrint h a
    respond a
{-# INLINABLE hPrintB #-}
{-# DEPRECATED hPrintB "Not that useful" #-}

hPutStrLnU :: (Proxy p) => IO.Handle -> String -> p String x String x IO r
hPutStrLnU h = runIdentityK $ foreverK $ \a' -> do
    lift $ IO.hPutStrLn h a'
    x <- request a'
    respond x
{-# INLINABLE hPutStrLnU #-}
{-# DEPRECATED hPutStrLnU "Not that useful" #-}

hPutStrLnB
    :: (Proxy p) => IO.Handle -> String -> p String String String String IO r
hPutStrLnB h = runIdentityK $ foreverK $ \a' -> do
    lift $ do
        IO.hPutStr h "U: "
        IO.hPutStrLn h a'
    a <- request a'
    lift $ do
        IO.hPutStr h "D: "
        IO.hPutStrLn h a
    respond a
{-# INLINABLE hPutStrLnB #-}
{-# DEPRECATED hPutStrLnB "Not that useful" #-}

replicateK :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
replicateK n0 k = go n0 where
    go n
        | n < 1     = return
        | n == 1    = k
        | otherwise = \a -> k a >>= go (n - 1)
{-# INLINABLE replicateK #-}
{-# DEPRECATED replicateK "Not very useful" #-}

liftK :: (Monad m, MonadTrans t) => (a -> m b) -> (a -> t m b)
liftK k a = lift (k a)
{-# INLINABLE liftK #-}
{-# DEPRECATED liftK "Use '(lift .)' instead" #-}

hoistK
    :: (Monad m, MFunctor t)
    => (forall a . m a -> n a)  -- ^ Monad morphism
    -> (b' -> t m b)            -- ^ Kleisli arrow
    -> (b' -> t n b)
hoistK k p a' = hoist k (p a')
{-# INLINABLE hoistK #-}
{-# DEPRECATED hoistK "Use '(hoist f .)' instead" #-}

raise :: (Monad m, MFunctor t1, MonadTrans t2) => t1 m r -> t1 (t2 m) r
raise = hoist lift
{-# INLINABLE raise #-}
{-# DEPRECATED raise "Use 'hoist lift' instead" #-}

raiseK
    :: (Monad m, MFunctor t1, MonadTrans t2)
    => (q -> t1 m r) -> (q -> t1 (t2 m) r)
raiseK = (hoist lift .)
{-# INLINABLE raiseK #-}
{-# DEPRECATED raiseK "Use '(hoist lift .)' instead" #-}

hoistPK
    :: (Monad m, Proxy p1, PFunctor t)
    => (forall _a' _a _b' _b _r .
        p1 _a' _a _b' _b m _r -> p2 _a' _a _b' _b n _r) -- ^ Proxy morphism
    -> (q -> t p1 a' a b' b m r) -- ^ Proxy Kleisli arrow
    -> (q -> t p2 a' a b' b n r)
hoistPK f = (hoistP f .)
{-# INLINABLE hoistPK #-}
{-# DEPRECATED hoistPK "Use '(hoistP f .)' instead" #-}

raiseP
    :: (Monad m, Proxy p, PFunctor t1, ProxyTrans t2)
    => t1 p a' a b' b m r -- ^ Proxy
    -> t1 (t2 p) a' a b' b m r
raiseP = hoistP liftP
{-# INLINABLE raiseP #-}
{-# DEPRECATED raiseP "Use 'hoistP liftP' instead" #-}

raisePK
    :: (Monad m, Proxy p, PFunctor t1, ProxyTrans t2)
    => (q -> t1 p a' a b' b m r) -- ^ Proxy Kleisli arrow
    -> (q -> t1 (t2 p) a' a b' b m r)
raisePK = hoistPK liftP
{-# INLINABLE raisePK #-}
{-# DEPRECATED raisePK "Use '(hoistP liftP .)' instead" #-}
