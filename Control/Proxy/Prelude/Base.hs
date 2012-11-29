-- | General purpose proxies

module Control.Proxy.Prelude.Base (
    -- * Maps
    mapD,
    mapU,
    mapB,
    mapMD,
    mapMU,
    mapMB,
    useD,
    useU,
    useB,
    execD,
    execU,
    execB,
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
    -- * Lists
    fromListS,
    fromListC,
    -- * Enumerations
    enumFromS,
    enumFromC,
    enumFromToS,
    enumFromToC,
    -- * Folds
    foldD,
    foldU,
    allD,
    allU,
    allD_,
    allU_,
    anyD,
    anyU,
    anyD_,
    anyU_,
    sumD,
    sumU,
    productD,
    productU,
    lengthD,
    lengthU,
    toListD,
    toListU,
    foldrD,
    foldrU,
    foldlD',
    foldlU',
    -- * Closed Adapters
    -- $open
    unitD,
    unitU
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Strict (WriterT, tell)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Control.Proxy.Class
import Control.Proxy.Synonym
import Control.Proxy.Trans.Identity (runIdentityP, runIdentityK)
import Data.Monoid

{-| @(mapD f)@ applies @f@ to all values going \'@D@\'ownstream.

> mapD f1 >-> mapD f2 = mapD (f2 . f1)
>
> mapD id = idT
-}
mapD :: (Monad m, Proxy p) => (a -> b) -> x -> p x a x b m r
mapD f = runIdentityK go where
    go x = do
        a  <- request x
        x2 <- respond (f a)
        go x2
-- mapD f = foreverK $ request >=> respond . f

{-| @(mapU g)@ applies @g@ to all values going \'@U@\'pstream.

> mapU g1 >-> mapU g2 = mapU (g1 . g2)
>
> mapU id = idT
-}
mapU :: (Monad m, Proxy p) => (b' -> a') -> b' -> p a' x b' x m r
mapU g = runIdentityK go where
    go b' = do
        x   <- request (g b')
        b'2 <- respond x
        go b'2
-- mapU g = foreverK $ (request . g) >=> respond

{-| @(mapB f g)@ applies @f@ to all values going downstream and @g@ to all
    values going upstream.

    Mnemonic: map \'@B@\'idirectional

> mapB f1 g1 >-> mapB f2 g2 = mapB (f2 . f1) (g1 . g2)
>
> mapB id id = idT
-}
mapB :: (Monad m, Proxy p) => (a -> b) -> (b' -> a') -> b' -> p a' a b' b m r
mapB f g = runIdentityK go where
    go b' = do
        a   <- request (g b')
        b'2 <- respond (f a )
        go b'2
-- mapB f g = foreverK $ request . g >=> respond . f

{-| @(mapMD f)@ applies the monadic function @f@ to all values going downstream

> mapMD f1 >-> mapMD f2 = mapMD (f1 >=> f2)
>
> mapMD return = idT
-}
mapMD :: (Monad m, Proxy p) => (a -> m b) -> x -> p x a x b m r
mapMD f = runIdentityK go where
    go x = do
        a  <- request x
        b  <- lift (f a)
        x2 <- respond b
        go x2
-- mapMD f = foreverK $ request >=> lift . f >=> respond

{-| @(mapMU g)@ applies the monadic function @g@ to all values going upstream

> mapMU g1 >-> mapMU g2 = mapMU (g2 >=> g1)
>
> mapMU return = idT
-}
mapMU :: (Monad m, Proxy p) => (b' -> m a') -> b' -> p a' x b' x m r
mapMU g = runIdentityK go where
    go b' = do
        a'  <- lift (g b')
        x   <- request a'
        b'2 <- respond x
        go b'2
-- mapMU g = foreverK $ lift . g >=> request >=> respond

{-| @(mapMB f g)@ applies the monadic function @f@ to all values going
    downstream and the monadic function @g@ to all values going upstream.

> mapMB f1 g1 >-> mapMB f2 g2 = mapMB (f1 >=> f2) (g2 >=> g1)
>
> mapMB return return = idT
-}
mapMB
 :: (Monad m, Proxy p) => (a -> m b) -> (b' -> m a') -> b' -> p a' a b' b m r
mapMB f g = runIdentityK go where
    go b' = do
        a'  <- lift (g b')
        a   <- request a'
        b   <- lift (f a )
        b'2 <- respond b
        go b'2
-- mapMB f g = foreverK $ lift . g >=> request >=> lift . f >=> respond

{-| @(useD f)@ executes the monadic function @f@ on all values flowing
    \'@D@\'ownstream

> useD f1 >-> useD f2 = useD (\a -> f1 a >> f2 a)
>
> useD (\_ -> return ()) = idT
-}
useD :: (Monad m, Proxy p) => (a -> m r1) -> x -> p x a x a m r
useD f = runIdentityK go where
    go x = do
        a  <- request x
        lift $ f a
        x2 <- respond a
        go x2

{-| @(useU g)@ executes the monadic function @g@ on all values flowing
    \'@U@\'pstream

> useU g1 >-> useU g2 = useU (\a' -> g2 a' >> g1 a')
>
> useU (\_ -> return ()) = idT
-}
useU :: (Monad m, Proxy p) => (a' -> m r2) -> a' -> p a' x a' x m r
useU g = runIdentityK go where
    go a' = do
        lift $ g a'
        x   <- request a'
        a'2 <- respond x
        go a'2

{-| @(useB f g)@ executes the monadic function @f@ on all values flowing
    downstream and the monadic function @g@ on all values flowing upstream

> useB f1 g1 >-> useB f2 g2 = useB (\a -> f1 a >> f2 a) (\a' -> g2 a' >> g1 a')
>
> useB (\_ -> return ()) (\_ -> return ()) = idT
-}
useB
 :: (Monad m, Proxy p) => (a -> m r1) -> (a' -> m r2) -> a' -> p a' a a' a m r
useB f g = runIdentityK go where
    go a' = do
        lift $ g a'
        a   <- request a'
        lift $ f a
        a'2 <- respond a
        go a'2

{-| @(execD md)@ executes @md@ every time values flow downstream through it.

> execD md1 >-> execD md2 = execD (md1 >> md2)
>
> execD (return ()) = idT
-}
execD :: (Monad m, Proxy p) => m r1 -> a' -> p a' a a' a m r
execD md = runIdentityK go where
    go a' = do
        a   <- request a'
        lift md
        a'2 <- respond a
        go a'2
{- execD md = foreverK $ \a' -> do
    a <- request a'
    lift md
    respond a -}

{-| @(execU mu)@ executes @mu@ every time values flow upstream through it.

> execU mu1 >-> execU mu2 = execU (mu2 >> mu1)
>
> execU (return ()) = idT
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

{-| @(execB md mu)@ executes @mu@ every time values flow upstream through it,
    and executes @md@ every time values flow downstream through it.

> execB md1 mu1 >-> execB md2 mu2 = execB (md1 >> md2) (mu2 >> mu1)
>
> execB (return ()) = idT
-}
execB :: (Monad m, Proxy p) => m r1 -> m r2 -> a' -> p a' a a' a m r
execB md mu = runIdentityK go where
    go a' = do
        lift mu
        a   <- request a'
        lift md
        a'2 <- respond a
        go a'2
{- execB md mu = foreverK $ \a' -> do
    lift mu
    a <- request a'
    lift md
    respond a -}

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
-- takeB n = replicateK n $ request >=> respond

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

{-| @(takeWhileD p)@ allows values to pass downstream so long as they satisfy
    the predicate @p@.

> -- Using the "All" monoid over functions:
> mempty = \_ -> True
> (p1 <> p2) a = p1 a && p2 a
>
> takeWhileD p1 >-> takeWhileD p2 = takeWhileD (p1 <> p2)
>
> takeWhileD mempty = idT
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

{-| @(takeWhileU p)@ allows values to pass upstream so long as they satisfy the
    predicate @p@.

> takeWhileU p1 >-> takeWhileU p2 = takeWhileU (p1 <> p2)
>
> takeWhileD mempty = idT
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

{-| @(dropD n)@ discards @n@ values going downstream

> dropD n1 >-> dropD n2 = dropD (n1 + n2)  -- n2 >= 0 && n2 >= 0
>
> dropD 0 = idT
-}
dropD :: (Monad m, Proxy p) => Int -> () -> Pipe p a a m r
dropD n0 = \() -> runIdentityP (go n0) where
    go n
        | n <= 0    = idT ()
        | otherwise = do
            request ()
            go (n - 1)
{- dropD n () = do
    replicateM_ n $ request ()
    idT () -}

{-| @(dropU n)@ discards @n@ values going upstream

> dropU n1 >-> dropU n2 = dropU (n1 + n2)  -- n2 >= 0 && n2 >= 0
>
> dropU 0 = idT
-}
dropU :: (Monad m, Proxy p) => Int -> a' -> CoPipe p a' a' m r
dropU n0 = runIdentityK (go n0) where
    go n
        | n <= 0    = idT
        | otherwise = \_ -> do
            a' <- respond ()
            go (n - 1) a'

{-| @(dropWhileD p)@ discards values going downstream until one violates the
    predicate @p@.

> -- Using the "Any" monoid over functions:
> mempty = \_ -> False
> (p1 <> p2) a = p1 a || p2 a
>
> dropWhileD p1 >-> dropWhileD p2 = dropWhileD (p1 <> p2)
>
> dropWhileD mempty = idT
-}
dropWhileD :: (Monad m, Proxy p) => (a -> Bool) -> () -> Pipe p a a m r
dropWhileD p () = runIdentityP go where
    go = do
        a <- request ()
        if (p a)
            then go
            else do
                x <- respond a
                idT x

{-| @(dropWhileU p)@ discards values going upstream until one violates the
    predicate @p@.

> dropWhileU p1 >-> dropWhileU p2 = dropWhileU (p1 <> p2)
>
> dropWhileU mempty = idT
-}
dropWhileU :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> CoPipe p a' a' m r
dropWhileU p = runIdentityK go where
    go a' =
        if (p a')
            then do
                a2 <- respond ()
                go a2
            else idT a'

{-| @(filterD p)@ discards values going downstream if they fail the predicate
    @p@

> -- Using the "All" monoid over functions:
> mempty = \_ -> True
> (p1 <> p2) a = p1 a && p2 a
>
> filterD p1 >-> filterD p2 = filterD (p1 <> p2)
>
> filterD mempty = idT
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

{-| @(filterU p)@ discards values going upstream if they fail the predicate @p@

> filterU p1 >-> filterU p2 = filterU (p1 <> p2)
>
> filterU mempty = idT
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

{-| Convert a list into a 'Producer'

> fromListS xs >=> fromListS ys = fromListS (xs ++ ys)
>
> fromListS [] = return
-}
fromListS :: (Monad m, Proxy p) => [b] -> () -> Producer p b m ()
fromListS xs = \_ -> foldr (\e a -> respond e ?>= \_ -> a) (return_P ()) xs
-- fromListS xs _ = mapM_ respond xs

{-| Convert a list into a 'CoProducer'

> fromListC xs >=> fromListC ys = fromListC (xs ++ ys)
>
> fromListC [] = return
-}
fromListC :: (Monad m, Proxy p) => [a'] -> () -> CoProducer p a' m ()
fromListC xs = \_ -> foldr (\e a -> request e ?>= \_ -> a) (return_P ()) xs
-- fromListC xs _ = mapM_ request xs

-- | 'Producer' version of 'enumFrom'
enumFromS :: (Enum b, Monad m, Proxy p) => b -> () -> Producer p b m r
enumFromS b0 = \_ -> runIdentityP (go b0) where
    go b = do
        respond b
        go (succ b)

-- | 'CoProducer' version of 'enumFrom'
enumFromC :: (Enum a', Monad m, Proxy p) => a' -> () -> CoProducer p a' m r
enumFromC a'0 = \_ -> runIdentityP (go a'0) where
    go a' = do
        request a'
        go (succ a')

-- | 'Producer' version of 'enumFromTo'
enumFromToS
 :: (Enum b, Ord b, Monad m, Proxy p) => b -> b -> () -> Producer p b m ()
enumFromToS b1 b2 _ = runIdentityP (go b1) where
    go b
        | b > b2    = return ()
        | otherwise = do
            respond b
            go (succ b)

-- | 'CoProducer' version of 'enumFromTo'
enumFromToC
 :: (Enum a', Ord a', Monad m, Proxy p)
 => a' -> a' -> () -> CoProducer p a' m ()
enumFromToC a1 a2 _ = runIdentityP (go a1) where
    go n
        | n > a2 = return ()
        | otherwise = do
            request n
            go (succ n)

{-| Fold values flowing \'@D@\'ownstream

> foldD f >-> foldD g = foldD (f <> g)
>
> foldD mempty = idT
-}
foldD
 :: (Monad m, Proxy p, Monoid w) => (a -> w) -> x -> p x a x a (WriterT w m) r
foldD f = runIdentityK go where
    go x = do
        a <- request x
        lift $ tell $ f a
        x2 <- respond a
        go x2

{-| Fold values flowing \'@U@\'pstream

> foldU f >-> foldU g = foldU (g <> f)
>
> foldU mempty = idT
-}
foldU
 :: (Monad m, Proxy p, Monoid w)
 => (a' -> w) -> a' -> p a' x a' x (WriterT w m) r
foldU f = runIdentityK go where
    go a' = do
        lift $ tell $ f a'
        x <- request a'
        a'2 <- respond x
        go a'2

{-| Fold that returns whether 'All' values flowing \'@D@\'ownstream satisfy the
    predicate -}
allD :: (Monad m, Proxy p) => (a -> Bool) -> x -> p x a x a (WriterT All m) r
allD pred = foldD (All . pred)

{-| Fold that returns whether 'All' values flowing \'@U@\'pstream satisfy the
    predicate -}
allU
 :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> p a' x a' x (WriterT All m) r
allU pred = foldU (All . pred)

{-| Fold that returns whether 'All' values flowing \'@D@\'ownstream satisfy the
    predicate

    'allD_' terminates on the first value that fails the predicate -}
allD_ :: (Monad m, Proxy p) => (a -> Bool) -> x -> p x a x a (WriterT All m) ()
allD_ pred = runIdentityK go where
    go x = do
        a <- request x
        if (pred a)
            then do
                x2 <- respond a
                go x2
            else lift $ tell $ All False

{-| Fold that returns whether 'All' values flowing \'@U@\'pstream satisfy the
    predicate

    'allU_' terminates on the first value that fails the predicate -}
allU_
 :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> p a' x a' x (WriterT All m) ()
allU_ pred = runIdentityK go where
    go a' =
        if (pred a')
            then do
                x   <- request a'
                a'2 <- respond x
                go a'2
            else lift $ tell $ All False

{-| Fold that returns whether 'Any' value flowing \'@D@\'ownstream satisfies
    the predicate -}
anyD :: (Monad m, Proxy p) => (a -> Bool) -> x -> p x a x a (WriterT Any m) r
anyD pred = foldD (Any . pred)

{-| Fold that returns whether 'Any' value flowing \'@U@\'pstream satisfies
    the predicate -}
anyU
 :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> p a' x a' x (WriterT Any m) r
anyU pred = foldU (Any . pred)

{-| Fold that returns whether 'Any' value flowing \'@D@\'ownstream satisfies the
    predicate

    'anyD_' terminates on the first value that satisfies the predicate -}
anyD_ :: (Monad m, Proxy p) => (a -> Bool) -> x -> p x a x a (WriterT Any m) ()
anyD_ pred = runIdentityK go where
    go x = do
        a <- request x
        if (pred a)
            then lift $ tell $ Any True
            else do
                x2 <- respond a
                go x2

{-| Fold that returns whether 'Any' value flowing \'@U@\'pstream satisfies the
    predicate

    'anyU_' terminates on the first value that satisfies the predicate -}
anyU_
 :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> p a' x a' x (WriterT Any m) ()
anyU_ pred = runIdentityK go where
    go a' =
        if (pred a')
            then lift $ tell $ Any True
            else do
                x   <- request a'
                a'2 <- respond x
                go a'2

-- | Compute the 'Sum' of all values that flow \'@D@\'ownstream
sumD :: (Monad m, Proxy p, Num a) => x -> p x a x a (WriterT (Sum a) m) r
sumD = foldD Sum

-- | Compute the 'Sum' of all values that flow \'@U@\'pstream
sumU :: (Monad m, Proxy p, Num a') => a' -> p a' x a' x (WriterT (Sum a') m) r
sumU = foldU Sum

-- | Compute the 'Product' of all values that flow \'@D@\'ownstream
productD
 :: (Monad m, Proxy p, Num a) => x -> p x a x a (WriterT (Product a) m) r
productD = foldD Product

-- | Compute the 'Product' of all values that flow \'@U@\'pstream
productU
 :: (Monad m, Proxy p, Num a') => a' -> p a' x a' x (WriterT (Product a') m) r
productU = foldU Product

-- | Count how many values flow \'@D@\'ownstream
lengthD :: (Monad m, Proxy p) => x -> p x a x a (WriterT (Sum Int) m) r
lengthD = foldD (\_ -> Sum 1)

-- | Count how many values flow \'@U@\'pstream
lengthU :: (Monad m, Proxy p) => a' -> p a' x a' x (WriterT (Sum Int) m) r
lengthU = foldU (\_ -> Sum 1)

-- | Fold the values flowing \'@D@\'ownstream into a list
toListD :: (Monad m, Proxy p) => x -> p x a x a (WriterT [a] m) r
toListD = foldD (\x -> [x])

-- | Fold the values flowing \'@U@\'pstream into a list
toListU :: (Monad m, Proxy p) => a' -> p a' x a' x (WriterT [a'] m) r
toListU = foldU (\x -> [x])

{-| Fold equivalent to 'foldr'

    To see why, consider this isomorphic type for 'foldr':

> foldr :: (a -> b -> b) -> [a] -> Endo b
-}
foldrD
 :: (Monad m, Proxy p) => (a -> b -> b) -> x -> p x a x a (WriterT (Endo b) m) r
foldrD step = foldD (Endo . step)

-- | Fold equivalent to 'foldr'
foldrU
 :: (Monad m, Proxy p)
 => (a' -> b -> b) -> a' -> p a' x a' x (WriterT (Endo b) m) r
foldrU step = foldU (Endo . step)

-- | Left strict fold over \'@D@\'ownstream values
foldlD'
 :: (Monad m, Proxy p) => (b -> a -> b) -> x -> p x a x a (StateT b m) r
foldlD' f = runIdentityK go where
    go x = do
        a  <- request x
        lift $ do
            b <- get
            put $! f b a
        x2 <- respond a
        go x2

-- | Left strict fold over \'@U@\'pstream values
foldlU'
 :: (Monad m, Proxy p) => (b -> a' -> b) -> a' -> p a' x a' x (StateT b m) r
foldlU' f = runIdentityK go where
    go a' = do
        lift $ do
            b <- get
            put $! f b a'
        x   <- request a'
        a'2 <- respond x
        go a'2

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
unitD :: (Monad m, Proxy p) => y' -> p x' x y' () m r
unitD _ = runIdentityP go where
    go = do
        respond ()
        go

-- | Compose 'unitU' with a closed downstream end to create a polymorphic end
unitU :: (Monad m, Proxy p) => y' -> p () x y' y m r
unitU _ = runIdentityP go where
    go = do
        request ()
        go
