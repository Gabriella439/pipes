-- | General purpose proxies

module Control.Proxy.Prelude.Base (
    -- * Maps
    mapB,
    mapD,
    mapU,
    mapMB,
    mapMD,
    mapMU,
    execB,
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
    -- * Lists
    fromListS,
    fromListC,
    -- * Enumerations
    enumFromS,
    enumFromC,
    enumFromToS,
    enumFromToC
    ) where

import Control.Monad (replicateM_, void, when, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Proxy.Class (request, respond, idT)
import Control.Proxy.Core (Proxy, Server, Client)
import Control.Proxy.Prelude.Kleisli (foreverK, replicateK)

{-| @(mapB f g)@ applies @f@ to all values going downstream and @g@ to all
    values going upstream.

    Mnemonic: map \'@B@\'idirectional

> mapB f1 g1 >-> mapB f2 g2 = mapB (f2 . f1) (g1 . g2)
>
> mapB id id = idT
-}
mapB :: (Monad m) => (a -> b) -> (b' -> a') -> b' -> Proxy a' a b' b m r
mapB f g = foreverK $ (request . g) >=> (respond . f)

{-| @(mapD f)@ applies @f@ to all values going \'@D@\'ownstream.

> mapD f1 >-> mapD f2 = mapD (f2 . f1)
>
> mapD id = idT
-}
mapD :: (Monad m) => (a -> b) -> x -> Proxy x a x b m r
mapD f = foreverK $ request >=> (respond . f)

{-| @(mapU g)@ applies @g@ to all values going \'@U@\'pstream.

> mapU g1 >-> mapU g2 = mapU (g1 . g2)
>
> mapU id = idT
-}
mapU :: (Monad m) => (b' -> a') -> b' -> Proxy a' x b' x m r
mapU g = foreverK $ (request . g) >=> respond

{-| @(mapMB f g)@ applies the monadic function @f@ to all values going
    downstream and the monadic function @g@ to all values going upstream.

> mapMB f1 g1 >-> mapMB f2 g2 = mapMB (f1 >=> f2) (g2 >=> g1)
>
> mapMB return return = idT
-}
mapMB :: (Monad m) => (a -> m b) -> (b' -> m a') -> b' -> Proxy a' a b' b m r
mapMB f g = foreverK $ lift . g >=> request >=> lift . f >=> respond

{-| @(mapMD f)@ applies the monadic function @f@ to all values going downstream

> mapMD f1 >-> mapMD f2 = mapMD (f1 >=> f2)
>
> mapMD return = idT
-}
mapMD :: (Monad m) => (a -> m b) -> x -> Proxy x a x b m r
mapMD f = foreverK $ request >=> lift . f >=> respond

{-| @(mapMU g)@ applies the monadic function @g@ to all values going upstream

> mapMU g1 >-> mapMU g2 = mapMU (g2 >=> g1)
>
> mapMU return = idT
-}
mapMU :: (Monad m) => (b' -> m a') -> b' -> Proxy a' x b' x m r
mapMU g = foreverK $ lift . g >=> request >=> respond

{-| @(execB md mu)@ executes @mu@ every time values flow upstream through it,
    and executes @md@ every time values flow downstream through it.

> execB md1 mu1 >-> execB md2 mu2 = execB (md1 >> md2) (mu2 >> mu1)
>
> execB (return ()) = idT
-}
execB :: (Monad m) => m () -> m () -> a' -> Proxy a' a a' a m r
execB md mu = foreverK $ \a' -> do
    lift mu
    a <- request a'
    lift md
    respond a

{-| @execD md)@ executes @md@ every time values flow downstream through it.

> execD md1 >-> execD md2 = execD (md1 >> md2)
>
> execD (return ()) = idT
-}
execD :: (Monad m) => m () -> a' -> Proxy a' a a' a m r
execD md = foreverK $ \a' -> do
    a <- request a'
    lift md
    respond a

{-| @execU mu)@ executes @mu@ every time values flow upstream through it.

> execU mu1 >-> execU mu2 = execU (mu2 >> mu1)
>
> execU (return ()) = idT
-}
execU :: (Monad m) => m () -> a' -> Proxy a' a a' a m r
execU mu = foreverK $ \a' -> do
    lift mu
    a <- request a'
    respond a

{-| @(takeB n)@ allows @n@ upstream/downstream roundtrips to pass through

> takeB n1 >=> takeB n2 = takeB (n1 + n2)  -- n1 >= 0 && n2 >= 0
>
> takeB 0 = return
-}
takeB :: (Monad m) => Int -> a' -> Proxy a' a a' a m a'
takeB n = replicateK n $ request >=> respond

-- | 'takeB_' is 'takeB' with a @()@  return value, convenient for composing
takeB_ :: (Monad m) => Int -> a' -> Proxy a' a a' a m ()
takeB_ n = fmap void (takeB n)

{-| @takeWhileD p@ allows values to pass downstream so long as they satisfy the
     predicate @p@.

> -- Using the "All" monoid over functions:
> mempty = \_ -> True
> (p1 <> p2) a = p1 a && p2 a
>
> takeWhileD p1 >-> takeWhileD p2 = takeWhileD (p1 <> p2)
>
> takeWhileD mempty = idT
-}
takeWhileD :: (Monad m) => (a -> Bool) -> a' -> Proxy a' a a' a m ()
takeWhileD p = go where
    go a' = do
        a <- request a'
        if (p a)
        then do
            a'2 <- respond a
            go a'2
        else return ()

{-| @takeWhileU p@ allows values to pass upstream so long as they satisfy the
    predicate @p@.

> takeWhileU p1 >-> takeWhileU p2 = takeWhileU (p1 <> p2)
>
> takeWhileD mempty = idT
-}
takeWhileU :: (Monad m) => (a' -> Bool) -> a' -> Proxy a' a a' a m ()
takeWhileU p = go where
    go a' =
        if (p a')
        then do
            a <- request a'
            a'2 <- respond a
            go a'2
        else return ()

{-| @(dropD n)@ discards @n@ values going downstream

> dropD n1 >-> dropD n2 = dropD (n1 + n2)  -- n2 >= 0 && n2 >= 0
>
> dropD 0 = idT
-}
dropD :: (Monad m) => Int -> () -> Proxy () a () a m r
dropD n () = do
    replicateM_ n $ request ()
    idT ()

{-| @(dropU n)@ discards @n@ values going upstream

> dropU n1 >-> dropU n2 = dropU (n1 + n2)  -- n2 >= 0 && n2 >= 0
>
> dropU 0 = idT
-}
dropU :: (Monad m) => Int -> a' -> Proxy a' () a' () m r
dropU n a'
    | n <= 0    = idT a'
    | otherwise = do
        replicateM_ (n - 1) $ respond ()
        a'2 <- respond ()
        idT a'2

{-| @(dropWhileD p)@ discards values going upstream until one violates the
    predicate @p@.

> -- Using the "Any" monoid over functions:
> mempty = \_ -> False
> (p1 <> p2) a = p1 a || p2 a
>
> dropWhileD p1 >-> dropWhileD p2 = dropWhileD (p1 <> p2)
>
> dropWhileD mempty = idT
-}
dropWhileD :: (Monad m) => (a -> Bool) -> () -> Proxy () a () a m r
dropWhileD p () = go where
    go = do
        a <- request ()
        if (p a)
        then go
        else do
            respond a
            idT ()

{-| @(dropWhileU p)@ discards values going downstream until one violates the
    predicate @p@.

> dropWhileU p1 >-> dropWhileU p2 = dropWhileU (p1 <> p2)
>
> dropWhileU mempty = idT
-}
dropWhileU :: (Monad m) => (a' -> Bool) -> a' -> Proxy a' () a' () m r
dropWhileU p = go where
    go a' =
        if (p a')
        then do
            a' <- respond ()
            go a'
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
filterD :: (Monad m) => (a -> Bool) -> () -> Proxy () a () a m r
filterD p () = go where
    go = do
        a <- request ()
        when (p a) $ respond a
        go

{-| @(filterU p)@ discards values going upstream if they fail the predicate @p@

> filterU p1 >-> filterU p2 = filterU (p1 <> p2)
>
> filterU mempty = idT
-}
filterU :: (Monad m) => (a' -> Bool) -> a' -> Proxy a' () a' () m r
filterU p a' = go a' where
    go a' = do
        when (p a') $ request a'
        a'2 <- respond ()
        go a'2

{-| Convert a list into a 'Server'

> fromListS xs >=> fromListS ys = fromListS (xs ++ ys)
>
> fromListS [] = return
-}
fromListS :: (Monad m) => [a] -> y' -> Proxy x' x y' a m ()
fromListS xs _ = mapM_ respond xs

{-| Convert a list into a 'Client'

> fromListC xs >=> fromListC ys = fromListC (xs ++ ys)
>
> fromListC [] = return
-}
fromListC :: (Monad m) => [a] -> y' -> Proxy a x y' y m ()
fromListC xs _ = mapM_ request xs

-- | 'Server' version of 'enumFrom'
enumFromS :: (Enum a, Monad m) => a -> y' -> Proxy x' x y' a m r
enumFromS a _ = go a where
    go a = do
        respond a
        go (succ a)

-- | 'Client' version of 'enumFrom'
enumFromC :: (Enum a, Monad m) => a -> y' -> Proxy a x y' y m r
enumFromC a _ = go a where
    go a = do
        request a
        go (succ a)

-- | 'Server' version of 'enumFromTo'
enumFromToS :: (Enum a, Ord a, Monad m) => a -> a -> y' -> Proxy x' x y' a m ()
enumFromToS a1 a2 _ = go a1 where
    go n
        | n > a2   = return ()
        | otherwise = do
            respond n
            go (succ n)

-- | 'Client' version of 'enumFromTo'
enumFromToC :: (Enum a, Ord a, Monad m) => a -> a -> y' -> Proxy a x y' y m ()
enumFromToC a1 a2 _ = go a1 where
    go n
        | n > a2 = return ()
        | otherwise = do
            request n
            go (succ n)
