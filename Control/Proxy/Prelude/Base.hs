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
    enumFromToC,
    -- * Closed Adapters
    -- $open
    openU,
    openD
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Proxy.Class
import Control.Proxy.Synonym
import Control.Proxy.Trans.Identity (P)
import Data.Closed (C)

{-| @(mapB f g)@ applies @f@ to all values going downstream and @g@ to all
    values going upstream.

    Mnemonic: map \'@B@\'idirectional

> mapB f1 g1 >-> mapB f2 g2 = mapB (f2 . f1) (g1 . g2)
>
> mapB id id = idT
-}
mapB :: (Monad m, ProxyP p) => (a -> b) -> (b' -> a') -> b' -> P p a' a b' b m r
mapB f g = go where
    go b' = do
        a   <- request (g b')
        b'2 <- respond (f a )
        go b'2
-- mapB f g = foreverK $ request . g >=> respond . f

{-| @(mapD f)@ applies @f@ to all values going \'@D@\'ownstream.

> mapD f1 >-> mapD f2 = mapD (f2 . f1)
>
> mapD id = idT
-}
mapD :: (Monad m, ProxyP p) => (a -> b) -> x -> P p x a x b m r
mapD f = go where
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
mapU :: (Monad m, ProxyP p) => (b' -> a') -> b' -> P p a' x b' x m r
mapU g = go where
    go b' = do
        x   <- request (g b')
        b'2 <- respond x
        go b'2
-- mapU g = foreverK $ (request . g) >=> respond

{-| @(mapMB f g)@ applies the monadic function @f@ to all values going
    downstream and the monadic function @g@ to all values going upstream.

> mapMB f1 g1 >-> mapMB f2 g2 = mapMB (f1 >=> f2) (g2 >=> g1)
>
> mapMB return return = idT
-}
mapMB
 :: (Monad m, ProxyP p) => (a -> m b) -> (b' -> m a') -> b' -> P p a' a b' b m r
mapMB f g = go where
    go b' = do
        a'  <- lift (g b')
        a   <- request a'
        b   <- lift (f a )
        b'2 <- respond b
        go b'2
-- mapMB f g = foreverK $ lift . g >=> request >=> lift . f >=> respond

{-| @(mapMD f)@ applies the monadic function @f@ to all values going downstream

> mapMD f1 >-> mapMD f2 = mapMD (f1 >=> f2)
>
> mapMD return = idT
-}
mapMD :: (Monad m, ProxyP p) => (a -> m b) -> x -> P p x a x b m r
mapMD f = go where
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
mapMU :: (Monad m, ProxyP p) => (b' -> m a') -> b' -> P p a' x b' x m r
mapMU g = go where
    go b' = do
        a'  <- lift (g b')
        x   <- request a'
        b'2 <- respond x
        go b'2
-- mapMU g = foreverK $ lift . g >=> request >=> respond

{-| @(execB md mu)@ executes @mu@ every time values flow upstream through it,
    and executes @md@ every time values flow downstream through it.

> execB md1 mu1 >-> execB md2 mu2 = execB (md1 >> md2) (mu2 >> mu1)
>
> execB (return ()) = idT
-}
execB :: (Monad m, ProxyP p) => m () -> m () -> a' -> P p a' a a' a m r
execB md mu = go where
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

{-| @execD md)@ executes @md@ every time values flow downstream through it.

> execD md1 >-> execD md2 = execD (md1 >> md2)
>
> execD (return ()) = idT
-}
execD :: (Monad m, ProxyP p) => m () -> a' -> P p a' a a' a m r
execD md = go where
    go a' = do
        a   <- request a'
        lift md
        a'2 <- respond a
        go a'2
{- execD md = foreverK $ \a' -> do
    a <- request a'
    lift md
    respond a -}

{-| @execU mu)@ executes @mu@ every time values flow upstream through it.

> execU mu1 >-> execU mu2 = execU (mu2 >> mu1)
>
> execU (return ()) = idT
-}
execU :: (Monad m, ProxyP p) => m () -> a' -> P p a' a a' a m r
execU mu = go where
    go a' = do
        lift mu
        a   <- request a'
        a'2 <- respond a
        go a'2
{- execU mu = foreverK $ \a' -> do
    lift mu
    a <- request a'
    respond a -}

{-| @(takeB n)@ allows @n@ upstream/downstream roundtrips to pass through

> takeB n1 >=> takeB n2 = takeB (n1 + n2)  -- n1 >= 0 && n2 >= 0
>
> takeB 0 = return
-}
takeB :: (Monad m, ProxyP p) => Int -> a' -> P p a' a a' a m a'
takeB n0 = go n0 where
    go n
        | n <= 0    = return
        | otherwise = \a' -> do
             a   <- request a'
             a'2 <- respond a
             go (n - 1) a'2
-- takeB n = replicateK n $ request >=> respond

-- | 'takeB_' is 'takeB' with a @()@ return value, convenient for composing
takeB_ :: (Monad m, ProxyP p) => Int -> a' -> P p a' a a' a m ()
takeB_ n0 = go n0 where
    go n
        | n <= 0    = \_ -> return ()
        | otherwise = \a' -> do
            a   <- request a'
            a'2 <- respond a
            go (n - 1) a'2
-- takeB_ n = fmap void (takeB n)

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
takeWhileD :: (Monad m, ProxyP p) => (a -> Bool) -> a' -> P p a' a a' a m ()
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
takeWhileU :: (Monad m, ProxyP p) => (a' -> Bool) -> a' -> P p a' a a' a m ()
takeWhileU p = go where
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
dropD :: (Monad m, ProxyP p) => Int -> () -> Pipe (P p) a a m r
dropD n0 = \() -> go n0 where
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
dropU :: (Monad m, ProxyP p) => Int -> a' -> CoPipe (P p) a' a' m r
dropU = go where
    go n
        | n <= 0    = idT
        | otherwise = \_ -> do
            a' <- respond ()
            go (n - 1) a'

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
dropWhileD :: (Monad m, ProxyP p) => (a -> Bool) -> () -> Pipe p a a m r
dropWhileD p () = go where
    go =
        request () ?>= \a ->
        if (p a)
        then go
        else respond a ?>= idT

{-| @(dropWhileU p)@ discards values going downstream until one violates the
    predicate @p@.

> dropWhileU p1 >-> dropWhileU p2 = dropWhileU (p1 <> p2)
>
> dropWhileU mempty = idT
-}
dropWhileU :: (Monad m, ProxyP p) => (a' -> Bool) -> a' -> CoPipe p a' a' m r
dropWhileU p = go where
    go a' = if (p a') then respond () ?>= go else idT a'

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
filterD :: (Monad m, ProxyP p) => (a -> Bool) -> () -> Pipe p a a m r
filterD p = \() ->  go where
    go = request () ?>= \a ->
         if (p a)
             then
                 respond a ?>= \_ ->
                 go
             else go

{-| @(filterU p)@ discards values going upstream if they fail the predicate @p@

> filterU p1 >-> filterU p2 = filterU (p1 <> p2)
>
> filterU mempty = idT
-}
filterU :: (Monad m, ProxyP p) => (a' -> Bool) -> a' -> CoPipe p a' a' m r
filterU p a'0 = go a'0 where
    go a' =
        if (p a')
        then
            request a' ?>= \_ ->
            respond () ?>= go
        else respond () ?>= go

{-| Convert a list into a 'Server'

> fromListS xs >=> fromListS ys = fromListS (xs ++ ys)
>
> fromListS [] = return
-}
fromListS :: (Monad m, ProxyP p) => [b] -> () -> Producer p b m ()
fromListS xs = \_ -> foldr (\e a -> respond e ?>= \_ -> a) (return_P ()) xs
-- fromListS xs _ = mapM_ respond xs
{-# INLINE fromListS #-} -- So that foldr/build fusion occurs

{-| Convert a list into a 'Client'

> fromListC xs >=> fromListC ys = fromListC (xs ++ ys)
>
> fromListC [] = return
-}
fromListC :: (Monad m, ProxyP p) => [a'] -> () -> CoProducer p a' m ()
fromListC xs = \_ -> foldr (\e a -> request e ?>= \_ -> a) (return_P ()) xs
-- fromListC xs _ = mapM_ request xs
{-# INLINE fromListC #-} -- So that foldr/build fusion occurs

-- | 'Server' version of 'enumFrom'
enumFromS :: (Enum b, Monad m, ProxyP p) => b -> () -> Producer p b m r
enumFromS b0 = \_ -> go b0 where
    go b =
        respond b ?>= \_ ->
        go (succ b)

-- | 'Client' version of 'enumFrom'
enumFromC :: (Enum a', Monad m, ProxyP p) => a' -> () -> CoProducer p a' m r
enumFromC a'0 = \_ -> go a'0 where
    go a' =
        request a' ?>= \_ ->
        go (succ a')

-- | 'Server' version of 'enumFromTo'
enumFromToS
 :: (Enum b, Ord b, Monad m, ProxyP p) => b -> b -> () -> Producer (P p) b m ()
enumFromToS b1 b2 _ = go b1 where
    go b
        | b > b2    = return ()
        | otherwise = do
            respond b
            go (succ b)

-- | 'Client' version of 'enumFromTo'
enumFromToC
 :: (Enum a', Ord a', Monad m, ProxyP p)
 => a' -> a' -> () -> CoProducer p a' m ()
enumFromToC a1 a2 _ = go a1 where
    go n
        | n > a2 = return_P ()
        | otherwise =
            request n ?>= \_ ->
            go (succ n)

{- $open
    Use the @open@ functions when you need to embed a proxy with a closed end
    within an open proxy.  For example, the following code will not type-check
    because @fromListS [1..]@  is a 'Server' and has a closed upstream end,
    which conflicts with the 'request' statement before it:

> p () = do
>     request ()
>     fromList [1..] ()

    You fix this by composing 'openU' upstream of it, which turns its closed
    upstream end into an open polymorphic end:

> p () = do
>     request ()
>     (fromList [1..] <-< openU) ()

-}

-- | Compose 'openU' with a closed upstream end to create a polymorphic end
openU :: (Monad m, ProxyP p) => C -> p a' a C () m r
openU _ = go where
    go = respond () ?>= \_ -> go

-- | Compose 'openD' with a closed downstream end to create a polymorphic end
openD :: (Monad m, ProxyP p) => b' -> p () C b' b m r
openD _ = go where
    go = request () ?>= \_ -> go
