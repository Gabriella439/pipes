-- | General purpose functions that happen to form functors

module Control.Proxy.Util.Map (
    -- * Maps
    mapB,
    mapD,
    mapU,
    mapMB,
    mapMD,
    mapMU,
    -- * Counts
    takeB,
    dropD,
    -- * Lists
    fromListD,
    fromListU,
    -- * Enumerations
    fromToD,
    fromToU
    ) where

import Control.Monad ((>=>))
import Control.Monad.Trans.Class (lift)
import Control.Proxy.Class (request, respond)
import Control.Proxy.Core (Proxy, Server, Client)
import Control.Proxy.Util.Core (foreverK, replicateK)

{-| @(mapB f g)@ applies @f@ to all values going downstream and @g@ to all
    values going upstream.

    Mnemonic: map \'@B@\'idirectional -}
mapB :: (Monad m) => (a -> b) -> (b' -> a') -> b' -> Proxy a' a b' b m r
mapB f g = foreverK $ (request . g) >=> (respond . f)

-- | @(mapD f)@ applies @f@ to all values going \'@D@\'ownstream.
mapD :: (Monad m) => (a -> b) -> x -> Proxy x a x b m r
mapD f = foreverK $ request >=> (respond . f)

-- | @(mapU g)@ applies @g@ to all values going \'@U@\'pstream.
mapU :: (Monad m) => (b' -> a') -> b' -> Proxy a' x b' x m r
mapU g = foreverK $ (request . g) >=> respond

{-| @(mapMB f g)@ applies the monadic function @f@ to all values going
    downstream and the monadic function @g@ to all values going upstream. -}
mapMB :: (Monad m) => (a -> m b) -> (b' -> m a') -> b' -> Proxy a' a b' b m r
mapMB f g = foreverK $ lift . g >=> request >=> lift . f >=> respond

-- | @(mapMD f)@ applies the monadic function @f@ to all values going downstream
mapMD :: (Monad m) => (a -> m b) -> x -> Proxy x a x b m r
mapMD f = foreverK $ request >=> lift . f >=> respond

-- | @(mapMU g)@ applies the monadic function @g@ to all values going upstream
mapMU :: (Monad m) => (b' -> m a') -> b' -> Proxy a' x b' x m r
mapMU g = foreverK $ lift . g >=> request >=> respond

-- | @(takeB n)@ allows @n@ upstream/downstream roundtrips to pass through
takeB :: (Monad m) => Int -> a' -> Proxy a' a a' a m a'
takeB n = replicateK n $ request >=> respond

-- | @(dropD n)@ discards @n@ values going downstream
dropD :: (Monad m) => Int -> a' -> Proxy a' a a' a m a'
dropD n = replicateK n $ \a' -> do
    request a'
    return a'

-- | 'respond' to \'@D@\'ownstream using values from a list
fromListD :: (Monad m) => [a] -> () -> Server () a m ()
fromListD xs () = mapM_ respond xs

-- | 'request' from \'@U@\'pstream using arguments from a list
fromListU :: (Monad m) => [a] -> () -> Client a () m ()
fromListU xs () = mapM_ request xs

-- | 'Server' version of 'enumFromTo'
fromToD :: (Enum a, Ord a, Monad m) => a -> a -> () -> Server () a m ()
fromToD a1 a2 () = go a1 where
    go n
        | n > a2   = return ()
        | otherwise = do
            respond n
            go (succ n)

-- | 'Client' version of 'enumFromTo'
fromToU :: (Enum a, Ord a, Monad m) => a -> a -> () -> Client a () m ()
fromToU a1 a2 () = go a1 where
    go n
        | n > a2 = return ()
        | otherwise = do
            request n
            go (succ n)
