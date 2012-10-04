-- | General purpose functions that happen to form functors

module Control.Proxy.Util.Map (
    -- * Maps
    mapB,
    mapD,
    mapU,
    -- * Counts
    takeB,
    dropD,
    -- * Lists
    fromListD,
    fromListU 
    ) where

import Control.Monad ((>=>))
import Control.Proxy.Class
import Control.Proxy.Core
import Control.Proxy.Util.Core
import Prelude hiding (drop, take)

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
