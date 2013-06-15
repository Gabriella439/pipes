-- | General purpose proxies

{-# LANGUAGE RankNTypes #-}

module Pipes.Prelude (
    -- * I/O
    stdin,
    fromHandle,
    stdout,
    toHandle,
    readLn,
    print,

    -- * Maps
    map,
    mapM,
    use,
    execD,
    execU,

    -- * Filters
    take,
    takeWhile,
    drop,
    dropWhile,
    filter,

    -- * Lists and Enumerations
    fromList,
    enumFrom,
    each,

    -- * Folds
    fold,
    all,
    all_,
    any,
    any_,
    sum,
    product,
    length,
    head,
    head_,
    last,
    toList,
    foldr,
    foldl',

    -- * ArrowChoice
    -- $choice
    left,
    right,

    -- * Zips and Merges
    zip,
    merge,

    -- * Adapters
    unitD,
    unitU,
    forward,
    generalize,

    -- * Kleisli utilities
    foreverK
    ) where

import Control.Monad (forever, replicateM_)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Control.Monad.Trans.Writer.Strict (WriterT, tell)
import qualified Data.Monoid as M
import Data.Monoid (
    appEndo, getAll, getAny, getSum, getProduct, getFirst, getLast )
import qualified System.IO as IO
import Pipes
import Pipes.Lift
import Prelude hiding (
    print,
    readLn,
    map,
    mapM,
    take,
    takeWhile,
    drop,
    dropWhile,
    filter,
    enumFrom,
    all,
    any,
    sum,
    product,
    length,
    head,
    last,
    foldr,
    zip )
import qualified Prelude

-- | Read 'String' lines from 'IO.stdin'
stdin :: () -> Producer String IO r
stdin () = forever $ do
    str <- lift getLine
    respond str
{-# INLINABLE stdin #-}

-- | Read 'String' lines from an 'IO.Handle'
fromHandle :: IO.Handle -> () -> Producer String IO ()
fromHandle h () = go
  where
    go = do
        eof <- lift $ IO.hIsEOF h
        if eof
            then return ()
            else do
                str <- lift $ IO.hGetLine h
                respond str
                go
{-# INLINABLE fromHandle #-}

-- | Write newline-terminated 'String's to 'IO.stdout'
stdout :: () -> Consumer String IO r
stdout = toHandle IO.stdout
{-# INLINABLE stdout #-}

-- | Write newline-terminated 'String's to a 'IO.Handle'
toHandle :: IO.Handle -> () -> Consumer String IO r
toHandle handle () = forever $ do
    str <- request ()
    lift $ IO.hPutStrLn handle str
{-# INLINABLE toHandle #-}

-- | 'read' from 'IO.stdin' using 'Prelude.readLn'
readLn :: (Read b) => () -> Producer b IO r
readLn () = forever $ do
    a <- lift Prelude.readLn
    respond a
{-# INLINABLE readLn #-}

-- | 'show' to 'IO.stdout' using 'Prelude.print'
print :: (Show a) => () -> Consumer a IO r
print () = forever $ do
    a <- request ()
    lift $ Prelude.print a
{-# INLINABLE print #-}

{-| @(map f)@ applies @f@ to all values going \'@D@\'ownstream.

> map f1 >-> map f2 = map (f2 . f1)
>
> map id = pull
-}
map :: (Monad m) => (a -> b) -> () -> Pipe a b m r
map f () = forever $ do
    a <- request ()
    respond (f a)
{-# INLINABLE map #-}

{-| @(mapM f)@ applies the monadic function @f@ to all values going downstream

> mapM f1 >-> mapM f2 = mapM (f1 >=> f2)
>
> mapM return = pull
-}
mapM :: (Monad m) => (a -> m b) -> () -> Pipe a b m r
mapM f () = forever $ do
    a <- request ()
    b <- lift $ f a
    respond b
{-# INLINABLE mapM #-}

{-| @(use f)@ executes the monadic function @f@ on all values flowing
    \'@D@\'ownstream, discarding the result

> use f1 >-> use f2 = use (\a -> f1 a >> f2 a)
>
> use (\_ -> return ()) = pull
-}
use :: (Monad m) => (a -> m b) -> () -> Pipe a a m r
use f () = forever $ do
    a <- request ()
    _ <- lift (f a)
    respond a
    return ()
{-# INLINABLE use #-}

{-| @(execD md)@ executes @md@ every time control flows downstream through it

> execD md1 >-> execD md2 = execD (md1 >> md2)
>
> execD (return ()) = pull
-}
execD :: (Monad m) => m b -> () -> Pipe a a m r
execD md () = forever $ do
    a <- request ()
    _ <- lift md
    respond a
{-# INLINABLE execD #-}

{-| @(execU mu)@ executes @mu@ every time control flows upstream through it

> execU mu1 >-> execU mu2 = execU (mu2 >> mu1)
>
> execU (return ()) = pull
-}
execU :: (Monad m) => m b -> () -> Pipe a a m r
execU mu () = forever $ do
    lift mu
    a <- request ()
    respond a
{-# INLINABLE execU #-}

-- | @(take n)@ only allows @n@ values to pass through
take :: (Monad m) => Int -> () -> Pipe a a m ()
take n () = replicateM_ n $ do
    a <- request ()
    respond a
{-# INLINABLE take #-}

{-| @(takeWhile p)@ allows values to pass downstream so long as they satisfy
    the predicate @p@.

> -- Using the "All" monoid over functions:
> mempty = \_ -> True
> (p1 <> p2) a = p1 a && p2 a
>
> takeWhile p1 >-> takeWhile p2 = takeWhile (p1 <> p2)
>
> takeWhile mempty = pull
-}
takeWhile :: (Monad m) => (a -> Bool) -> () -> Pipe a a m ()
takeWhile predicate () = go
  where
    go = do
        a <- request ()
        if (predicate a)
            then do
                respond a
                go
            else return ()
{-# INLINABLE takeWhile #-}

{-| @(drop n)@ discards @n@ values going downstream

> drop n1 >-> drop n2 = drop (n1 + n2)  -- n2 >= 0 && n2 >= 0
>
> drop 0 = pull
-}
drop :: (Monad m) => Int -> () -> Pipe a a m r
drop n () = do
    replicateM_ n $ request ()
    pull ()
{-# INLINABLE drop #-}

{-| @(dropWhile p)@ discards values going downstream until one violates the
    predicate @p@.

> -- Using the "Any" monoid over functions:
> mempty = \_ -> False
> (p1 <> p2) a = p1 a || p2 a
>
> dropWhile p1 >-> dropWhile p2 = dropWhile (p1 <> p2)
>
> dropWhile mempty = pull
-}
dropWhile :: (Monad m) => (a -> Bool) -> () -> Pipe a a m r
dropWhile p () = go
  where
    go = do
        a <- request ()
        if (p a)
            then go
            else do
                respond a
                pull ()
{-# INLINABLE dropWhile #-}

{-| @(filter p)@ discards values going downstream if they fail the predicate
    @p@

> -- Using the "All" monoid over functions:
> mempty = \_ -> True
> (p1 <> p2) a = p1 a && p2 a
>
> filter p1 >-> filter p2 = filter (p1 <> p2)
>
> filter mempty = pull
-}
filter :: (Monad m) => (a -> Bool) -> () -> Pipe a a m r
filter p () = go
  where
    go = do
        a <- request ()
        if (p a)
            then do
                respond a
                go
            else go
{-# INLINABLE filter #-}

{-| Convert a list into a 'Producer'

> fromList xs >=> fromList ys = fromList (xs ++ ys)
>
> fromList [] = return
-}
fromList :: (Monad m) => [b] -> () -> Producer b m ()
fromList xs () = mapM_ respond xs
{-# INLINABLE fromList #-}

-- | 'Producer' version of 'enumFrom'
enumFrom :: (Enum b, Monad m) => b -> () -> Producer b m r
enumFrom b0 = \_ -> go b0
  where
    go b = do
        _ <- respond b
        go $! succ b
{-# INLINABLE enumFrom #-}

{-| Non-deterministically choose from all values in the given list

> mappend <$> each xs <*> each ys = each (mappend <$> xs <*> ys)
>
> each (pure mempty) = pure mempty
-}
each :: (Monad m) => [b] -> ListT m b
each bs = RespondT (fromList bs ())
{-# INLINABLE each #-}

-- | Strict fold using the provided 'M.Monoid'
fold :: (M.Monoid w, Monad m) => (a -> w) -> () -> Consumer a (WriterT w m) r
fold f () =  forever $ do
    a <- request ()
    lift $ tell (f a)
{-# INLINABLE fold #-}

-- | Fold that returns whether 'M.All' values satisfy the predicate
all :: (Monad m) => (a -> Bool) -> () -> Consumer a (WriterT M.All m) r
all predicate = fold (M.All . predicate)
{-# INLINABLE all #-}

{-| Fold that returns whether 'M.All' values satisfy the predicate

    'all_' terminates on the first value that fails the predicate
-}
all_ :: (Monad m) => (a -> Bool) -> () -> Consumer a (WriterT M.All m) ()
all_ predicate () = go
  where
    go = do
        a <- request ()
        if (predicate a)
            then go
            else lift $ tell (M.All False)
{-# INLINABLE all_ #-}

-- | Fold that returns whether 'M.Any' value satisfies the predicate
any :: (Monad m) => (a -> Bool) -> () -> Consumer a (WriterT M.Any m) r
any predicate = fold (M.Any . predicate)
{-# INLINABLE any #-}

{-| Fold that returns whether 'M.Any' value satisfies the predicate

    'any_' terminates on the first value that satisfies the predicate
-}
any_ :: (Monad m) => (a -> Bool) -> () -> Consumer a (WriterT M.Any m) ()
any_ predicate () = go
  where
    go = do
        a <- request ()
        if (predicate a)
            then lift $ tell (M.Any True)
            else go
{-# INLINABLE any_ #-}

-- | Compute the 'M.Sum' of all values
sum :: (Monad m, Num a) => () -> Consumer a (WriterT (M.Sum a) m) r
sum = fold M.Sum
{-# INLINABLE sum #-}

-- | Compute the 'M.Product' of all values
product :: (Monad m, Num a) => () -> Consumer a (WriterT (M.Product a) m) r
product = fold M.Product
{-# INLINABLE product #-}

-- | Count the number of values
length :: (Monad m) => () -> Consumer a (WriterT (M.Sum Int) m) r
length = fold (\_ -> M.Sum 1)
{-# INLINABLE length #-}

-- | Retrieve the 'M.First' value
head :: (Monad m) => () -> Consumer a (WriterT (M.First a) m) r
head = fold (M.First . Just)
{-# INLINABLE head #-}

{-| Retrieve the 'M.First' value

    'head_' terminates on the first value it receives
-}
head_ :: (Monad m) => () -> Consumer a (WriterT (M.First a) m) ()
head_ () = do
    a <- request ()
    lift $ tell $ M.First (Just a)
{-# INLINABLE head_ #-}

-- | Retrieve the last value
last :: (Monad m) => () -> Consumer a (WriterT (M.Last a) m) r
last = fold (M.Last . Just)
{-# INLINABLE last #-}

-- | Fold the values flowing \'@D@\'ownstream into a list
toList :: (Monad m) => () -> Consumer a (WriterT [a] m) r
toList = fold (\x -> [x])
{-# INLINABLE toList #-}

{-| Fold equivalent to 'foldr'

    To see why, consider this isomorphic type for 'foldr':

> foldr :: (a -> b -> b) -> [a] -> M.Endo b
-}
foldr :: (Monad m) => (a -> b -> b) -> () -> Consumer a (WriterT (M.Endo b) m) r
foldr step = fold (M.Endo . step)
{-# INLINABLE foldr #-}

-- | Fold equivalent to 'foldl''.
--
-- Uses 'StateT' instead of 'WriterT' to ensure a strict accumulation
foldl' :: (Monad m) => (s -> a -> s) -> () -> Consumer a (StateT s m) r
foldl' step () = go
  where
    go = do
        a <- request ()
        s <- lift get
        lift $ put $! step s a 
        go
{-# INLINABLE foldl' #-}

{- $choice
    'left' and 'right' satisfy the 'ArrowChoice' laws using @arr = map@.
-}

{-| Lift a proxy to operate only on 'Left' values flowing \'@D@\'ownstream and
    forward 'Right' values
-}
left
    :: (Monad m)
    => (q -> Proxy x a x b m r)
    -> (q -> Proxy x (Either a e) x (Either b e) m r)
left k = up \>\ (k />/ dn)
  where
    dn b = respond (Left b)
    up x = do
        ma <- request x
        case ma of
            Left  a -> return a
            Right e -> do
                x2 <- respond (Right e)
                up x2
{-# INLINABLE left #-}

{-| Lift a proxy to operate only on 'Right' values flowing \'@D@\'ownstream and
    forward 'Left' values
-}
right
    :: (Monad m)
    => (q -> Proxy x a x b m r)
    -> (q -> Proxy x (Either e a) x (Either e b) m r)
right k = up \>\ (k />/ dn)
  where
    dn b = respond (Right b)
    up x = do
        ma <- request x
        case ma of
            Left  e -> do
                x2 <- respond (Left e)
                up x2
            Right a -> return a
{-# INLINABLE right #-}

-- | Zip values flowing downstream
zip :: (Monad m) => () -> Consumer' a (Consumer' b (Producer' (a, b) m)) r
zip () = go
  where
    go = do
        a <- request ()
        lift $ do
            b <- request ()
            lift $ respond (a, b)
        go
{-# INLINABLE zip #-}

-- | Interleave values flowing downstream using simple alternation
merge :: (Monad m) => () -> Consumer' a (Consumer' a (Producer' a m)) r
merge () = go
  where
    go = do
        a1 <- request ()
        lift $ do
            lift $ respond a1
            a2 <- request ()
            lift $ respond a2
        go
{-# INLINABLE merge #-}

-- | Discards all values going upstream
unitD :: (Monad m) => q -> Proxy x' x y' () m r
unitD _ = go
  where
    go = do
        _ <- respond ()
        go
{-# INLINABLE unitD #-}

-- | Discards all values going downstream
unitU :: (Monad m) => q -> Proxy () x y' y m r
unitU _ = go
  where
    go = do
        _ <- request ()
        go
{-# INLINABLE unitU #-}

{-| Transform a 'Consumer' to a 'Pipe' that reforwards all values further
    downstream
-}
forward :: (Monad m) => (() -> Consumer a m r) -> (() -> Pipe a a m r)
forward p () = evalStateP Nothing $ do
    r <- up >\\ hoist lift (p ())
    ma <- lift get
    case ma of
        Nothing -> return ()
        Just a  -> respond a
    return r
  where
    up () = do
        ma <- lift get
        case ma of
            Nothing -> return ()
            Just a  -> respond a
        a <- request ()
        lift $ put (Just a)
        return a
{-# INLINABLE forward #-}

-- | Transform a unidirectional 'Pipe' to a bidirectional 'Pipe'
generalize :: (Monad m) => (() -> Pipe a b m r) -> x -> Proxy x a x b m r
generalize p x = evalStateP x $ up >\\ hoist lift (p ()) //> dn
  where
    up () = do
        x <- lift get
        request x
    dn a = do
        x <- respond a
        lift $ put x
{-# INLINABLE generalize #-}

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
