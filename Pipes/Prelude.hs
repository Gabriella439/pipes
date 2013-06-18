-- | General purpose pipes

{-# LANGUAGE RankNTypes #-}

module Pipes.Prelude (
    -- * Producers
    stdin,
    fromHandle,
    readLn,
    fromList,

    -- * Pipes
    map,
    mapM,
    take,
    takeWhile,
    drop,
    dropWhile,
    filter,

    -- * Consumers
    stdout,
    toHandle,
    print,

    -- ** Folds
    fold,
    all,
    any,
    sum,
    product,
    length,
    head,
    last,
    toList,
    foldr,
    foldl',

    -- * Zip
    zip,

    -- * ListT
    each,

    -- * ArrowChoice
    -- $choice
    left,
    right,

    -- * Utilities
    discard,
    forward,
    generalize,
    foreverK
    ) where

import Control.Monad (when, unless, forever, replicateM_)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Control.Monad.Trans.Writer.Strict (WriterT, tell)
import qualified Data.Monoid as M
import qualified System.IO as IO
import Pipes
import Pipes.Internal
import Pipes.Lift (evalStateP)
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

-- | Read 'String's from 'IO.stdin' using 'getLine'
stdin :: () -> Producer String IO ()
stdin = fromHandle IO.stdin
{-# INLINABLE stdin #-}

-- | Read 'String's from a 'IO.Handle' using 'IO.hGetLine'
fromHandle :: IO.Handle -> () -> Producer String IO ()
fromHandle h () = go
  where
    go = do
        eof <- lift $ IO.hIsEOF h
        unless eof $ do
            str <- lift $ IO.hGetLine h
            respond str
            go
{-# INLINABLE fromHandle #-}

-- | 'read' from 'IO.stdin' using 'Prelude.readLn'
readLn :: (Read b) => () -> Producer b IO ()
readLn () = go
  where
    go = do
        eof <- lift $ IO.hIsEOF IO.stdin
        unless eof $ do
            str <- lift Prelude.readLn
            respond str
            go
{-# INLINABLE readLn #-}

-- | Convert a list into a 'Producer'
fromList :: (Monad m) => [b] -> () -> Producer b m ()
fromList bs () = mapM_ respond bs
{-# INLINABLE fromList #-}

-- | Transform all values using a pure function
map :: (Monad m) => (a -> b) -> () -> Pipe a b m r
map f () = forever $ do
    a <- request ()
    respond (f a)
{-# INLINABLE map #-}

-- | Transform all values using a monadic function
mapM :: (Monad m) => (a -> m b) -> () -> Pipe a b m r
mapM f () = forever $ do
    a <- request ()
    b <- lift $ f a
    respond b
{-# INLINABLE mapM #-}

-- | @(take n)@ only allows @n@ values to pass through
take :: (Monad m) => Int -> () -> Pipe a a m ()
take n () = replicateM_ n $ do
    a <- request ()
    respond a
{-# INLINABLE take #-}

{-| @(takeWhile p)@ allows values to pass downstream so long as they satisfy
    the predicate @p@.
-}
takeWhile :: (Monad m) => (a -> Bool) -> () -> Pipe a a m ()
takeWhile predicate () = go
  where
    go = do
        a <- request ()
        when (predicate a) $ do
            respond a
            go
{-# INLINABLE takeWhile #-}

-- | @(drop n)@ discards @n@ values going downstream
drop :: (Monad m) => Int -> () -> Pipe a a m r
drop n () = do
    replicateM_ n $ request ()
    pull ()
{-# INLINABLE drop #-}

{-| @(dropWhile p)@ discards values going downstream until one violates the
    predicate @p@.
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

-- | Write 'String's to 'IO.stdout' using 'putStrLn'
stdout :: () -> Consumer String IO r
stdout = toHandle IO.stdout
{-# INLINABLE stdout #-}

-- | Write 'String's to a 'IO.Handle' using 'IO.hPutStrLn'
toHandle :: IO.Handle -> () -> Consumer String IO r
toHandle handle () = forever $ do
    str <- request ()
    lift $ IO.hPutStrLn handle str
{-# INLINABLE toHandle #-}

-- | 'show' to 'IO.stdout' using 'Prelude.print'
print :: (Show a) => () -> Consumer a IO r
print () = forever $ do
    a <- request ()
    lift $ Prelude.print a
{-# INLINABLE print #-}

-- | Strict fold using the provided 'M.Monoid'
fold :: (Monad m, M.Monoid w) => (a -> w) -> () -> Consumer a (WriterT w m) r
fold f () =  forever $ do
    a <- request ()
    lift $ tell (f a)
{-# INLINABLE fold #-}

{-| Fold that returns whether 'M.All' values satisfy the predicate

    'all' terminates on the first value that fails the predicate
-}
all :: (Monad m) => (a -> Bool) -> () -> Consumer a (WriterT M.All m) ()
all predicate () = go
  where
    go = do
        a <- request ()
        if (predicate a)
            then go
            else lift $ tell (M.All False)
{-# INLINABLE all #-}

{-| Fold that returns whether 'M.Any' value satisfies the predicate

    'any' terminates on the first value that satisfies the predicate
-}
any :: (Monad m) => (a -> Bool) -> () -> Consumer a (WriterT M.Any m) ()
any predicate () = go
  where
    go = do
        a <- request ()
        if (predicate a)
            then lift $ tell (M.Any True)
            else go
{-# INLINABLE any #-}

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

{-| Retrieve the 'M.First' value

    'head' terminates on the first value it receives
-}
head :: (Monad m) => () -> Consumer a (WriterT (M.First a) m) ()
head () = do
    a <- request ()
    lift $ tell $ M.First (Just a)
{-# INLINABLE head #-}

-- | Retrieve the 'M.Last' value
last :: (Monad m) => () -> Consumer a (WriterT (M.Last a) m) r
last = fold (M.Last . Just)
{-# INLINABLE last #-}

-- | Fold input values into a list
toList :: (Monad m) => () -> Consumer a (WriterT [a] m) r
toList = fold (\x -> [x])
{-# INLINABLE toList #-}

{-| Fold equivalent to 'foldr'

    To see why, consider this isomorphic type for 'foldr':

> foldr :: (a -> b -> b) -> [a] -> Endo b
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

-- | Zip two 'Producer's
zip :: (Monad m)
    => (() -> Producer  a     m r)
    -> (() -> Producer     b  m r)
    -> (() -> Producer (a, b) m r)
zip p1_0 p2_0 () = go1 (p1_0 ()) (p2_0 ())
  where
    go1 p1 p2 = M (do
        x <- step p1
        case x of
            Left r         -> return (Pure r)
            Right (a, p1') -> return (go2 p1' p2 a) )
    go2 p1 p2 a = M (do
        x <- step p2
        case x of
            Left r         -> return (Pure r)
            Right (b, p2') -> return (Respond (a, b) (\_ -> go1 p1 p2')) )
    step p = case p of
        Request _ fa  -> step (fa ())
        Respond b fb' -> return (Right (b, fb' ()))
        Pure    r     -> return (Left r)
        M         m   -> m >>= step
{-# INLINABLE zip #-}

-- | Non-deterministically choose from all values in the given list
each :: (Monad m) => [b] -> ListT m b
each bs = RespondT (fromList bs ())
{-# INLINABLE each #-}

{- $choice
    'left' and 'right' satisfy the 'ArrowChoice' laws using
    @arr f = generalize (map f)@.
-}

-- | Lift a proxy to operate only on 'Left' values and forward 'Right' values
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

-- | Lift a proxy to operate only on 'Right' values and forward 'Left' values
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

-- | Discards all values going downstream
discard :: (Monad m) => () -> Consumer a m r
discard () = go
  where
    go = do
        _ <- request ()
        go
{-# INLINABLE discard #-}

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
generalize p x0 = evalStateP x0 $ up >\\ hoist lift (p ()) //> dn
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
