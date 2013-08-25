{-| General purpose utilities

    The names in this module clash heavily with the Haskell Prelude, so I
    recommend the following import scheme:

> import Pipes
> import qualified Pipes.Prelude as P  -- or use any other qualifier you prefer

    Note that 'String'-based 'IO' is inefficient.  The 'String'-based utilities
    in this module exist only for simple demonstrations without incurring a
    dependency on the @text@ package.

    Also, 'stdin' and 'stdout' remove and add newlines, respectively.  This
    behavior is intended to simplify examples.  The upcoming 'ByteString' and
    'Text' utilities for @pipes@ will preserve newlines.
-}

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Pipes.Prelude (
    -- * Producers
    -- $producers
    stdin,
    fromHandle,

    -- * Consumers
    -- $consumers
    stdout,
    toHandle,

    -- * Pipes
    -- $pipes
    map,
    mapM,
    filter,
    take,
    takeWhile,
    drop,
    dropWhile,
    concat,
    findIndices,
    scan,
    scanM,
    chain,
    read,
    show,

    -- * Folds
    -- $folds
    fold,
    foldM,
    all,
    any,
    find,
    findIndex,
    head,
    index,
    last,
    length,
    null,
    toList,
    toListM,

    -- * Zips
    zip,
    zipWith,

    -- * Utilities
    tee,
    generalize
    ) where

import Control.Exception (throwIO, try)
import Control.Monad (liftM, replicateM_, when, unless)
import Control.Monad.Trans.State.Strict (get, put)
import Data.Functor.Identity (Identity, runIdentity)
import qualified GHC.IO.Exception as G
import Pipes
import Pipes.Core
import Pipes.Internal
import Pipes.Lift (evalStateP)
import qualified System.IO as IO
import qualified Prelude
import Prelude hiding (
    all,
    any,
    concat,
    drop,
    dropWhile,
    filter,
    head,
    last,
    length,
    map,
    mapM,
    null,
    read,
    show,
    take,
    takeWhile,
    zip,
    zipWith )

{- $producers
    Use 'for' loops to iterate over 'Producer's whenever you want to perform the
    same action for every element:

> -- Echo all lines from standard input to standard output
> run $ for P.stdin $ \str -> do
>     lift $ putStrLn str

    ... or more concisely:

>>> run $ for P.stdin (lift . putStrLn)
Test<Enter>
Test
ABC<Enter>
ABC
...

-}

{-| Read 'String's from 'IO.stdin' using 'getLine'

    Terminates on end of input
-}
stdin :: Producer' String IO ()
stdin = fromHandle IO.stdin
{-# INLINABLE stdin #-}

{-| Read 'String's from a 'IO.Handle' using 'IO.hGetLine'

    Terminates on end of input
-}
fromHandle :: IO.Handle -> Producer' String IO ()
fromHandle h = go
  where
    go = do
        eof <- lift $ IO.hIsEOF h
        unless eof $ do
            str <- lift $ IO.hGetLine h
            yield str
            go
{-# INLINABLE fromHandle #-}

{- $consumers
    Feed a 'Consumer' the same value repeatedly using ('>~'):

>>> run $ lift getLine >~ P.stdout
Test<Enter>
Test
ABC<Enter>
ABC
...

-}

{-| Write 'String's to 'IO.stdout' using 'putStrLn'

    Terminates on a broken output pipe
-}
stdout :: Consumer' String IO ()
stdout = toHandle IO.stdout
{-# INLINABLE stdout #-}

{-| Write 'String's to a 'IO.Handle' using 'IO.hPutStrLn'

    Terminates on a broken output pipe
-}
toHandle :: IO.Handle -> Consumer' String IO ()
toHandle handle = do
    loop
  where
    loop = do
        str <- await
        x   <- lift $ try $ IO.hPutStrLn handle str
        case x of
            Left e@(G.IOError { G.ioe_type = t}) ->
                lift $ unless (t == G.ResourceVanished) $ throwIO e
            Right () -> loop
{-# INLINABLE toHandle #-}

{- $pipes
    Use ('>->') to connect 'Producer's, 'Pipe's, and 'Consumer's:

>>> run $ P.stdin >-> P.takeWhile (/= "quit") >-> P.stdout
Test<Enter>
Test
ABC<Enter>
ABC
quit<Enter>
>>>

-}

-- | Apply a function to all values flowing downstream
map :: (Monad m) => (a -> b) -> Pipe a b m r
map f = for cat (yield . f)
{-# INLINABLE map #-}

-- | Apply a monadic function to all values flowing downstream
mapM :: (Monad m) => (a -> m b) -> Pipe a b m r
mapM f = for cat $ \a -> do
    b <- lift (f a)
    yield b
{-# INLINABLE mapM #-}

-- | @(filter predicate)@ only forwards values that satisfy the predicate.
filter :: (Monad m) => (a -> Bool) -> Pipe a a m r
filter predicate = for cat $ \a -> when (predicate a) (yield a)
{-# INLINABLE filter #-}

-- | @(take n)@ only allows @n@ values to pass through
take :: (Monad m) => Int -> Pipe a a m ()
take n = replicateM_ n $ do
    a <- await
    yield a
{-# INLINABLE take #-}

{-| @(takeWhile p)@ allows values to pass downstream so long as they satisfy
    the predicate @p@.
-}
takeWhile :: (Monad m) => (a -> Bool) -> Pipe a a m ()
takeWhile predicate = go
  where
    go = do
        a <- await
        if (predicate a)
            then do
                yield a
                go
            else return ()
{-# INLINABLE takeWhile #-}

-- | @(drop n)@ discards @n@ values going downstream
drop :: (Monad m) => Int -> Pipe a a m r
drop n = do
    replicateM_ n await
    cat
{-# INLINABLE drop #-}

{-| @(dropWhile p)@ discards values going downstream until one violates the
    predicate @p@.
-}
dropWhile :: (Monad m) => (a -> Bool) -> Pipe a a m r
dropWhile predicate = go
  where
    go = do
        a <- await
        if (predicate a)
            then go
            else do
                yield a
                cat
{-# INLINABLE dropWhile #-}

-- | Flatten all 'Foldable' elements flowing downstream
concat :: (Monad m, Foldable f) => Pipe (f a) a m r
concat = for cat each
{-# INLINABLE concat #-}

-- | Outputs the indices of all elements that satisfied the predicate
findIndices :: (Monad m) => (a -> Bool) -> Pipe a Int m r
findIndices predicate = loop 0
  where
    loop n = do
        a <- await
        when (predicate a) (yield n)
        loop $! n + 1
{-# INLINABLE findIndices #-}

-- | Strict left scan
scan :: (Monad m) => (x -> a -> x) -> x -> (x -> b) -> Pipe a b m r
scan step begin done = loop begin
  where
    loop x = do
        yield (done x)
        a <- await
        let x' = step x a
        loop $! x'
{-# INLINABLE scan #-}

-- | Strict, monadic left scan
scanM :: (Monad m) => (x -> a -> m x) -> m x -> (x -> m b) -> Pipe a b m r
scanM step begin done = do
    x <- lift begin
    loop x
  where
    loop x = do
        b <- lift (done x)
        yield b
        a  <- await
        x' <- lift (step x a)
        loop $! x'
{-# INLINABLE scanM #-}

-- | Apply an action to all values flowing downstream
chain :: (Monad m) => (a -> m ()) -> Pipe a a m r
chain f = for cat $ \a -> do
    lift (f a)
    yield a
{-# INLINABLE chain #-}

-- | Parse 'Read'able values, only forwarding the value if the parse succeeds
read :: (Monad m, Read a) => Pipe String a m r
read = for cat $ \str -> case (reads str) of
    [(a, "")] -> yield a
    _         -> return ()
{-# INLINABLE read #-}

-- | Convert 'Show'able values to 'String's
show :: (Monad m, Show a) => Pipe a String m r
show = map Prelude.show
{-# INLINABLE show #-}

{- $folds
    Use these to fold the output of a 'Producer'.  Many of these folds will stop
    drawing elements if they can compute their result early, like 'any':

>>> P.any null P.stdin
Test<Enter>
ABC<Enter>
<Enter>
True
>>>

-}

-- | Strict fold of the elements of a 'Producer'
fold :: (Monad m) => (x -> a -> x) -> x -> (x -> b) -> Producer a m () -> m b
fold step begin done p0 = loop p0 begin
  where
    loop p x = case p of
        Request _  fu -> loop (fu ()) x
        Respond a  fu -> loop (fu ()) $! step x a
        M          m  -> m >>= \p' -> loop p' x
        Pure    _     -> return (done x)
{-# INLINABLE fold #-}

-- | Strict, monadic fold of the elements of a 'Producer'
foldM
    :: (Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> Producer a m () -> m b
foldM step begin done p0 = do
    x0 <- begin
    loop p0 x0
  where
    loop p x = case p of
        Request _  fu -> loop (fu ()) x
        Respond a  fu -> do
            x' <- step x a
            loop (fu ()) $! x'
        M          m  -> m >>= \p' -> loop p' x
        Pure    _     -> done x
{-# INLINABLE foldM #-}

{-| @(all predicate p)@ determines whether all the elements of @p@ satisfy the
    predicate.
-}
all :: (Monad m) => (a -> Bool) -> Producer a m () -> m Bool
all predicate p = null $ for p $ \a -> when (not $ predicate a) (yield a)
{-# INLINABLE all #-}

{-| @(any predicate p)@ determines whether any element of @p@ satisfies the
    predicate.
-}
any :: (Monad m) => (a -> Bool) -> Producer a m () -> m Bool
any predicate p = liftM not $ null $ for p $ \a -> when (predicate a) (yield a)
{-# INLINABLE any #-}

-- | Find the first value that satisfies the predicate
find :: (Monad m) => (a -> Bool) -> Producer a m () -> m (Maybe a)
find predicate p = head $ for p  $ \a -> when (predicate a) (yield a)
{-# INLINABLE find #-}

-- | Find the index of the first value that satisfies the predicate
findIndex :: (Monad m) => (a -> Bool) -> Producer a m () -> m (Maybe Int)
findIndex predicate p = head (p >-> findIndices predicate)
{-# INLINABLE findIndex #-}

-- | Retrieve the first value from a 'Producer'
head :: (Monad m) => Producer a m () -> m (Maybe a)
head p = do
    x <- next p
    case x of
        Left   _     -> return Nothing
        Right (a, _) -> return (Just a)
{-# INLINABLE head #-}

-- | Index into a 'Producer'
index :: (Monad m) => Int -> Producer a m () -> m (Maybe a)
index n p = head (p >-> drop n)
{-# INLINABLE index #-}

-- | Retrieve the last value from a 'Producer'
last :: (Monad m) => Producer a m () -> m (Maybe a)
last p0 = do
    x <- next p0
    case x of
        Left   _      -> return Nothing
        Right (a, p') -> loop a p'
  where
    loop a p = do
        x <- next p
        case x of
            Left   _       -> return (Just a)
            Right (a', p') -> loop a' p'
{-# INLINABLE last #-}

-- | Count the number of elements in a 'Producer'
length :: (Monad m) => Producer a m () -> m Int
length = fold (\n _ -> n + 1) 0 id
{-# INLINABLE length #-}

-- | Determine if a 'Producer' is empty
null :: (Monad m) => Producer a m () -> m Bool
null p = do
    x <- next p
    return $ case x of
        Left  _ -> True
        Right _ -> False
{-# INLINABLE null #-}

-- | Convert a pure 'Producer' into a list
toList :: Producer a Identity () -> [a]
toList = loop
  where
    loop p = case p of
        Request _ fu -> loop (fu ())
        Respond a fu -> a:loop (fu ())
        M         m  -> loop (runIdentity m)
        Pure    _    -> []
{-# INLINABLE toList #-}

{-| Convert an effectful 'Producer' into a list

    Note: 'toListM' is not an idiomatic use of @pipes@, but I provide it for
    simple testing purposes.  Idiomatic @pipes@ style consumes the elements
    immediately as they are generated instead of loading all elements into
    memory.
-}
toListM :: (Monad m) => Producer a m () -> m [a]
toListM = loop
  where
    loop p = case p of
        Request _ fu -> loop (fu ())
        Respond a fu -> do
            as <- loop (fu ())
            return (a:as)
        M         m  -> m >>= loop
        Pure    _    -> return []
{-# INLINABLE toListM #-}

-- | Zip two 'Producer's
zip :: (Monad m)
    => (Producer   a     m r)
    -> (Producer      b  m r)
    -> (Producer' (a, b) m r)
zip = zipWith (,)
{-# INLINABLE zip #-}

-- | Zip two 'Producer's using the provided combining function
zipWith :: (Monad m)
    => (a -> b -> c)
    -> (Producer  a m r)
    -> (Producer  b m r)
    -> (Producer' c m r)
zipWith f = go
  where
    go p1 p2 = do
        e1 <- lift $ next p1
        case e1 of
            Left r         -> return r
            Right (a, p1') -> do
                e2 <- lift $ next p2
                case e2 of
                    Left r         -> return r
                    Right (b, p2') -> do
                        yield (f a b)
                        go p1' p2'
{-# INLINABLE zipWith #-}

{-| Transform a 'Consumer' to a 'Pipe' that reforwards all values further
    downstream
-}
tee :: (Monad m) => Consumer a m r -> Pipe a a m r
tee p = evalStateP Nothing $ do
    r <- up >\\ (hoist lift p //> dn)
    ma <- lift get
    case ma of
        Nothing -> return ()
        Just a  -> yield a
    return r
  where
    up () = do
        ma <- lift get
        case ma of
            Nothing -> return ()
            Just a  -> yield a
        a <- await
        lift $ put (Just a)
        return a
    dn _ = return ()
{-# INLINABLE tee #-}

{-| Transform a unidirectional 'Pipe' to a bidirectional 'Proxy'

> generalize (f >-> g) = generalize f >+> generalize g
>
> generalize cat = pull
-}
generalize :: (Monad m) => Pipe a b m r -> x -> Proxy x a x b m r
generalize p x0 = evalStateP x0 $ up >\\ hoist lift p //> dn
  where
    up () = do
        x <- lift get
        request x
    dn a = do
        x <- respond a
        lift $ put x
{-# INLINABLE generalize #-}
