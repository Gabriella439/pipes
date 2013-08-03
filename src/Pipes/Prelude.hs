{-| General purpose utilities

    The names in this module clash heavily with the Haskell Prelude, so I
    recommend the following import scheme:

> import Pipes
> import qualified Pipes.Prelude as P  -- or use any other qualifier you prefer
-}

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Pipes.Prelude (
    -- * Producers
    -- $producers
    stdin,
    fromHandle,

    -- * Unfolds
    -- $unfolds
    replicate,
    replicateM,
    yieldIf,
    yieldAfter,
    debug,
    read,

    -- * Pipes
    -- $pipes
    take,
    takeWhile,
    drop,
    dropWhile,
    findIndices,
    scanl,
    scanM,

    -- * Folds
    -- $folds
    foldl,
    foldM,
    all,
    any,
    find,
    findIndex,
    head,
    index,
    last,
    null,
    toList,

    -- * Zips
    zip,
    zipWith,

    -- * Utilities
    tee,
    generalize
    ) where

import Control.Monad (liftM, replicateM_, when, unless)
import Control.Monad.Trans.State.Strict (get, put)
import Data.Functor.Identity (Identity, runIdentity)
import Pipes
import Pipes.Core
import Pipes.Internal
import Pipes.Lift (evalStateP)
import qualified System.IO as IO
import Prelude hiding (
    all,
    any,
    drop,
    dropWhile,
    foldl,
    read,
    head,
    last,
    null,
    replicate,
    scanl,
    take,
    takeWhile,
    zip,
    zipWith )

{- $producers
    Use 'for' to iterate over 'Producer's whenever you want to perform the same
    action for every element:

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

    Note that 'String'-based 'IO' is inefficient.  These 'Producer's exist only
    for simple demonstrations without incurring a dependency on the @text@
    package.

    Don't forget about 'each', exported by the main "Pipes" module, if you want
    to transform 'Foldable's (like lists) into 'Producer's.  This is designed to
    resemble foreach notation:

>>> run $ for (each [1..3]) (lift . print)
1
2
3

    You can also build your own custom 'Producer's using 'yield' and the 'Monad'
    \/ 'MonadTrans' instances for 'Producer'.
-}

-- | Read 'String's from 'IO.stdin' using 'getLine'
stdin :: Producer' String IO ()
stdin = fromHandle IO.stdin
{-# INLINABLE stdin #-}

-- | Read 'String's from a 'IO.Handle' using 'IO.hGetLine'
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

{- $unfolds
    An unfold is a 'Producer' which doubles as a stream transformer.

    You use 'for' to apply an unfold to a stream.  This behaves like a
    'concatMap', generating a new 'Producer':

> -- Outputs two copies of every input string
> for P.stdin (P.replicate 2) :: Producer String IO ()

    To apply an additional handler, you can either iterate over the newly minted
    'Producer' using another 'for' loop:

> run $ for (for P.stdin (P.replicate 2)) $ \str -> do
>     lift $ putStrLn str

    ... or you can nest 'for' loops:

> run $
>     for P.stdin $ \str1 -> do
>         for (P.replicate 2 str1) $ \str2 -> do
>             lift $ putStrLn str2

    ... or you can compose the two handlers using ('~>'):

>>> run $ for P.stdin (P.replicate 2 ~> lift . putStrLn)
Test<Enter>
Test
Test
ABC<Enter>
ABC
ABC
...

    All three of the above idioms always behave identically.

    Note that 'each' is also an unfold and can be used to flatten streams of
    'Foldable' elements:

>>> run $ for (each [[1, 2], [4, 5]]) (lift . print)
[1,2]
[3,4]
>>> run $ for (each [[1, 2], [3, 4]]) (each ~> lift . print)
1
2
3
4
>>> run $ (each ~> each ~> lift . print) [[1, 2], [3, 4]]  -- This works, too!
1
2
3
4

-}

-- | @(replicate n a)@ 'yield's the value \'@a@\' a total of \'@n@\' times.
replicate :: (Monad m) => Int -> a -> Producer' a m ()
replicate n a = replicateM_ n (yield a)
{-# INLINABLE replicate #-}

{-| @(replicateM n m)@ calls \'@m@\' a total of \'@n@\' times, 'yield'ing each
    result.
-}
replicateM :: (Monad m) => Int -> m a -> Producer' a m ()
replicateM n m = replicateM_ n $ do
    a <- lift m
    yield a
{-# INLINABLE replicateM #-}

{-| @(yieldIf pred a)@ only re-'yield's @a@ if it satisfies the predicate @p@

    Use 'yieldIf' to filter a stream:

>>> run $ (each ~> yieldIf even ~> lift . print) [1..4]
-}
yieldIf :: (Monad m) => (a -> Bool) -> a -> Producer' a m ()
yieldIf predicate a =
    if (predicate a)
    then do
        _ <- yield a
        return ()
    else return ()
{-# INLINABLE yieldIf #-}

-- | Re-'yield' the value after first applying the given action
yieldAfter :: (Monad m) => (a -> m b) -> a -> Producer' a m ()
yieldAfter f a = do
    _ <- lift (f a)
    _ <- yield a
    return ()
{-# INLINABLE yieldAfter #-}

{-| 'print' values flowing through for debugging purposes

> debug = yieldAfter print
-}
debug :: (Show a) => a -> Producer' a IO ()
debug = yieldAfter print
{-# INLINABLE debug #-}

-- | Parse 'Read'able values, only forwarding the value if the parse succeeds
read :: (Monad m, Read a) => String -> Producer' a m ()
read str = case (reads str) of
    [(a, "")] -> do
        _ <- yield a
        return ()
    _         -> return ()
{-# INLINABLE read #-}

{- $pipes
    Use ('>->') to transform a 'Producer' using a 'Pipe':

>>> run $ for (P.stdin >-> P.takeWhile (/= "quit")) (lift . putStrLn)
Test<Enter>
Test
ABC<Enter>
ABC
quit<Enter>
>>>

    You can also use ('>->') to connect 'Pipe's:

>>> run $ for (P.stdin >-> (P.drop 2 >-> P.take 2)) (lift . putStrLn)
1<Enter>
2<Enter>
3<Enter>
3
4<Enter>
4

    This gives the same behavior as first connecting the 'Producer', because
    ('>->') is associative:

>>> run $ for ((P.stdin >-> P.drop 2) >-> P.take 2) (lift . putStrLn)

    ... so you can drop the parentheses altogether since the meaning is
    unambiguous:

>>> run $ for (P.stdin >-> P.drop 2 >-> P.take 2) (lift . putStrLn)

    You can also connect 'Consumer's this way, too, but most useful 'Consumer's
    are non-trivial and you will find them all in derived @pipes@ libraries
    instead of here.
-}

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
drop = go
  where
    go n =
        if (n <= 0)
        then cat
        else do
            await
            go (n - 1)
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
scanl :: (Monad m) => (b -> a -> b) -> b -> Pipe a b m r
scanl step = loop
  where
    loop b = do
        yield b
        a <- await
        let b' = step b a
        loop $! b'
{-# INLINABLE scanl #-}

-- | Strict, monadic left scan
scanM :: (Monad m) => (b -> a -> m b) -> b -> Pipe a b m r
scanM step = loop
  where
    loop b = do
        yield b
        a  <- await
        b' <- lift (step b a)
        loop $! b'
{-# INLINABLE scanM #-}

{- $folds
    Use these to fold the output of a 'Producer'.  Certain folds will stop
    drawing elements if they can compute their result early, like 'any':

>>> P.any null P.stdin
Test<Enter>
ABC<Enter>
<Enter>
True
>>>

    To run multiple folds at a time, instead use multiple 'WriterT' layers in
    the base monad:

> import Control.Monad.Trans.Class
> import Control.Monad.Trans.Writer.Strict
> import Data.Monoid
> import Pipes
>
> fold :: Effect (WriterT (Sum Int) (Writer Any)) ()
> fold = for (each [1..10]) $ \i -> do
>     lift $ tell $ Sum i
>     lift $ lift $ tell $ Any (i > 5)

    These types of folds will consume the entire 'Producer':

>>> print $ runWriter $ execWriterT $ run fold
(Sum {getSum = 55},Any {getAny = True})

-}

-- | Strict fold of the elements of a 'Producer'
foldl :: (Monad m) => (b -> a -> b) -> b -> Producer a m r -> m b
foldl step b0 p0 = loop p0 b0
  where
    loop p b = case p of
        Request _  fu -> loop (fu ()) b
        Respond a  fu -> loop (fu ()) $! step b a
        M          m  -> m >>= \p' -> loop p' b
        Pure    _     -> return b
{-
    loop p b = do
        x <- next p
        case x of
            Left   _      -> return b
            Right (a, p') -> loop p' $! step b a
-}
{-# INLINABLE foldl #-}

-- | Strict, monadic fold of the elements of a 'Producer'
foldM :: (Monad m) => (b -> a -> m b) -> b -> Producer a m r -> m b
foldM step b0 p0 = loop p0 b0
  where
    loop p b = case p of
        Request _  fu -> loop (fu ()) b
        Respond a  fu -> do
            b' <- step b a
            loop (fu ()) $! b'
        M          m  -> m >>= \p' -> loop p' b
        Pure    _     -> return b
{-
    loop p b = do
        x <- next p
        case x of
            Left   _      -> return b
            Right (a, p') -> do
                b' <- step b a
                loop p' $! b'
-}
{-# INLINABLE foldM #-}

{-| @(all predicate p)@ determines whether all the elements of @p@ satisfy the
    predicate.
-}
all :: (Monad m) => (a -> Bool) -> Producer a m r -> m Bool
all predicate p = null $ for p (yieldIf (not . predicate))
{-# INLINABLE all #-}

{-| @(any predicate p)@ determines whether any element of @p@ satisfies the
    predicate.
-}
any :: (Monad m) => (a -> Bool) -> Producer a m r -> m Bool
any predicate p = liftM not $ null $ for p (yieldIf predicate)
{-# INLINABLE any #-}

-- | Find the first value that satisfies the predicate
find :: (Monad m) => (a -> Bool) -> Producer a m r -> m (Maybe a)
find predicate p = head $ for p (yieldIf predicate)
{-# INLINABLE find #-}

-- | Find the index of the first value that satisfies the predicate
findIndex :: (Monad m) => (a -> Bool) -> Producer a m r -> m (Maybe Int)
findIndex predicate p = head (p >-> findIndices predicate)
{-# INLINABLE findIndex #-}

-- | Retrieve the first value from a 'Producer'
head :: (Monad m) => Producer a m r -> m (Maybe a)
head p = do
    x <- next p
    case x of
        Left   _     -> return Nothing
        Right (a, _) -> return (Just a)
{-# INLINABLE head #-}

-- | Index into a 'Producer'
index :: (Monad m) => Int -> Producer a m r -> m (Maybe a)
index n p = head (p >-> drop n)
{-# INLINABLE index #-}

-- | Retrieve the last value from a 'Producer'
last :: (Monad m) => Producer a m r -> m (Maybe a)
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

-- | Determine if a 'Producer' is empty
null :: (Monad m) => Producer a m r -> m Bool
null p = do
    x <- next p
    return $ case x of
        Left  _ -> True
        Right _ -> False
{-# INLINABLE null #-}

-- | Convert a pure 'Producer' into a list
toList :: Producer a Identity r -> [a]
toList = loop
  where
    loop p = case p of
        Request _ fu -> loop (fu ())
        Respond a fu -> a:loop (fu ())
        M         m  -> loop (runIdentity m)
        Pure    _    -> []
{-# INLINABLE toList #-}

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

{-| Transform a unidirectional 'Pipe' to a bidirectional 'Pipe'

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
