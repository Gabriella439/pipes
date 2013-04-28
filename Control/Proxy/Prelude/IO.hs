{-| 'String'-based 'IO' operations.

    Note that 'String's are very inefficient, and I will release future separate
    packages with 'ByteString' and 'Text' operations.  I only provide these to
    allow users to test simple I/O without requiring additional library
    dependencies.
-}
module Control.Proxy.Prelude.IO (
    -- * Standard I/O
    stdinS,
    readLnS,
    stdoutD,
    printD,
    printU,
    printB,

    -- * Handle I/O
    hGetLineS,
    hPutStrLnD,
    hPrintD,
    hPrintU,
    hPrintB,
    ) where

import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Proxy.Prelude.Kleisli (foreverK)
import Control.Proxy.Class (Proxy(request, respond))
import Control.Proxy.Trans.Identity (runIdentityP, runIdentityK)
import Control.Proxy.Synonym (Producer, CoProducer)
import qualified System.IO as IO

{-| A 'Producer' that sends lines from 'stdin' downstream

> stdinS = hGetLineS stdin
-}
stdinS :: (Proxy p) => () -> Producer p String IO r
stdinS () = runIdentityP $ forever $ do
    str <- lift getLine
    respond str

{-# INLINABLE stdinS #-}

-- | 'read' input from 'stdin' one line at a time and send \'@D@\'ownstream
readLnS :: (Read b, Proxy p) => () -> Producer p b IO r
readLnS () = runIdentityP $ forever $ do
    a <- lift readLn
    respond a
{-# INLINABLE readLnS #-}

{-| 'putStrLn's all values flowing \'@D@\'ownstream to 'stdout'

> stdoutD = hPutStrLnD stdout
-}
stdoutD :: (Proxy p) => x -> p x String x String IO r
stdoutD = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ putStrLn a
    respond a
{-# INLINABLE stdoutD #-}

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

-- | 'print's all values flowing \'@D@\'ownstream to a 'Handle'
hPrintD :: (Show a, Proxy p) => IO.Handle -> x -> p x a x a IO r
hPrintD h = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ IO.hPrint h a
    respond a
{-# INLINABLE hPrintD #-}

-- | 'print's all values flowing \'@U@\'pstream to a 'Handle'
hPrintU :: (Show a', Proxy p) => IO.Handle -> a' -> p a' x a' x IO r
hPrintU h = runIdentityK $ foreverK $ \a' -> do
    lift $ IO.hPrint h a'
    x <- request a'
    respond x
{-# INLINABLE hPrintU #-}

{-| 'print's all values flowing through it to a 'Handle'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
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
