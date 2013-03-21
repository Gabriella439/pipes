{-| 'String'-based 'IO' operations.

    Note that 'String's are very inefficient, and I will release future separate
    packages with 'ByteString' and 'Text' operations.  I only provide these to
    allow users to test simple I/O without requiring additional library
    dependencies.
-}
module Control.Proxy.Prelude.IO (
    -- * Standard I/O

    -- ** Input
    stdinS,
    getLineS,
    getLineC,
    readLnS,
    readLnC,

    -- ** Output
    stdoutD,
    putStrLnD,
    putStrLnU,
    putStrLnB,
    printD,
    printU,
    printB,

    -- * Handle I/O

    -- ** Input

    hGetLineS,
    hGetLineC,

    -- ** Output
    hPrintD,
    hPrintU,
    hPrintB,
    hPutStrLnD,
    hPutStrLnU,
    hPutStrLnB,
    ) where

import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Proxy.Prelude.Kleisli (foreverK)
import Control.Proxy.Class (Proxy(request, respond))
import Control.Proxy.Trans.Identity (runIdentityP, runIdentityK)
import Control.Proxy.Synonym (Producer, CoProducer)
import qualified System.IO as IO

-- | Synonym for 'getLineS'
stdinS :: (Proxy p) => () -> Producer p String IO r
stdinS = getLineS

-- | A 'Producer' that sends lines from 'stdin' downstream
getLineS :: (Proxy p) => () -> Producer p String IO r
getLineS () = runIdentityP $ forever $ do
    str <- lift getLine
    respond str

-- | A 'CoProducer' that sends lines from 'stdin' upstream
getLineC :: (Proxy p) => () -> CoProducer p String IO r
getLineC () = runIdentityP $ forever $ do
    str <- lift getLine
    request str

-- | 'read' input from 'stdin' one line at a time and send \'@D@\'ownstream
readLnS :: (Read b, Proxy p) => () -> Producer p b IO r
readLnS () = runIdentityP $ forever $ do
    a <- lift readLn
    respond a

-- | 'read' input from 'stdin' one line at a time and send \'@U@\'pstream
readLnC :: (Read a', Proxy p) => () -> CoProducer p a' IO r
readLnC () = runIdentityP $ forever $ do
    a <- lift readLn
    request a

-- | Synonym for 'putStrLnD'
stdoutD :: (Proxy p) => x -> p x String x String IO r
stdoutD = putStrLnD

-- | 'putStrLn's all values flowing \'@D@\'ownstream to 'stdout'
putStrLnD :: (Proxy p) => x -> p x String x String IO r
putStrLnD = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ putStrLn a
    respond a

-- | 'putStrLn's all values flowing \'@U@\'pstream to 'stdout'
putStrLnU :: (Proxy p) => String -> p String x String x IO r
putStrLnU = runIdentityK $ foreverK $ \a' -> do
    lift $ putStrLn a'
    x <- request a'
    respond x

{-| 'putStrLn's all values flowing through it to 'stdout'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
putStrLnB :: (Proxy p) => String -> p String String String String IO r
putStrLnB = runIdentityK $ foreverK $ \a' -> do
    lift $ do
        putStr "U: "
        putStrLn a'
    a <- request a'
    lift $ do
        putStr "D: "
        putStrLn a
    respond a

-- | 'print's all values flowing \'@D@\'ownstream to 'stdout'
printD :: (Show a, Proxy p) => x -> p x a x a IO r
printD = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ print a
    respond a

-- | 'print's all values flowing \'@U@\'pstream to 'stdout'
printU :: (Show a', Proxy p) => a' -> p a' x a' x IO r
printU = runIdentityK $ foreverK $ \a' -> do
    lift $ print a'
    x <- request a'
    respond x

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

-- | A 'CoProducer' that sends lines from a 'Handle' upstream
hGetLineC :: (Proxy p) => IO.Handle -> () -> CoProducer p String IO ()
hGetLineC h () = runIdentityP go where
    go = do
        eof <- lift $ IO.hIsEOF h
        if eof
            then return ()
            else do
                str <- lift $ IO.hGetLine h
                request str
                go

-- | 'print's all values flowing \'@D@\'ownstream to a 'Handle'
hPrintD :: (Show a, Proxy p) => IO.Handle -> x -> p x a x a IO r
hPrintD h = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ IO.hPrint h a
    respond a

-- | 'print's all values flowing \'@U@\'pstream to a 'Handle'
hPrintU :: (Show a', Proxy p) => IO.Handle -> a' -> p a' x a' x IO r
hPrintU h = runIdentityK $ foreverK $ \a' -> do
    lift $ IO.hPrint h a'
    x <- request a'
    respond x

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

-- | 'putStrLn's all values flowing \'@D@\'ownstream to a 'Handle'
hPutStrLnD :: (Proxy p) => IO.Handle -> x -> p x String x String IO r
hPutStrLnD h = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ IO.hPutStrLn h a
    respond a

-- | 'putStrLn's all values flowing \'@U@\'pstream to a 'Handle'
hPutStrLnU :: (Proxy p) => IO.Handle -> String -> p String x String x IO r
hPutStrLnU h = runIdentityK $ foreverK $ \a' -> do
    lift $ IO.hPutStrLn h a'
    x <- request a'
    respond x

{-| 'putStrLn's all values flowing through it to a 'Handle'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
hPutStrLnB
    :: (Proxy p) => IO.Handle -> String -> p String String String String IO r
hPutStrLnB h = runIdentityK $ foreverK $ \a' -> do
    lift $ do
        IO.hPutStr h "U: "
        IO.hPutStrLn h a'
    a <- request a'
    lift $ do
        IO.hPutStr h "D: "
        IO.hPutStrLn h a
    respond a
