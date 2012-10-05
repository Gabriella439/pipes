{-| 'String'-based 'IO' operations.

    Note that 'String's are very inefficient, and I will release future separate
    packages with 'ByteString' and 'Text' operations.  I only provide these to
    allow users to test simple I/O without requiring additional library
    dependencies.
-}

module Control.Proxy.Prelude.IO (
    -- * Standard I/O
    -- ** Input
    getLineS,
    getLineC,
    readLnS,
    readLnC,
    -- ** Output
    printB,
    printD,
    printU,
    putStrLnB,
    putStrLnD,
    putStrLnU,
    -- ** Interaction
    promptS,
    promptC,
    -- * Handle I/O
    -- ** Input
    hGetLineD,
    hGetLineU,
    -- ** Output
    hPrintB,
    hPrintD,
    hPrintU,
    hPutStrLnB,
    hPutStrLnD,
    hPutStrLnU
    ) where

import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Proxy.Prelude.Kleisli (foreverK)
import Control.Proxy.Core (Proxy, Client, Server)
import Control.Proxy.Class (request, respond)
import System.IO (Handle, hGetLine, hPutStr, hPutStrLn, hPrint, stdin, stdout)

-- | Get input from 'stdin' one line at a time and send \'@D@\'ownstream
getLineS :: () -> Server () String IO r
getLineS () = forever $ do
    str <- lift getLine
    respond str

-- | Get input from 'stdin' one line at a time and send \'@U@\'pstream
getLineC :: () -> Client String () IO r
getLineC () = forever $ do
    str <- lift getLine
    request str

-- | 'read' input from 'stdin' one line at a time and send \'@D@\'ownstream
readLnS :: (Read a) => () -> Server () a IO r
readLnS () = forever $ do
    a <- lift readLn
    respond a

-- | 'read' input from 'stdin' one line at a time and send \'@U@\'pstream
readLnC :: (Read a) => () -> Client a () IO r
readLnC () = forever $ do
    a <- lift readLn
    request a

{-| 'print's all values flowing through it to 'stdout'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
printB :: (Show a, Show a') => a' -> Proxy a' a a' a IO r
printB = foreverK $ \a' -> do
    lift $ do
        putStr "U: "
        print a'
    a <- request a'
    lift $ do
        putStr "D: "
        print a
    respond a

-- | 'print's all values flowing \'@D@\'ownstream to 'stdout'
printD :: (Show a) => x -> Proxy x a x a IO r
printD = foreverK $ \x -> do
    a <- request x
    lift $ print a
    respond a

-- | 'print's all values flowing \'@U@\'pstream to 'stdout'
printU :: (Show a') => a' -> Proxy a' x a' x IO r
printU = foreverK $ \a' -> do
    lift $ print a'
    x <- request a'
    respond x

{-| 'putStrLn's all values flowing through it to 'stdout'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
putStrLnB :: String -> Proxy String String String String IO r
putStrLnB = foreverK $ \a' -> do
    lift $ do
        putStr "U: "
        putStrLn a'
    a <- request a'
    lift $ do
        putStr "D: "
        putStrLn a
    respond a

-- | 'putStrLn's all values flowing \'@D@\'ownstream to 'stdout'
putStrLnD :: x -> Proxy x String x String IO r
putStrLnD = foreverK $ \x -> do
    a <- request x
    lift $ putStrLn a
    respond a

-- | 'putStrLn's all values flowing \'@U@\'pstream to 'stdout'
putStrLnU :: String -> Proxy String x String x IO r
putStrLnU = foreverK $ \a' -> do
    lift $ putStrLn a'
    x <- request a'
    respond x

-- | Convert 'stdin'/'stdout' into a line-based 'Server'
promptS :: String -> Server String String IO r
promptS = foreverK $ \send -> do
    recv <- lift $ do
        putStrLn send
        getLine
    respond recv

-- | Convert 'stdin'/'stdout' into a line-based 'Client'
promptC :: () -> Client String String IO r
promptC () = forever $ do
    send <- lift getLine
    recv <- request send
    lift $ putStrLn recv

-- | Get input from a handle one line at a time and send \'@D@\'ownstream
hGetLineD :: Handle -> () -> Server () String IO r
hGetLineD h () = forever $ do
    str <- lift $ hGetLine h
    respond str

-- | Get input from a handle one line at a time and send \'@U@\'pstream
hGetLineU :: Handle -> () -> Client String () IO r
hGetLineU h () = forever $ do
    str <- lift $ hGetLine h
    request str

{-| 'print's all values flowing through it to a 'Handle'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
hPrintB :: (Show a, Show a') => Handle -> a' -> Proxy a' a a' a IO r
hPrintB h = foreverK $ \a' -> do
    lift $ do
        hPutStr h "U: "
        hPrint h a'
    a <- request a'
    lift $ do
        hPutStr h "D: "
        hPrint h a
    respond a

-- | 'print's all values flowing \'@D@\'ownstream to a 'Handle'
hPrintD :: (Show a) => Handle -> x -> Proxy x a x a IO r
hPrintD h = foreverK $ \x -> do
    a <- request x
    lift $ hPrint h a
    respond a

-- | 'print's all values flowing \'@U@\'pstream to a 'Handle'
hPrintU :: (Show a') => Handle -> a' -> Proxy a' x a' x IO r
hPrintU h = foreverK $ \a' -> do
    lift $ hPrint h a'
    x <- request a'
    respond x

{-| 'putStrLn's all values flowing through it to a 'Handle'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
hPutStrLnB :: Handle -> String -> Proxy String String String String IO r
hPutStrLnB h = foreverK $ \a' -> do
    lift $ do
        hPutStr h "U: "
        hPutStrLn h a'
    a <- request a'
    lift $ do
        hPutStr h "D: "
        hPutStrLn h a
    respond a

-- | 'putStrLn's all values flowing \'@D@\'ownstream to a 'Handle'
hPutStrLnD :: Handle -> x -> Proxy x String x String IO r
hPutStrLnD h = foreverK $ \x -> do
    a <- request x
    lift $ hPutStrLn h a
    respond a

-- | 'putStrLn's all values flowing \'@U@\'pstream to a 'Handle'
hPutStrLnU :: Handle -> String -> Proxy String x String x IO r
hPutStrLnU h = foreverK $ \a' -> do
    lift $ hPutStrLn h a'
    x <- request a'
    respond x
