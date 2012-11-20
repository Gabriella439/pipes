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
import Control.Proxy.Class (ProxyP(request, respond))
import Control.Proxy.Trans.Identity (runIdentityP, runIdentityK)
import System.IO (Handle, hGetLine, hPutStr, hPutStrLn, hPrint, stdin, stdout)

-- | Get input from 'stdin' one line at a time and send \'@D@\'ownstream
getLineS
 :: (ProxyP p) => y' -> p x' x y' String IO r
getLineS _ = runIdentityP $ forever $ do
    str <- lift getLine
    respond str

-- | Get input from 'stdin' one line at a time and send \'@U@\'pstream
getLineC
 :: (ProxyP p) => y' -> p String x y' y IO r
getLineC _ = runIdentityP $ forever $ do
    str <- lift getLine
    request str

-- | 'read' input from 'stdin' one line at a time and send \'@D@\'ownstream
readLnS
 :: (Read a, ProxyP p) => y' -> p x' x y' a IO r
readLnS _ = runIdentityP $ forever $ do
    a <- lift readLn
    respond a

-- | 'read' input from 'stdin' one line at a time and send \'@U@\'pstream
readLnC
 :: (Read a, ProxyP p) => y' -> p a x y' y IO r
readLnC _ = runIdentityP $ forever $ do
    a <- lift readLn
    request a

{-| 'print's all values flowing through it to 'stdout'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
printB
 :: (Show a, Show a', ProxyP p) => a' -> p a' a a' a IO r
printB = runIdentityK $ foreverK $ \a' -> do
    lift $ do
        putStr "U: "
        print a'
    a <- request a'
    lift $ do
        putStr "D: "
        print a
    respond a

-- | 'print's all values flowing \'@D@\'ownstream to 'stdout'
printD
 :: (Show a, ProxyP p) => x -> p x a x a IO r
printD = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ print a
    respond a

-- | 'print's all values flowing \'@U@\'pstream to 'stdout'
printU
 :: (Show a', ProxyP p) => a' -> p a' x a' x IO r
printU = runIdentityK $ foreverK $ \a' -> do
    lift $ print a'
    x <- request a'
    respond x

{-| 'putStrLn's all values flowing through it to 'stdout'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
putStrLnB
 :: (ProxyP p) => String -> p String String String String IO r
putStrLnB = runIdentityK $ foreverK $ \a' -> do
    lift $ do
        putStr "U: "
        putStrLn a'
    a <- request a'
    lift $ do
        putStr "D: "
        putStrLn a
    respond a

-- | 'putStrLn's all values flowing \'@D@\'ownstream to 'stdout'
putStrLnD
 :: (ProxyP p) => x -> p x String x String IO r
putStrLnD = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ putStrLn a
    respond a

-- | 'putStrLn's all values flowing \'@U@\'pstream to 'stdout'
putStrLnU
 :: (ProxyP p) => String -> p String x String x IO r
putStrLnU = runIdentityK $ foreverK $ \a' -> do
    lift $ putStrLn a'
    x <- request a'
    respond x

-- | Convert 'stdin'/'stdout' into a line-based 'Server'
promptS
 :: (ProxyP p) => String -> p x' x String String IO r
promptS = runIdentityK $ foreverK $ \send -> do
    recv <- lift $ do
        putStrLn send
        getLine
    respond recv

-- | Convert 'stdin'/'stdout' into a line-based 'Client'
promptC
 :: (ProxyP p) => y' -> p String String y' y IO r
promptC _ = runIdentityP $ forever $ do
    send <- lift getLine
    recv <- request send
    lift $ putStrLn recv

-- | Get input from a handle one line at a time and send \'@D@\'ownstream
hGetLineD
 :: (ProxyP p) => Handle -> y' -> p x' x y' String IO r
hGetLineD h _ = runIdentityP $ forever $ do
    str <- lift $ hGetLine h
    respond str

-- | Get input from a handle one line at a time and send \'@U@\'pstream
hGetLineU
 :: (ProxyP p) => Handle -> y' -> p String x y' y IO r
hGetLineU h _ = runIdentityP $ forever $ do
    str <- lift $ hGetLine h
    request str

{-| 'print's all values flowing through it to a 'Handle'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
hPrintB
 :: (Show a, Show a', ProxyP p) => Handle -> a' -> p a' a a' a IO r
hPrintB h = runIdentityK $ foreverK $ \a' -> do
    lift $ do
        hPutStr h "U: "
        hPrint h a'
    a <- request a'
    lift $ do
        hPutStr h "D: "
        hPrint h a
    respond a

-- | 'print's all values flowing \'@D@\'ownstream to a 'Handle'
hPrintD
 :: (Show a, ProxyP p) => Handle -> x -> p x a x a IO r
hPrintD h = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ hPrint h a
    respond a

-- | 'print's all values flowing \'@U@\'pstream to a 'Handle'
hPrintU
 :: (Show a', ProxyP p) => Handle -> a' -> p a' x a' x IO r
hPrintU h = runIdentityK $ foreverK $ \a' -> do
    lift $ hPrint h a'
    x <- request a'
    respond x

{-| 'putStrLn's all values flowing through it to a 'Handle'

    Prefixes upstream values with \"@U: @\" and downstream values with \"@D: @\"
-}
hPutStrLnB
 :: (ProxyP p) => Handle -> String -> p String String String String IO r
hPutStrLnB h = runIdentityK $ foreverK $ \a' -> do
    lift $ do
        hPutStr h "U: "
        hPutStrLn h a'
    a <- request a'
    lift $ do
        hPutStr h "D: "
        hPutStrLn h a
    respond a

-- | 'putStrLn's all values flowing \'@D@\'ownstream to a 'Handle'
hPutStrLnD
 :: (ProxyP p) => Handle -> x -> p x String x String IO r
hPutStrLnD h = runIdentityK $ foreverK $ \x -> do
    a <- request x
    lift $ hPutStrLn h a
    respond a

-- | 'putStrLn's all values flowing \'@U@\'pstream to a 'Handle'
hPutStrLnU
 :: (ProxyP p) => Handle -> String -> p String x String x IO r
hPutStrLnU h = runIdentityK $ foreverK $ \a' -> do
    lift $ hPutStrLn h a'
    x <- request a'
    respond x
