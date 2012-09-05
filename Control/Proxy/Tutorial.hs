-- | This module provides the tutorial for "Control.Proxy"

module Control.Proxy.Tutorial (
    -- * Basics
    -- $basics

    -- * Types
    -- $types

    -- * Composition
    -- $composition

    -- * Idioms
    -- $idioms

    -- * Pipe Compatibility
    -- $pipes

    -- * Future extensions
    -- $future
    ) where

import Control.Monad.Trans.Class
import Control.Proxy

{- $basics
    The 'Proxy' type models composable chains of client-server interactions.

    An 'Proxy' is a monad transformer that extends the base monad with the
    ability to 'request' input from its upstream interface and 'respond' with
    output to requests on its downstream interface.

    For example, consider the following contrived remote procedure call
    'Server':

> import Control.Proxy
> import Control.Monad.Trans
>
> incrementer :: Int -> Server Int Int IO r
> incrementer question = do
>     lift $ putStrLn $ "Server received : " ++ show question
>     let answer = question + 1
>     lift $ putStrLn $ "Server responded: " ++ show answer
>     nextQuestion <- respond answer
>     incrementer nextQuestion

    We can understand what the 'Server' does just by looking at the type:

>        | Question | Answer | Base monad | Return value
> Server   Int        Int        IO           r

    Our 'Server' receives questions about 'Int's, and responds with answers that
    are 'Int's.  The base monad is 'IO' because our 'Server' 'lift's two
    'putStrLn' statements to chat out loud.

    Now we can write a 'Client' that interacts with our 'Server':

> import Control.Monad
>
> oneTwoThree :: () -> Client Int Int IO ()
> oneTwoThree () = forM_ [1, 2, 3] $ \question -> do
>     lift $ putStrLn $ "Client requested: " ++ show question
>     answer <- request question
>     lift $ putStrLn $ "Client received : " ++ show answer
>     lift $ putStrLn "*"

    Again, the type explains what the 'Client' does:

>        | Question | Answer | Base monad | Return value
> Client   Int      | Int    | IO         | ()

    Our 'Client' asks questions about 'Int's and receives answers that are
    'Int's.  The client also uses 'IO' as the base monad.

    We can then compose the 'Client' and 'Server' into a 'Session' using the
    ('<-<') operator:

> session :: () -> Session IO ()
> session = oneTwoThree <-< incrementer

    The 'Session' type indicates that we have a self-contained session that we
    can run in the 'IO' monad.  We run it using the the 'runSession' function:

>>> runSession session :: IO ()
Client requested: 1
Server received : 1
Server responded: 2
Client received : 2
*
Client requested: 2
Server received : 2
Server responded: 3
Client received : 3
*
Client requested: 3
Server received : 3
Server responded: 4
Client received : 4
*

    Now, let's add an intermediate 'Proxy' between the 'Client' and 'Server'
    that subtly tampers with the stream going through it:

> malicious :: Int -> Proxy Int Int Int Int IO r
> malicious question = do
>     question' <- if (question > 2)
>                  then do
>                      lift $ putStrLn "MUAHAHAHA!"
>                      return (question + 1)
>                  else return question
>     answer <- request question'
>     nextQuestion <- respond answer
>     malicious nextQuestion

    The type tells us what our 'Proxy' does:

>       | Upstream (Server) | Downstream (Client) |
>       | Question | Answer | Question |  Answer  | Base monad | Return value
> Proxy   Int        Int      Int         Int       IO           r

    A 'Proxy' bridges two separate interfaces.  The first two parameters define
    the upstream interface (i.e. in the 'Server' direction) and the second two
    parameters define the downstream interface (i.e. in the 'Client' direction).

    We can see if our proxy does its job correctly:

>>> runSession $ oneTwoThree <-< malicious <-< incrementer
Client requested: 1
Server received : 1
Server responded: 2
Client received : 2
*
Client requested: 2
Server received : 2
Server responded: 3
Client received : 3
*
Client requested: 3
MUAHAHAHA!
Server received : 4
Server responded: 5
Client received : 5
*

    We can also add more proxies as we see fit:

>>> runSession $ oneTwoThree <-< malicious <-< malicious <-< incrementer 
Client requested: 1
Server received : 1
Server responded: 2
Client received : 2
*
Client requested: 2
Server received : 2
Server responded: 3
Client received : 3
*
Client requested: 3
MUAHAHAHA!
MUAHAHAHA!
Server received : 5
Server responded: 6
Client received : 6
*
-}

{- $types
    You probably noticed something odd: ('<-<') seems to be composing values of
    different types.  Sometimes it composes a 'Server' or a 'Client' or a
    'Proxy'.  In reality, though, both 'Server' and 'Client' are just type
    synonyms for special cases of 'Proxy':

> type Server arg ret = Proxy Void  () arg  ret
> type Client arg ret = Proxy  arg ret  () Void

    A 'Server' is just a 'Proxy' that has no upstream interface, and a 'Client'
    is just a 'Proxy' that has no downstream interface.  In fact, 'Session' is
    a 'Proxy', too:

> type Session        = Proxy Void  ()  () Void

    A 'Session' is just a 'Proxy' that has neither an upstream interface nor a
    downstream interface.

    The 'Proxy' is the unifying type of the module that all other types derive
    from and ('<-<') always composes two 'Proxy's and returns a new 'Proxy' of
    the correct type.

    You also probably noticed another odd thing: we parametrize every 'Proxy'
    on its initial argument:

>                +- Initial Arg
>                |
>                v
> incrementer :: Int -> Server         Int Int IO r
> malicious   :: Int -> Proxy  Int Int Int Int IO r
> oneTwoThree :: ()  -> Client Int Int         IO ()
>
> session     :: ()  -> Session                IO ()

    This initial input is what properly initializes each 'Proxy' and always
    corresponds to the input received from the downstream interface, which you
    can see if you expand out the 'Server' and 'Client' type synonyms:

>                +- Initial Arg = This -+
>                |                      |
>                v                      v
> incrementer :: Int -> Proxy  Void ()  Int Int  IO r
> malicious   :: Int -> Proxy  Int  Int Int Int  IO r
> oneTwoThree :: ()  -> Proxy  Int  Int ()  Void IO ()
>
> session     :: ()  -> Proxy  Void ()  ()  Void IO ()

    Every 'Proxy' receives its initial argument through a parameter (otherwise
    we cannot initialize it correctly), but receives all subsequent arguments
    from 'respond' statements (if any).

    A 'Client' receives an empty argument (i.e. @()@) so it can be initialized
    at any time, and a client cannot 'respond' so it never receives any
    subsequent arguments.

    Both a 'Proxy' and 'Server' use their parameter to receive their first
    request and then receive all further requests by binding the result of the
    'respond' command.

    This means that the actual types you compose are all of the form:

> proxy :: b' -> Proxy a' a b' b m r
-}

{- $composition
    'Proxy' composition posseses an identity 'Proxy' that is completely
    transparent to anything upstream or downstream of it:

> idT :: (Monad m) => arg -> Proxy arg ret arg ret m r
> idT question = do
>     answer       <- request question
>     nextQuestion <- respond answer
>     idT nextQuestion

    Formally, this means that:

> idT <-< p = p
>
> p <-< idT = p

    Also, 'Proxy' composition has the nice property that it behaves exactly the
    same way no matter how you group components:

> (p1 <-< p2) <-< p3 = p1 <-< (p2 <-< p3)

    This means that ('<-<') and 'idT' define a category, and the above equations
    are the category laws.  These laws guarantee the following nice
    properties of components:

    * You can reason about each component's behavior independently of other
      components

    * You don't encounter boundary cases between components

    * You don't encounter edge cases at the 'Server' or 'Client' ends

    The semantics of 'Proxy' composition are simple:

    * 'request' blocks until it receives a response from upstream

    * 'respond' blocks until it receives a new request from downstream

    * If any 'Proxy' in the chain 'return's, the entire chain terminates
-}

{- $idioms
    We frequently encounter the following recurring pattern when writing
    'Proxy's:

> someProxy arg = do
>     ...
>     arg' <- respond x
>     someProxy arg'

    "Control.Proxy" provides the 'foreverK' utility function which abstracts
    away this manual recursion:

> foreverK f = \a -> do
>     a' <- f a
>     foreverK f a'
>
> -- or: foreverK f = f >=> foreverK f

    Using 'foreverK', we can simplify the definition of 'incrementer':

> incrementer = foreverK $ \question -> do
>     lift $ putStrLn $ "Server received : " ++ show question
>     let answer = question + 1
>     lift $ putStrLn $ "Server responded: " ++ show answer
>     respond answer

    ... which looks exactly like the way you might write server code in another
    programming language.

    We can similarly simplify 'malicious' this way:

> malicious = foreverK $ \question -> do
>     question' <- if (question > 2)
>                  then do
>                      lift $ putStrLn "MUAHAHAHA!"
>                      return (question + 1)
>                  else return question
>     answer <- request question'
>     respond answer

    ... or 'idT':

> idT = foreverK $ \question -> do
>     answer <- request question
>     respond answer
>
> -- or: idT = foreverK (request >=> respond)
> --         = request >=> respond >=> request >=> respond >=> ...
-}

{- $pipes
    'Proxy's generalize 'Pipe's by permitting communication upstream.
    Fortunately, though, you don't need to rewrite your code if you have already
    used 'Pipe's.  "Control.Proxy" formulates all of the 'Pipe' types and
    primitives in terms of the 'Proxy' type

    This means that if you wish to upgrade your 'Pipe' code to take advantage of
    upstream communication, you only need to import "Control.Proxy" instead
    of "Control.Pipe" and everything will still work out of the box.

    To understand how 'Pipe's map onto 'Proxy's, just check out the 'Pipe'
    definition in "Control.Proxy":

> type Pipe a b = Proxy () a () b

    In other words, a 'Pipe' is just a 'Proxy' where you never pass any
    parameters upstream.

    "Control.Pipe" will not be deprecated, however, and will be preserved for
    users who do not wish to communicate information upstream.
-}
