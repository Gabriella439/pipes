-- | This module provides the tutorial for "Control.Proxy".

module Control.Pipe.Tutorial (
    -- * Introduction
    -- $intro

    -- * Type Synonyms
    -- $synonyms

    -- * Request and Respond
    -- $interact

    -- * Composition
    -- $composition

    -- * Running Proxies
    -- $run

    -- * The Proxy Class
    -- $class

    -- * Composition
    -- $compose

    -- * Modularity
    -- $modular

    -- * Vertical Concatenation
    -- $vertical

    -- * Return Values
    -- $return

    -- * Termination
    -- $terminate

    -- * Folds
    -- $folds

    -- * Resource Management
    -- $resource

    -- * Bidirectional Flow
    -- $bidirectional
    ) where

-- For documentation
import Control.Category
import Control.Monad.Trans.Class
import Control.Proxy

{- $intro
    The @pipes@ library encompasses many types of streaming abstractions, all of
    which are special cases of \"proxies\".

    Let's begin with the simplest 'Proxy': a 'Producer'.  The following
    'Producer' generates a stream of 'Int's from user input:

> import Control.Monad
> import Control.Monad.Trans.Class (lift)
> import Control.Proxy
> 
> -- Ignore the 'runIdentityP' and '()' for now
>
> promptInt :: (Proxy p) => () -> Producer p Int IO r
> promptInt () = runIdentityP $ forever $ do
>     lift $ putStrLn "Enter an Integer:"
>     n <- lift readLn
>     respond n

    The next simplest 'Proxy' is a 'Consumer'.  The following 'Consumer'
    'request's a stream of 'Show'able values and 'print's them:

> printer :: (Show a, Proxy p) => () -> Consumer p a IO r
> printer () = runIdentityP $ forever $ do
>     a <- request ()
>     lift $ putStrLn "Received a value:"
>     lift $ print a

    You can compose a 'Consumer' and a 'Producer' using ('>->'), which produces
    a runnable 'Session':

> promptInt >-> printer :: (Proxy p) => () -> Session p IO r

    ... and you run the 'Session' using 'runProxy':

>>> runProxy $ promptInt >-> printer :: IO r
Enter an Integer:
1<Enter>
Received a value:
1
Enter an Integer:
5<Enter>
Received a value:
5
...

    This proceeds endlessly until we hit @Ctrl-C@ to interrupt it.

    We would like to limit the number of iterations, so lets define an
    intermediate 'Proxy' that behaves like a verbose 'take'.  I will call it a
    'Pipe' since values flow through it:

> take' :: (Proxy p) => Int -> () -> Pipe p a a IO ()
> take' n () = runIdentityP $ do
>     replicateM_ n $ do
>         a <- request ()
>         respond a
>     lift $ putStrLn "You shall not pass!"

    This 'Pipe' forwards the first @n@ values it receives undisturbed, then it
    outputs a cute message.  You can compose it between our 'Consumer' and
    'Producer' using ('>->'):

>>> runProxy $ promptInt >-> take' 2 >-> printer
Enter an Integer:
9<Enter>
Received a value:
9
Enter an Integer:
2<Enter>
Received a value:
2
You shall not pass!

    However, so far we've only defined proxies that send information downstream
    in the direction of the ('>->') arrow.  We need not limit ourselves to
    unidirectional communication and we can enhance these proxies with the
    ability to send information upstream with each 'request' that determines
    how upstream stages 'respond'.

    For example, 'Client's generalize 'Consumer's because they can supply an
    argument other than @()@ with each 'request'.  The following 'Client'
    sends three 'request's upstream, each of which provides an 'Int' @argument@
    and expects a 'Bool' @result@:

> threeReqs :: (Proxy p) => () -> Client p Int Bool IO ()
> threeReqs () = runIdentityP $ forM_ [1, 3, 1] $ \argument -> do
>     lift $ putStrLn $ "Client Sends:   " ++ show argument
>     result <- request argument
>     lift $ putStrLn $ "Client Receives:" ++ show result
>     lift $ putStrLn $ if result then "Success" else "Failure"
>     lift $ putStrLn "*"

    Notice how 'Client's use \"@request argument@\" instead of
    \"@request ()@\".  This sends \"@argument@\" upstream to parametrize the
    'request'.

    'Server's similarly generalize 'Producer's because they receive arguments
    other than @()@.  The following 'Server' receives 'Int' 'request's and
    'respond's whether or not the 'Int' was greater than two:

> comparer :: (Proxy p) => Int -> Server p Int Bool IO r
> comparer = runIdentityK loop where
>     loop argument = do
>         lift $ putStrLn $ "Server Receives:" ++ show argument
>         let result = argument > 2
>         lift $ putStrLn $ "Server Sends:   " ++ show result
>         nextArgument <- respond result
>         loop nextArgument

    Notice how 'Server's receive their first argument as a parameter and bind
    each subsequent argument using 'respond'.

    You can compose a 'Client' and 'Server' using ('>->'), and this also returns
    a runnable 'Session':

> comparer >-> threeReqs :: (Proxy p) => () -> Session p IO ()

    Running this executes the client-server session:

>>> runProxy $ comparer >-> threeReqs :: IO ()
Client Sends:    1
Server Receives: 1
Server Sends:    False
Client Receives: False
Failure
*
Client Sends:    3
Server Receives: 3
Server Sends:    True
Client Receives: True
Success
*
Client Sends:    1
Server Receives: 1
Server Sends:    False
Client Receives: False
Failure
*

    'Proxy's generalize 'Pipe's because they can make non-empty 'request's to
    upstream and they receive non-empty arguments from downstream.  The
    following 'Proxy' caches 'request's to reduce the load on the 'Server' if
    the request is a repeat:

> import qualified Data.Map as M
>
> -- 'p' is the Proxy, as the (Proxy p) constraint indicates
>
> cache :: (Proxy p, Ord key) => key -> p key val key val IO r
> cache = runIdentityK (go M.empty) where
>     go m key = case M.lookup key m of
>         Nothing -> do
>             val  <- request key
>             key2 <- respond val
>             go (M.insert key val m) key2
>         Just val -> do
>             lift $ putStrLn "Used cache!"
>             key2 <- respond val
>             go m key2

    You can compose the @cache@ 'Proxy' between our 'Client' and 'Server' using
    ('>->'):

>>> runProxy $ comparer >-> cache >-> threeReqs
Client Sends:    1
Server Receives: 1
Server Sends:    False
Client Receives: False
Failure
*
Client Sends:    3
Server Receives: 3
Server Sends:    True
Client Receives: True
Success
*
Client Sends:    1
Used cache!
Client Receives: False
Failure
*

    This bidirectional flow of information separates @pipes@ from other
    streaming libraries which only handle a unidirectional flow of information.
    These other libraries cannot model 'Client's, 'Server's, or 'Proxy's. -}

{- $synonyms
    You might wonder why ('>->') accepts 'Producer's, 'Consumer's, 'Pipe's,
    'Client's, 'Server's, and 'Proxy's.  These all type-check because they are
    all type synonyms built on top of the central 'Proxy' type class:

> -- The most general type
> (Proxy p) => p a' a b' b m r

    Like the name suggests, a 'Proxy' exposes two interfaces: an upstream
    interface and a downstream interface.  Each interface can both send and
    receive values:

> Upstream | Downstream
>     +---------+
>     |         |
> a' <==       <== b'
>     |  Proxy  |
> a  ==>       ==> b
>     |         |
>     +---------+

    Instances of the 'Proxy' type class are monad transformers that enrich the
    base monad with the ability to send or receive values upstream or
    downstream:

>   | Sends    | Receives | Receives   | Sends      | Base  | Return
>   | Upstream | Upstream | Downstream | Downstream | Monad | Value
> p   a'         a          b'           b            m       r

    We can selectively close certain inputs or outputs to generate specialized
    proxies.  A 'Producer' is a 'Proxy' that can only output values to its
    downstream interface:

> -- The 'C' type is uninhabited, so it 'C'loses an output end
>
> Upstream | Downstream
>     +----------+
>     |          |
> C  <==        <== ()
>     | Producer |
> () ==>        ==> b
>     |          |
>     +----------+
>
> type Producer p b m r = p C () () b m r

    A 'Consumer' is a 'Proxy' that can only receive values on its upstream
    interface:

> Upstream | Downstream
>     +----------+
>     |          |
> () <==        <== ()
>     | Consumer |
> a  ==>        ==> C
>     |          |
>     +----------+
>
> type Consumer p b m r = p () a () C m r

    A 'Pipe' is a 'Proxy' that can only receive values on its upstream interface
    and send values on its downstream interface:

> Upstream | Downstream
>     +--------+
>     |        |
> () <==      <== ()
>     |  Pipe  |
> a  ==>      ==> b
>     |        |
>     +--------+
>
> type Pipe p a b m r = p () a () b m r

    When we compose proxies, the type system ensures sure that their input and
    output types match:

>       promptInt    >->    take' 2    >->    printer
>
>     +-----------+       +---------+       +---------+
>     |           |       |         |       |         |
> C  <==         <== ()  <==       <== ()  <==       <== ()
>     |           |       |         |       |         |
>     | promptInt |       | take' 2 |       | printer |
>     |           |       |         |       |         |
> () ==>         ==> Int ==>       ==> Int ==>       ==> C
>     |           |       |         |       |         |
>     +-----------+       +---------+       +---------+

    Composition fuses these into a new 'Proxy' that has both ends closed, which
    is a 'Session':

>     +-----------------------------------+
>     |                                   |
> C  <==                                 <== ()
>     |                                   |
>     | promptInt >-> take' 2 >-> printer |
>     |                                   |
> () ==>                                 ==> C
>     |                                   |
>     +-----------------------------------+
>
> type Session p m r = p C () () C m r

    A 'Client' is a 'Proxy' that only uses its upstream interface:

> Upstream | Downstream
>     +----------+
>     |          |
> a' <==        <== ()
>     |  Client  |
> a  ==>        ==> C
>     |          |
>     +----------+
>
> type Client p a' a m r = p a' a () C m r

    A 'Server' is a 'Proxy' that only uses its downstream interface:


> Upstream | Downstream
>     +----------+
>     |          |
> C  <==        <== b'
>     |  Server  |
> () ==>        ==> b
>     |          |
>     +----------+
>
> type Server p b' b m r = p C () b' b m r

    The compiler ensures that the types match when we compose 'Client's,
    'Server's, and 'Proxy's.

>        comparer   >->     cache   >->      threeReqs
>
>     +----------+        +-------+        +-----------+
>     |          |        |       |        |           |
> C  <==        <== Int  <==     <== Int  <==         <== ()
>     |          |        |       |        |           |
>     | comparer |        | cache |        | threeReqs |
>     |          |        |       |        |           |
> () ==>        ==> Bool ==>     ==> Bool ==>         ==> C
>     |          |        |       |        |           |
>     +----------+        +-------+        +-----------+

    This similarly fuses into a 'Session':

>     +----------------------------------+
>     |                                  |
> C  <==                                <== ()
>     |                                  |
>     | comparer >-> cache >-> threeReqs |
>     |                                  |
> () ==>                                ==> C
>     |                                  |
>     +----------------------------------+

    @pipes@ encourages substantial code reuse by implementing all abstractions
    as type synonyms on top of a single type class: 'Proxy'.  This makes your
    life easier because:

    * You only use one composition operator: ('>->')

    * You can mix multiple abstractions together as long as the types check -}

{- $interact
    There are only two ways to interact with other 'Proxy's: 'request' and
    'respond'.  Let's examine their type signatures to understand how they
    work:

> request :: (Monad m, Proxy p) => a' -> p a' a b' b m a
>                                  ^                   ^
>                                  |                   |
>                       Argument --+          Result --+

    'request' sends an argument of type @a'@ upstream, and binds a result of
    type @a@.  Whenever you 'request', you block until upstream 'respond's with
    a value.


> respond :: (Monad m, Proxy p) => b -> p a' a b' b m b'
>                                  ^                  ^
>                                  |                  |
>                         Result --+  Next Argument --+

    'respond' replies with a result of type @b@, and then binds the /next/
    argument of type @b'@.  Whenever you 'respond', you block until downstream
    'request's a new value.

    Wait, if 'respond' always binds the /next/ argument, where does the /first/
    argument come from?  Well, it turns out that every 'Proxy' receives its
    initial argument as an ordinary parameter.
   
    We can see this if we take all the previous proxies we defined and fully
    expand every type synonym:

>                                          These
>                                    +--  Columns  ---+
>                                    |     Match      |
>                                    v                v
> promptInt :: (Proxy p)          => ()  -> p C   ()  ()  Int  IO r
> printer   :: (Proxy p, Show a)  => ()  -> p ()  a   ()  C    IO r
> take'     :: (Proxy p)   => Int -> ()  -> p ()  a   ()  a    IO ()
> comparer  :: (Proxy p)          => Int -> p C   ()  Int Bool IO r
> cache     :: (Proxy p, Ord key) => key -> p key val key val  IO r

    You can also study the type of composition, which insists that each proxy
    takes an initial argument matching their third type parameter:

> (>->) :: (Monad m, Proxy p)
>  => (b' -> p a' a b' b m r)
>  -> (c' -> p b' b c' c m r)
>  -> (c' -> p a' a c' c m r)
>      ^            ^
>      |   These    |
>      +---Match----+

    The initial argument each 'Proxy' accepts always matches the third type
    parameter: the input received on the downstream interface.  I've marked this
    input on the following diagram:

> Upstream | Downstream
>     +---------+
>     |         |
> a' <==       <== b' << Entry point
>     |  Proxy  |
> a  ==>       ==> b
>     |         |
>     +---------+

    In fact, one invariant of all 'Session's is that there is always exactly one
    'Proxy' active at all times and every 'Proxy' in the 'Session' is either:

    * The currently active 'Proxy'

    * Downstream of the active 'Proxy', blocked on a 'request'

    * Upstream of the active 'Proxy', blocked on a 'respond'

    Conceptually, every 'Proxy' is initially blocked on a 'respond' so their
    initial argument has the same type as the return value of a 'respond'
    command.  This reflects the fact that the entry point of a 'Session' begins
    downstream of every 'Proxy'.

>       Blocked          Blocked
>         on               on
>       respond          respond
>     +---------+      +---------+
>     |         |      |         |
> a' <==       <== b' <==       <== c' << Entry Point
>     |  Proxy  |      |  Proxy  |
> a  ==>       ==> b  ==>       ==> c
>     |         |      |         |
>     +---------+      +---------+

    We can trivially initialize 'Producer's, 'Consumer's, 'Pipe's, and
    'Client's with @()@ since they don't require any useful input on their
    downstream interface.  However, 'Server's and 'Proxy's cannot begin until
    they receive their first argument, which composition supplies from the next
    stage downstream.

    'Server's and 'Proxy's aren't the only abstractions that require this
    initial argument.  "Control.Proxy.Synonym" defines the full constellation of
    type synonyms, including the dual 'CoProducer', 'CoConsumer', and 'CoPipe'
    synonyms, where information only flows upstream.  These three dual
    constructions require an initial argument as well. -}

{- $composition
    When we compose @(p1 >-> p2)@, composition ensures that @p1@'s downstream
    interface matches @p2@'s upstream interface.  This follows from the type of
    ('>->'):

> (>->) :: (Monad m, Proxy p)
>  => (b' -> p a' a b' b m r)  -- Proxy from (a', a) to (b', b)
>  -> (c' -> p b' b c' c m r)  -- Proxy from (b', b) to (c', c)
>  -> (c' -> p a' a c' c m r)  -- Proxy from (a', a) to (c', c)

    Diagramatically, this looks like:

>         p1     >->      p2
>
>     +--------+      +--------+
>     |        |      |        |
> a' <==      <== b' <==      <== c'
>     |   p1   |      |   p2   |
> a  ==>      ==> b  ==>      ==> c
>     |        |      |        |
>     +--------+      +--------+

    @p1@'s downstream @(b', b)@ interface matches @p2@'s upstream @(b', b)@
    interface, so composition connects them on this shared interface.  This
    fuses away the @(b', b)@ interface, leaving behind @p1@'s upstream @(a', a)@
    interface and @p2@'s downstream @(c', c)@ interface:

>     +-----------------+
>     |                 |
> a' <==               <== c'
>     |   p1  >->  p2   |
> a  ==>               ==> c
>     |                 |
>     +-----------------+

    Proxy composition has the very nice property that it is associative, meaning
    that it behaves the exact same way no matter how you group composition:

> (p1 >-> p2) >-> p3 = p1 >-> (p2 >-> p3)

    ... so you can safely elide the parentheses:

> p1 >-> p2 >-> p3

    Also, we can define a \'@T@\'ransparent 'Proxy' that auto-forwards values
    both ways:

> idT :: (Monad m, Proxy p) => a' -> p a' a a' a m r
> idT = runIdentityK go where
>     go a' = do
>         a   <- request a'
>         a'2 <- respond a
>         go a'2

    Diagramatically, it looks like:

>     +-----+
>     |     |
> a' <==   <== a'
>     | idT |
> a  ==>   ==> a
>     |     |
>     +-----+

    Transparency means that:

> idT >-> p = p
>
> p >-> idT = p

    In other words, 'idT' is an identity of composition.

    This means that proxies form a true 'Category' where ('>->') is composition
    and 'idT' is the identity.   The associativity law and the two
    identity laws are just the 'Category' laws.

    These 'Category' laws guarantee the following important properties:

    * You can reason about each proxy's behavior independently of other proxies

    * You don't encounter weird behavior at the boundary between components

    * You don't encounter corner cases at the 'Server' or 'Client' ends of a
     'Session' -}

{- $run
    All instance of the 'Proxy' type class are monad transformers.  One such
    instance is the 'ProxyFast' type, which provides a very fast implementation
    of the 'Proxy' type class:

> instance Proxy ProxyFast where ...

    Like many other monad transformers, you convert a 'Proxy' back to the base
    monad using some sort of \"@run...@\" function.  In this case, it's the
    'runProxy' function:

> runProxy :: (Monad m) => (() -> ProxyFast a' () () b m r)

    'runProxy' accepts any 'Proxy' that has trivially satisfiable inputs of type
    @()@ and just discards any out-going values:

>     +----------+
>     |          |
> a' <==        <== ()
>     |          |
>     | Runnable |
>     |          |
> () ==>        ==> b
>     |          |
>     +----------+
>
> -- 'runProxy' discards any outgoing a' and b values

    'runProxy' accepts 'Session's since their inputs are trivially satisfiable:

>     +---------+
>     |         |
> C  <==       <== ()
>     |         |
>     | Session |
>     |         |
> () ==>       ==> C
>     |         |
>     +---------+

    However, it also accepts 'Producer's, for example, since their inputs are
    also trivially satisfiable:

>     +----------+
>     |          |
> C  <==        <== ()
>     |          |
>     | Producer |
>     |          |
> () ==>        ==> b
>     |          |
>     +----------+

>>> runProxy $ promptInt
Enter an Integer:
77<Enter>
Enter an Integer:
8<Enter>
Enter an Integer:
9<Enter>
...

    'runProxy' discards by default because it significantly improves the
    reusability of components.  Without this
-}

{- $stdlib
    "Control.Proxy" re-exports "Control.Proxy.Prelude" which provides the
    \"Proxy Prelude\": a standard library of useful utility functions.

    Many of the proxies we wrote already exist in the standard library, such as
    'printD', which generalizes @printer@:

> printD :: (Proxy p, Show a) => x -> p x a x a IO r
> 

    These utilities not only simplify many common tasks but also provide code
    examples that you can consult when learning to define your own proxies.
-}

{- 
    'Producer's, 'Consumer's and 'Pipe's only send information downstream, but
    the more general proxy type permits information flow upstream.  To see how,
    let's inspect the types of the 'request' and 'respond' primitives:

> request :: (Monad m, Proxy p) => a' -> p a' a b' b m a

    'request' requires an argument of type @a'@ that it sends upstream.  It
    then receives a result of type @a@.  'request' corresponds to the upstream
    interface of a proxy:

> Upstream |
>     +-----
>     |  request
> a' <== argument
>     |
> a  ==> request
>     |  result
>     +-----

    'respond' is the dual of 'request':

> respond :: (Monad m, Proxy p) => b -> p a' a b' b m b'

    'respond' answers a downstream request with a value of type @b@, and then
    binds the next request's argument of type @b'@.  'respond' corresponds to
    the downstream interface of a proxy:

>      | Downstream
>      -----+
>     next  |
> argument <== b'
>           |
>  current ==> b
>   result  |
>      -----+

    If @b'@ is always the /next/ argument, where does the proxy get its
    /first/ argument?  We pass the first argument as an ordinary parameter of
    type @b'@:

> b' -> p a' a b' b m r

    This is why all the initial examples require an extra @()@ parameter.  We
    can see this if we expand out their type synonyms, where the initial
    argument always matches the third type parameter:

>                                   +-- These  --+
>                                   |   Match    |
>                                   v            v
> promptInt :: (Proxy p)         => () -> p C () () Int IO r
> printer   :: (Proxy p, Show a) => () -> p () a () C   IO r
> take'     :: (Proxy p)  => Int -> () -> p () a () a   IO ()

    We can expand the type synonyms for @oneTwoThree@ and @incrementer@ to
    verify that they also take an initial argument corresponding to the input
    on their downstream interface:

>                                      +--   These    --+
>                                      |     Match      |
>                                      v                v
> incrementer :: (Monad m, Proxy p) => Int -> p C   ()  Int Int m r
> oneTwoThree :: (Monad m, Proxy p) => ()  -> p Int Int ()  C   m r

    Since they are still proxies under the hood, we compose them together using
    the same mechanism

> printer () = runIdentityP $ forever $ do
>     a <- request () -- <== '()' sent upstream
>     ...

 which is why the examples 

"Control.Proxy.Synonym" defines even more type synonyms, including ones that
-}

{- $class
    All of our proxy code programs generically over the 'Proxy' type class,
    which defines the three central operations of this library's API:

    * ('>->'): Proxy composition

    * 'request': Request input from upstream

    * 'respond': Respond with output to downstream

    @pipes@ defines everything in terms of these three operations, which is
    why all the library's utilities are polymorphic over the 'Proxy' type class.

    Let's look at some example instances of the 'Proxy' type class:

> instance Proxy ProxyFast     -- Fastest implementation
> instance Proxy ProxyCorrect  -- Strict monad transformer laws

    These two proxy types are the two alternative base implementations.  As the
    names suggest, the 'ProxyFast' implementation is faster but the
    'ProxyCorrect' implementation has a monad transformer implementation that is
    correct by construction.

    "Control.Proxy" automatically selects the fast implementation for you, but
    you can always select the correct implementation instead by replacing
    "Control.Proxy" with the following two imports:

> import Control.Proxy.Core    -- Everything except the base implementation
> import Control.Proxy.Correct -- The base implementation

    These are not the only 'Proxy' instances, though!  This library defines
    several extensions to the base 'Proxy' type, all of which also implement the
    'Proxy' type class:

> instance (Proxy p) => Proxy (IdentityP p) -- Like IdentityT
> instance (Proxy p) => Proxy (MaybeP    p) -- Like MaybeT
> instance (Proxy p) => Proxy (EitherP e p) -- Like EitherT
> instance (Proxy p) => Proxy (StateP  s p) -- Like StateT

    These extensions behave like monad transformers, except they also correctly
    lift proxy operations over the monad transformer.

    These are not the only extensions possible and I plan on releasing more as
    separate packages.  I included these extensions in the main library because
    they are simple and illustrate how to define your extensions.  Also, these
    extensions solve many common issues that @pipes@ users face.

    The 'Proxy' type class lets you define your own base implementations or
    extensions while still sharing standard utility functions.  This encourages
    substantial code reuse and preserves a unified API for the @pipes@ ecosystem
    that plays nicely with extensions.
-}

{- $compose
    Like many other monad transformers, you convert the 'Pipe' monad back to the
    base monad using some sort of \"@run...@\" function.  In this case, it's the
    'runPipe' function:

> runPipe :: (Monad m) => Pipeline m r -> m r

    'runPipe' only works on self-contained 'Pipeline's, but you don't need to
    worry about explicitly type-restricting any of your 'Pipe's.  Self-contained
    'Pipeline's will automatically have polymorphic input and output ends and
    they will type-check when you provide them to 'runPipe'.

    Let's try using 'runPipe':

>>> runPipe pipeline
1
2
3
You shall not pass!

    Fascinating!  Our 'Pipe' terminates even though @printer@ never terminates
    and @fromList@ never terminates when given an infinite list.  To illustrate
    why our 'Pipe' terminates, let's outline the 'Pipe' flow control rules for
    composition:

    * 'Pipe's are lazy, so execution begins at the most downstream 'Pipe'
      (@printer@ in our example).

    * When a 'Pipe' 'await's, it blocks until it receives input from the next
      'Pipe' upstream

    * When a 'Pipe' 'yield's, it blocks until it receives a new 'await' request
      from downstream.

    * If a 'Pipe' terminates, it terminates every other 'Pipe' composed with it.

    All of these flow control rules uniquely follow from the 'Category' laws.

    It might surprise you that termination brings down the entire 'Pipeline'
    until you realize that:

    * Downstream 'Pipe's depending on the result from the terminated 'Pipe'
      cannot proceed

    * Upstream 'Pipe's won't be further evaluated because the terminated 'Pipe'
      will not request any further input from them

    So in our previous example, the 'Pipeline' terminated because \"@take' 3@\"
    terminated and brought down the entire 'Pipeline' with it.

    Actually, these flow control rules will mislead you into thinking that
    composed 'Pipe's behave as a collection of sub-'Pipe's with some sort of
    message passing architecture between them, but nothing could be further from
    the truth! When you compose 'Pipe's, they automatically fuse into a single
    'Pipe' that corresponds to how you would have written the control flow by
    hand.

    For example, if you compose @printer@ and @fromList@:

> printer <+< fromList [1..]

    The result is indistinguishable from:

> lift (mapM_ print [1..])

    ... which is what we would have written by hand if we had not used 'Pipe's
    at all!  All 'runPipe' does is just remove the 'lift'!
-}

{- $modular
    Given a loop like:

> loop :: IO r
> loop = forever $ do
>     x <- dataSource
>     y <- processData x
>     dataSink y

    We could decompose it into three separate parts:

> stage1 :: Producer a IO r
> stage1 = forever $ do
>     x <- dataSource
>     yield x
>
> stage2 :: Pipe a b IO r
> stage2 = forever $ do
>     x <- await
>     y <- processData x
>     yield y
>
>
> stage3 :: Consumer b IO r
> stage3 = forever $ do
>     y <- await
>     dataSink y
>
> stage3 <+< stage2 <+< stage1 = lift loop

    In other words, 'Pipe's let you decompose loops into modular components,
    which promotes loose coupling and allows you to freely mix and match those
    components.

    To demonstrate this, let's define a new data source that indefinitely
    prompts the user for integers:

> prompt :: Producer Int IO a
> prompt = forever $ do
>     lift $ putStrLn "Enter a number: "
>     n <- read <$> lift getLine
>     yield n

    Now we can use it as a drop-in replacement for @fromList@:

>>> runPipe $ printer <+< take' 3 <+< prompt
Enter a number:
1<Enter>
1
Enter a number:
2<Enter>
2
Enter a number:
3<Enter>
3
You shall not pass!

-}

{- $vertical
    You can easily \"vertically\" concatenate 'Pipe's, 'Producer's, and
    'Consumer's, all using simple monad sequencing: ('>>').  For example, here
    is how you concatenate 'Producer's:

>>> runPipe $ printer <+< (fromList [1..3] >> fromList [10..12])
1
2
3
10
11
12

    Here's how you would concatenate 'Consumer's:

>>> let print' n = printer <+< take' n :: (Show a) => Int -> Consumer a IO ()
>>> runPipe $ (print' 3 >> print' 4) <+< fromList [1..]
1
2
3
You shall not pass!
4
5
6
7
You shall not pass!

   ... but the above example is gratuitous because we could have just
   concatenated the intermediate @take'@ 'Pipe':

>>> runPipe $ printer <+< (take' 3 >> take' 4) <+< fromList [1..]
1
2
3
You shall not pass!
4
5
6
7
You shall not pass!

-}

{- $return
    'Pipe' composition imposes an important requirement: You can only compose
    'Pipe's that have the same return type.  For example, I could write the
    following function:

> deliver :: (Monad m) => Int -> Consumer a m [a]
> deliver n = replicateM n await

    ... and I might try to compose it with @fromList@:

>>> runPipe $ deliver 3 <+< fromList [1..10] -- wrong!

    ... but this wouldn't type-check, because @fromList@ has a return type of
    @()@ and @deliver@ has a return type of @[Int]@.  Composition requires that
    every 'Pipe' has a return value ready in case it terminates first.

    Fortunately, we don't have to rewrite the @fromList@ function because we can
    just add a return value using vertical concatenation:

>>> runPipe $ deliver 3 <+< (fromList [1..10] >> return [])
[1,2,3]

    ... although a more idiomatic Haskell version would be:

>>> runPipe $ (Just <$> deliver 3) <+< (fromList [1..10] *> pure Nothing)
Just [1,2,3]

    This forces you to cover all code paths by thinking about what return value
    you would provide if something were to go wrong.  For example, let's say I
    were to make a mistake and request more input than @fromList@ can deliver:

>>> runPipe $ (Just <$> deliver 99) <+< (fromList [1..10] *> pure Nothing)
Nothing

    The type system saved me by forcing me to cover all corner cases and handle
    every way my program could terminate.
-}

{- $terminate

    Now what if you wanted to write a 'Pipe' that only reads from its input end
    (i.e. a 'Consumer') and returns a list of every value delivered to it when
    its input 'Pipe' terminates?

> toList :: (Monad m) => Consumer a m [a]
> toList = ???

    You can't write such a 'Pipe' because if its input terminates then it brings
    down @toList@ with it!  This is correct because @toList@ as defined is not
    compositional (yet!).

    To see why, let's say you somehow got @toList@ to work and the following
    imaginary code sample worked:

>>> runPipe $ toList <+< (fromList [1..5] >> return [])
[1,2,3,4,5]

    @toList@ is defined to return its value when the 'Pipe' immediately upstream
    (@fromList@ in this case) terminates.  This behavior immediately leads to a
    problem.  What if I were to insert an \"identity\" 'Pipe' between @toList@
    and @fromList@:

> identity = forever $ await >>= yield
> -- This is how id is actually implemented!

    This 'Pipe' forwards every valued untouched, so we would expect it to not
    have any affect if we were to insert it in the middle:

>>> runPipe $ toList <+< identity <+< (fromList [1..5] >> return [])
??? -- Oops! Something other than [1,2,3,4,5], perhaps even non-termination

    The answer couldn't be @[1,2,3,4,5]@ because @toList@ would monitor 
    @identity@ instead of @fromList@ and since @identity@ never terminates
    @toList@ never terminates.  This is what I mean when I say that @toList@'s
    specified behavior is non-compositional.  It only works if it is coupled
    directly to the desired 'Pipe' and breaks when you introduce intermediate
    stages.

    Note that a terminated 'Pipe' only brings down 'Pipe's composed with it.  To
    illustrate this, let's use the following example:

> p = do a <+< b
>        c

    @a@, @b@, and @c@ are 'Pipe's, and @c@ shares the same input and output as
    the composite 'Pipe' @a <+< b@, otherwise we cannot combine them within the
    same monad.  In the above example, either @a@ or @b@ could terminate and
    bring down the other one since they are composed, but @c@ is guaranteed to
    continue after @a <+< b@ terminates because it is not composed with them.
    Conceptually, we can think of this as @c@ automatically taking over the
    'Pipe''s channeling responsibilities when @a <+< b@ can no longer continue.
    There is no need to \"restart\" the input or output manually as in some
    other iteratee libraries.

    The @pipes@ library, unlike other iteratee libraries, grounds its vertical
    and horizontal concatenation in category theory by deriving horizontal
    concatenation ('.') from its 'Category' instance and vertical concatenation
    ('>>') from its 'Monad' instance.  This makes it easier to reason about
    'Pipe's because you can leverage your intuition about 'Category's and
    'Monad's to understand their behavior.  The only 'Pipe'-specific primitives
    are 'await' and 'yield'.
-}

{- $folds
    While we cannot intercept termination, we can still fold our input.  We can
    embed 'WriterT' in our base monad, since 'Pipe' is a monad transformer, and
    store the result in the monoid:

> toList :: Consumer a (WriterT [a] m) r
> toList = forever $ do
>     a <- await
>     lift $ tell [a]

>>> execWriterT $ runPipe $ toList <+< fromList [1..4]
[1,2,3,4]

    But what if other pipes have a base monad that is not compatible, such as:

> prompt3 :: Producer Int IO a
> prompt3 = take' 3 <+< prompt

    That's okay, because we can transparently 'lift' any Pipe's base monad,
    using 'hoistFreeT' from @Control.Monad.Trans.Free@ in the @free@ package:

>>> execWriterT $ runPipe $ toList <+< hoistFreeT lift prompt3
3<Enter>
4<Enter>
6<Enter>
[3,4,6]

-}

{- $resource
    Pipes handle streaming computations well, but do not handle resource
    management well.  To see why, let's say we have the file \"@test.txt@\"
    with the following contents:

> Line 1
> Line 2
> Line 3

  .. and we wish to lazily read one line at a time from it:

> readFile' :: Handle -> Producer Text IO ()
> readFile' h = do
>     eof <- lift $ hIsEOF h
>     when (not eof) $ do
>         s <- lift $ hGetLine h
>         yield s
>         readFile' h

    We could then try to be slick and write a lazy version that only reads as
    many lines as we request:

> read' :: FilePath -> Producer Text IO ()
> read' file = do
>     lift $ putStrLn "Opening file ..."
>     h <- lift $ openFile file ReadMode
>     readFile' h
>     lift $ putStrLn "Closing file ..."
>     lift $ hClose h

    Now compose!

>>> runPipe $ printer <+< read' "test.xt"
Opening file ...
"Line 1"
"Line 2"
"Line 3"
Closing file ...

    So far, so good.  Equally important, the file is never opened if we replace
    @printer@ with a 'Pipe' that never demands input:

>>> runPipe $ (lift $ putStrLn "I don't need input") <+< read' "test.txt"
I don't need input

    There is still one problem, though. What if we wrote:

>>> runPipe $ printer <+< take' 2 <+< read' "test.txt"
Opening file ...
"Line 1"
"Line 2"
You shall not pass!

    Oh no!  While it was lazy and only read two lines from the file, it was also
    too lazy to properly close our file!  \"@take' 2@\" terminated before
    @read'@, preventing @read'@ from properly closing \"test.txt\".  This is why
    'Pipe' composition fails to guarantee deterministic finalization.

    The "Control.Frame" module of this library provides a temporary solution to
    this problem, but in the longer run there will be a more elegant solution
    built on top of "Control.Proxy".
-}

{- $bidirectional
    The 'Pipe' type suffers from one restriction: it only handles a
    unidirectional flow of information.  If you want a bidirectional 'Pipe'
    type, then use the 'Proxy' type from "Control.Proxy", which generalizes the
    'Pipe' type to bidirectional flow.

    More importantly, the 'Proxy' type is a strict superset of the 'Pipe' type,
    so all 'Pipe' utilities and extensions are actually written as 'Proxy'
    utilities and extensions, in order to avoid code duplication.

    So if you want to use these extensions, import "Control.Proxy" instead,
    which exports a backwards compatible 'Pipe' implementation along with all
    utilities and extensions.  The 'Pipe' implementation in "Control.Pipe.Core"
    exists purely as a reference implementation for people who wish to study the
    simpler 'Pipe' type when building their own iteratee libraries.
-}
