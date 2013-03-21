{-| This module provides a brief introductory tutorial in the \"Introduction\"
    section followed by a lengthy discussion of the library's design and idioms.

    I've condensed all the code examples into a self-contained code block in the
    Appendix section that you can use to follow along.
-}

module Control.Proxy.Tutorial (
    -- * Introduction
    -- $intro

    -- * Bidirectionality
    -- $bidir

    -- * Type Synonyms
    -- $synonyms

    -- * Request and Respond
    -- $interact

    -- * Composition
    -- $composition

    -- * The Proxy Class
    -- $class

    -- * Interleaving Effects
    -- $interleave

    -- * Mixing Base Monads
    -- $hoist

    -- * Utilities
    -- $utilities

    -- * Mix Monads and Composition
    -- $mixmonadcomp

    -- * ListT
    -- $listT

    -- * Folds
    -- $folds

    -- * Resource Management
    -- $resource

    -- * Extensions
    -- $extend

    -- * Error handling
    -- $error

    -- * Local state
    -- $state

    -- * Branching, zips, and merges
    -- $branch

    -- * Proxy Transformers
    -- $proxytrans

    -- * Proxy Morphism Laws
    -- $proxymorph

    -- * Functors on Proxies
    -- $proxyfunctor

    -- * Conclusion
    -- $conclusion

    -- * Appendix
    -- $appendix
    ) where

-- For documentation
import Control.Category
import Control.Monad.Morph
import Control.Monad.Trans.Class
import Control.Proxy
import Control.Proxy.Morph
import Control.Proxy.Core.Correct (ProxyCorrect)
import Control.Proxy.Trans.Either

{- $intro
    The @pipes@ library replaces lazy 'IO' with a safe, elegant, and
    theoretically principled alternative.  Use this library if you:

    * want to write high-performance streaming programs,

    * believe that lazy 'IO' was a bad idea,

    * enjoy composing modular and reusable components, or

    * love theory and elegant code.

    This library unifies many kinds of streaming abstractions, all of which are
    special cases of \"proxies\" (The @pipes@ name is a legacy of one such
    abstraction).

    Let's begin with the simplest 'Proxy': a 'Producer'.  The following
    'Producer' lazily streams lines from a 'Handle'

> import Control.Monad
> import Control.Proxy
> import System.IO
> 
> --                 Produces Strings ---+----------+
> --                                     |          |
> --                                     v          v
> lines' :: (Proxy p) => Handle -> () -> Producer p String IO ()
> lines' h () = runIdentityP loop where
>     loop = do
>         eof <- lift $ hIsEOF h
>         if eof
>         then return ()
>         else do
>             str <- lift $ hGetLine h
>             respond str  -- Produce the string
>             loop
>
> -- Ignore the 'runIdentityP' and '()' for now

    But why limit ourselves to streaming lines from some file?  Why not lazily
    generate values from an industrious user?

> --               Uses 'IO' as the base monad --+
> --                                             |
> --                                             v
> promptInt :: (Proxy p) => () -> Producer p Int IO r
> promptInt () = runIdentityP $ forever $ do
>     lift $ putStrLn "Enter an Integer:"
>     n <- lift readLn  -- 'lift' invokes an action in the base monad
>     respond n

    Now we need to hook our 'Producer's up to a 'Consumer'.  The following
    'Consumer' endlessly 'request's a stream of 'Show'able values and 'print's
    them:

> --                   Consumes 'a's ---+----------+    +-- Never terminates, so
> --                                    |          |    |   the return value is
> --                                    v          v    v   polymorphic
> printer :: (Proxy p, Show a) => () -> Consumer p a IO r
> printer () = runIdentityP $ forever $ do
>     a <- request ()  -- Consume a value
>     lift $ putStrLn "Received a value:"
>     lift $ print a

    You can compose a 'Producer' and a 'Consumer' using ('>->'), which produces
    a runnable 'Session':

> --                Self-contained session ---+         +--+-- These must match
> --                                          |         |  |   each component
> --                                          v         v  v
> lines' h  >-> printer :: (Proxy p) => () -> Session p IO ()
>
> promptInt >-> printer :: (Proxy p) => () -> Session p IO r

    ('>->') connects each 'request' in @printer@ with a 'respond' in
    @lines'@ or @promptInt@.

    Finally, you use 'runProxy' to run the 'Session' and convert it back to the
    base monad.  First we'll try our @lines'@ 'Producer', which will stream
    lines from the following file:

> $ cat test.txt
> Line 1
> Line 2
> Line 3

    The following program never brings more than a single line into memory (not
    that it matters for such a small file):

>>> withFile "test.txt" ReadMode $ \h -> runProxy $ lines' h >-> printer
Received a value:
"Line 1"
Received a value:
"Line 2"
Received a value:
"Line 3"

    Similarly, we can lazily stream user input, requesting values from the user
    only when we need them:

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

    The last example proceeds endlessly until we hit @Ctrl-C@ to interrupt it.

    We would like to limit the number of iterations, so lets define an
    intermediate 'Proxy' that behaves like a verbose 'take'.  I will call it a
    'Pipe' (this library's namesake) since values flow through it:

> --                        'a's flow in ---+ +--- 'a's flow out
> --                                        | |
> --                                        v v
> take' :: (Proxy p) => Int -> () -> Pipe p a a IO ()
> take' n () = runIdentityP $ do
>     replicateM_ n $ do
>         a <- request ()
>         respond a
>     lift $ putStrLn "You shall not pass!"

    This 'Pipe' forwards the first @n@ values it receives undisturbed, then it
    outputs a cute message.  You can compose it between the 'Producer' and
    'Consumer' using ('>->'):

>>> runProxy $ promptInt >-> take' 2 >-> printer :: IO ()
Enter an Integer:
9<Enter>
Received a value:
9
Enter an Integer:
2<Enter>
Received a value:
2
You shall not pass!

    When @take' 2@ terminates, it brings down every 'Proxy' composed with it.

    Notice how @promptInt@ behaves lazily and only 'respond's with as many
    values as we 'request'.  We 'request'ed exactly two values, so it only
    prompts the user twice.

    We can already spot several improvements upon traditional lazy 'IO':

    * You can define your own lazy components that have nothing to do with files

    * @pipes@ never uses 'unsafePerformIO' and never violates referential
      transparency.

    * You don't need strictness hacks to ensure the proper ordering of effects

    * You can interleave effects in downstream stages, too

    However, this library can offer even more than that!
-}

{- $bidir
    So far we've only defined proxies that send information downstream in the
    direction of the ('>->') arrow.  However, we don't need to limit ourselves
    to unidirectional communication and we can enhance these proxies with the
    ability to send information upstream with each 'request' that determines
    how upstream stages 'respond'.

    For example, 'Client's generalize 'Consumer's because they can supply an
    argument other than @()@ with each 'request'.  The following 'Client'
    sends three 'request's upstream, each of which provides an 'Int' @argument@
    and expects a 'Bool' @result@:

> --                   Sends out 'Int's ---+   +-- Receives back 'Bool's
> --                                       |   |
> --                                       v   v
> threeReqs :: (Proxy p) => () -> Client p Int Bool IO ()
> threeReqs () = runIdentityP $ forM_ [1, 3, 1] $ \argument -> do
>     lift $ putStrLn $ "Client Sends:    " ++ show (argument :: Int)
>     result <- request argument
>     lift $ putStrLn $ "Client Receives: " ++ show (result :: Bool)
>     lift $ putStrLn "*"

    Notice how 'Client's use \"@request argument@\" instead of
    \"@request ()@\".  This sends \"@argument@\" upstream to parametrize the
    'request'.

    'Server's similarly generalize 'Producer's because they receive arguments
    other than @()@.  The following 'Server' receives 'Int' requests and 
    responds with 'Bool's:

> --                    Receives 'Int's ---+   +--- Replies with 'Bool's
> --                                       |   |
> --                                       v   v
> comparer :: (Proxy p) => Int -> Server p Int Bool IO r
> comparer = runIdentityK loop where
>     loop argument = do
>         lift $ putStrLn $ "Server Receives: " ++ show (argument :: Int)
>         let result = argument > 2
>         lift $ putStrLn $ "Server Sends:    " ++ show (result :: Bool)
>         nextArgument <- respond result
>         loop nextArgument

    Notice how 'Server's receive their first argument as a parameter and bind
    each subsequent argument using 'respond'.  This library provides a
    combinator which abstracts away this common pattern:

> foreverK :: (Monad m) => (a -> m a) -> a -> m b
> foreverK f = loop where
>     loop argument = do
>          nextArgument <- f argument
>          loop nextArgument
>
> -- or: foreverK f = f >=> foreverK f
> --                = f >=> f >=> f >=> f >=> ...

    We can use this to simplify the @comparer@ 'Server':

> comparer = runIdentityK $ foreverK $ \argument -> do
>     lift $ putStrLn $ "Server Receives: " ++ show argument
>     let result = argument > 2
>     lift $ putStrLn $ "Server Sends:    " ++ show result
>     respond result

    ... which looks just like the way you might write a server's main loop in
    another programming language.

    You can compose a 'Server' and 'Client' using ('>->'), and this also returns
    a runnable 'Session':

> comparer >-> threeReqs :: (Proxy p) => () -> Session p IO ()

    Running this executes the client-server session:

>>> runProxy $ comparer >-> threeReqs :: IO ()
Client Sends:    1
Server Receives: 1
Server Sends:    False
Client Receives: False
*
Client Sends:    3
Server Receives: 3
Server Sends:    True
Client Receives: True
*
Client Sends:    1
Server Receives: 1
Server Sends:    False
Client Receives: False
*

    'Proxy's generalize 'Pipe's because they allow information to flow upstream.
    The following 'Proxy' caches 'request's to reduce the load on the 'Server'
    if the request matches a previous one:

> import qualified Data.Map as M
>
> -- 'p' is the Proxy, as the (Proxy p) constraint indicates
>
> cache :: (Proxy p, Ord key) => key -> p key val key val IO r
> cache = runIdentityK (loop M.empty) where
>     loop _map key = case M.lookup key _map of
>         Nothing -> do
>             val  <- request key
>             key2 <- respond val
>             loop (M.insert key val _map) key2
>         Just val -> do
>             lift $ putStrLn "Used cache!"
>             key2 <- respond val
>             loop _map key2

    You can compose the @cache@ 'Proxy' between the 'Server' and 'Client' using
    ('>->'):

>>> runProxy $ comparer >-> cache >-> threeReqs
Client Sends:    1
Server Receives: 1
Server Sends:    False
Client Receives: False
*
Client Sends:    3
Server Receives: 3
Server Sends:    True
Client Receives: True
*
Client Sends:    1
Used cache!
Client Receives: False
*

    This bidirectional flow of information separates @pipes@ from other
    streaming libraries which are unable to model 'Client's, 'Server's, or
    'Proxy's.  Using @pipes@ you can define interfaces to RPC interfaces, REST
    architectures, message buses, chat clients, web servers, network protocols
    ... you name it!
-}

{- $synonyms
    You might wonder why ('>->') accepts 'Producer's, 'Consumer's, 'Pipe's,
    'Client's, 'Server's, and 'Proxy's.  It turns out that these type-check
    because they are all type synonyms that expand to the following central
    type:

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

    Proxies are monad transformers that enrich the base monad with the ability
    to send or receive values upstream or downstream:

>   | Sends    | Receives | Receives   | Sends      | Base  | Return
>   | Upstream | Upstream | Downstream | Downstream | Monad | Value
> p   a'         a          b'           b            m       r

    We can selectively close certain inputs or outputs to generate specialized
    proxies.

    For example, a 'Producer' is a 'Proxy' that can only output values to its
    downstream interface:

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
>
> -- The 'C' type is uninhabited, so it 'C'loses an output end

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
> type Consumer p a m r = p () a () C m r

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

    When we compose proxies, the type system ensures that their input and output
    types match:

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

    The compiler ensures that the types match when we compose 'Server's,
    'Proxy's, and 'Client's.

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

    * You can mix multiple abstractions together as long as the types match
-}

{- $interact
    There are only two ways to interact with other proxies: 'request' and
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
    argument come from?  Well, it turns out that every 'Proxy' receives this
    initial argument as an ordinary parameter, as if they all began blocked on
    a 'respond' statement.
   
    We can see this if we take all the previous proxies we defined and fully
    expand every type synonym.  The initial argument of each 'Proxy' matches
    the type parameter corresponding to the return value of 'respond':

>                                          These
>                                    +--  Columns  ---+
>                                    |     Match      |
>                                    v                v
> promptInt :: (Proxy p)          => ()  -> p C   ()  ()  Int  IO r
> printer   :: (Proxy p, Show a)  => ()  -> p ()  a   ()  C    IO r
> take'     :: (Proxy p)   => Int -> ()  -> p ()  a   ()  a    IO ()
> comparer  :: (Proxy p)          => Int -> p C   ()  Int Bool IO r
> cache     :: (Proxy p, Ord key) => key -> p key val key val  IO r

    You can also study the type of composition, which follows this same pattern.
    Composition requires two 'Proxy's blocked on a 'respond', and produces a new
    'Proxy' similarly blocked on a 'respond':

> (>->) :: (Monad m, Proxy p)
>  => (b' -> p a' a b' b m r)
>  -> (c' -> p b' b c' c m r)
>  -> (c' -> p a' a c' c m r)
>      ^            ^
>      |   These    |
>      +---Match----+

    This is why 'Producer's, 'Consumer's, 'Pipe's and 'Client's all take @()@
    as their initial argument, because their corresponding 'respond' commands
    all have a return value of @()@.

    This library also provides ('>~>'), which is the dual of the ('>->')
    composition operator.  ('>~>') composes two 'Proxy's blocked on a 'request'
    and returns a new 'Proxy' blocked on a 'request':

> (>~>)
>     :: (Monad m, Proxy p)
>     => (a -> p a' a b' b m r)
>     -> (b -> p b' b c' c m r)
>     -> (a -> p a' a c' c m r)

    Conceptually, ('>->') composes pull-based systems and ('>~>') composes
    push-based systems.

    In fact, if you went back through the previous code and systematically
    replaced every:

    * ('>->') with ('<~<'),

    * 'respond' with 'request', and

    * 'request' with 'respond'

    ... then everything would still work and produce identical behavior, except
    the compiler would now infer the symmetric types with all interfaces
    reversed.  We can therefore conclude the obvious: pull-based systems are
    symmetric to push-based systems.

    Since these two composition operators are perfectly symmetric, I arbitrarily
    standardize on using ('>->') and I provide all standard library proxies
    blocked on 'respond' so that they work with ('>->').  This gives behavior
    more familiar to Haskell programmers that work with lazy pull-based
    functions.  I only include the ('>~>') composition operator for theoretical
    completeness.
-}

{- $composition
    When we compose @(p1 >-> p2)@, composition ensures that @p1@'s downstream
    interface matches @p2@'s upstream interface.  This follows from the type of
    ('>->'):

> (>->)
>     :: (Monad m, Proxy p)
>     => (b' -> p a' a b' b m r)
>     -> (c' -> p b' b c' c m r)
>     -> (c' -> p a' a c' c m r)

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
> idT = runIdentityK loop where
>     loop a' = do
>         a   <- request a'
>         a'2 <- respond a
>         loop a'2
>
> -- or: idT = runIdentityK $ foreverK $ request >=> respond
> --         = runIdentityK $ request >=> respond >=> request >=> respond ...

    Diagramatically, this looks like:

>     +-----+
>     |     |
> a' <======== a'   <- All values pass
>     | idT |          straight through
> a  ========> a    <- immediately
>     |     |
>     +-----+

    Transparency means that:

> idT >-> p = p
>
> p >-> idT = p

    In other words, 'idT' is an identity of composition.

    This means that proxies form a true 'Category' where ('>->') is composition
    and 'idT' is the identity.   The associativity law and the two
    identity laws are just the 'Category' laws.  The objects of the category are
    the 'Proxy' interfaces.

    These 'Category' laws guarantee the following important properties:

    * You can reason about each proxy's behavior independently of other proxies
      (otherwise composition wouldn't be associative)

    * You don't encounter weird behavior at the interface between two components
      or at the 'Server' or 'Client' ends of a 'Session' (otherwise 'idT'
      wouldn't be an identity)
-}

{- $class
    All the proxy code we wrote was generic over the 'Proxy' type class, which
    defines the four central operations of this library's API:

    * ('->>'): \"Pointful\" pull-based composition, from which the point-free
      ('>->') operator derives

    * ('>>~'): \"Pointful\" push-based composition, from which the point-free
      ('>~>') operator derives

    * 'request': Request input from upstream

    * 'respond': Respond with output to downstream

    @pipes@ defines everything in terms of these four operations, which is
    why all the library's utilities are polymorphic over the 'Proxy' type class.

    Let's look at some example instances of the 'Proxy' type class:

> instance Proxy ProxyFast     -- Fastest implementation
> instance Proxy ProxyCorrect  -- Strict monad transformer laws

    These two types provide the two alternative base implementations:

    * 'ProxyFast': This runs significantly faster on pure code segments and
      employs several rewrite rules to optimize your code into the equivalent
      hand-tuned code.

    * 'ProxyCorrect': This uses a monad transformer implementation that is
      correct by construction, but runs about 8x slower on pure code segments.
      However, for 'IO'-bound code, the performance gap is small.

    These two implementations differ only in the 'runProxy' function that they
    export, which is how the compiler selects which 'Proxy' implementation to
    use.

    "Control.Proxy" automatically selects the fast implementation for you, but
    you can always choose the correct implementation instead by replacing
    "Control.Proxy" with the following two imports:

> import Control.Proxy.Core         -- Everything except the base implementation
> import Control.Proxy.Core.Correct -- The alternative base implementation

    These are not the only instances of the 'Proxy' type class!  This library
    also provides several \"proxy transformers\", which are like monad
    transformers except that they also correctly lift the 'Proxy' type class:

> instance (Proxy p) => Proxy (IdentityP p)
> instance (Proxy p) => Proxy (EitherP e p)
> instance (Proxy p) => Proxy (MaybeP    p)
> instance (Proxy p) => Proxy (ReaderP i p)
> instance (Proxy p) => Proxy (StateP  s p)
> instance (Proxy p) => Proxy (WriterP w p)

    All of the 'Proxy' code we wrote so far also works seamlessly with all of
    these proxy transformers.  The 'Proxy' class abstracts over the
    implementation details and extensions so that you can reuse the same library
    code for any feature set.

    This polymorphism comes at a price: you must embed your 'Proxy' code in at
    least one proxy transformer if you want clean type class constraints.  If
    you don't use extensions then you embed your code in the identity proxy
    transformer: 'IdentityP'.  This is why all the examples use 'runIdentityP'
    or 'runIdentityK' to embed their code in 'IdentityP'.  "Control.Proxy.Class"
    provides a longer discussion on this subject.

    Without this 'IdentityP' embedding, the compiler infers uglier constraints,
    which are also significantly less polymorphic.  We can show this by
    removing the 'runIdentityP' call from @promptInt@ and see what type the
    compiler infers:

> promptInt () = forever $ do
>     lift $ putStrLn "Enter an Integer:"
>     n <- lift readLn
>     respond n

>>> :t promptInt  -- I've cleaned up the inferred type
promptInt
  :: (Monad (Producer p Int IO), MonadTrans (Producer p Int), Proxy p) =>
     () -> Producer p Int IO r

    All 'Proxy' instances are already monads and monad transformers, but the
    compiler cannot infer that without the 'IdentityP' embedding.  When we embed
    @promptInt@ in 'IdentityP', the compiler collapses the 'Monad' and
    'MonadTrans' constraints into the 'Proxy' constraint.

    Fortunately, you do not pay any performance price for this 'IdentityP'
    embedding or the type class polymorphism.  Your polymorphic code will still
    run very rapidly, as fast as if you had specialized it to a concrete
    'Proxy' instance without the 'IdentityP' embedding.  I've taken great care
    to ensure that all optimizations and rewrite rules always see through these
    abstractions without any assistance on your part.
-}

{- $interleave
    When you compose two proxies, you interleave their effects in the base
    monad.  The following two proxies demonstrate this interleaving of effects:

> downstream :: (Proxy p) => () -> Consumer p () IO ()
> downstream () = runIdentityP $ do
>     lift $ print 1
>     request ()  -- Switch to upstream
>     lift $ print 3
>     request ()  -- Switch to upstream
>
> upstream :: (Proxy p) => () -> Producer p () IO ()
> upstream () = runIdentityP $ do
>     lift $ print 2
>     respond () -- Switch to downstream
>     lift $ print 4

     "Control.Proxy.Class" enumerates the 'Proxy' laws, which equationally
     define how all 'Proxy' instances must behave.  These laws require that
     @(upstream >-> downstream)@ must reduce to the following:

> upstream >-> downstream  -- This is true no matter what feature
> =                        -- set or 'Proxy' instance you select
> \() -> lift $ do
>     print 1
>     print 2
>     print 3
>     print 4

    Conceptually, 'runProxy' just applies this to @()@ and removes the 'lift':

> runProxy $ upstream >-> downstream
> =
> do print 1
>    print 2
>    print 3
>    print 4

    Let's test this:

>>> runProxy $ upstream >-> downstream
1
2
3
4

    The 'Proxy' laws let you reason about how proxies interleave effects without
    knowing any specifics about the underlying implementation.  Intuitively, the
    'Proxy' laws say that:

    * 'request' blocks until upstream 'respond's

    * 'respond' blocks until downstream 'request's

    * If a 'Proxy' terminates, it terminates every 'Proxy' composed with it

    Several of the utilities in "Control.Proxy.Prelude.Base" use these
    equational laws to rigorously prove things about their behavior.  For
    example, consider the 'mapD' proxy, which applies a function @f@ to all
    values flowing downstream:

> mapD :: (Monad m, Proxy p) => (a -> b) -> x -> p x a x b m r
> mapD f = runIdentityK loop where
>     loop x = do
>         a  <- request x
>         x2 <- respond (f a)
>         loop x2
>
> -- or: mapD f = runIdentityK $ foreverK $ request >=> respond . f

    We can use the 'Proxy' laws to prove that:

> mapD f >-> mapD g = mapD (g . f)
>
> mapD id = idT

    ... which is what we expect.  We can fuse two consecutive 'mapD's into one
    by composing their functions, and mapping 'id' does nothing at all, just
    like the identity proxy: 'idT'.

    In fact, these are just the functor laws in disguise, where 'mapD' defines a
    functor between the category of Haskell function composition and the
    category of 'Proxy' composition.  "Control.Proxy.Prelude.Base" is full of
    utilities like this that are simultaneously practical and theoretically
    elegant.
-}

{- $hoist
    Composition can't interleave two proxies if their base monads do not
    match.  For instance, I might try to modify @promptInt@ to use
    @EitherT String@ to report the error instead of using exceptions:

> import Control.Monad.Trans.Either  -- from the "either" package
> import Safe (readMay)              -- from the "safe"   package
>
> promptInt2 :: (Proxy p) => () -> Producer p Int (EitherT String IO) r
> promptInt2 () = runIdentityP $ forever $ do
>     str <- lift $ lift $ do
>         putStrLn "Enter an Integer:"
>         getLine
>     case readMay str of
>         Nothing -> lift $ left "Could not read an Integer"
>         Just n  -> respond n

    However, if I try to compose it with @printer@, I receive a type error:

>>> runEitherT $ runProxy $ promptInt2 >-> printer
<interactive>:2:40:
    Couldn't match expected type `EitherT String IO'
                with actual type `IO'
    ...

    The type error says that @promptInt2@ uses @(EitherT String IO)@ for its
    base monad, but @printer@ uses @IO@ for its base monad, so composition can't
    interleave their effects.

    You can easily fix this using the 'hoist' function from the @mmorph@
    package, which transforms the base monad of any monad transformer that
    implements 'MFunctor'.  Since all proxies implement 'MFunctor' you can use
    'hoist' from 'MFunctor' to 'lift' one proxy's base monad to match another
    proxy's base monad, like so:

>>> runEitherT $ runProxy $ promptInt2 >-> (hoist lift . printer)
Enter an Integer:
Hello<Enter>
Left "Could not read an Integer"

    This library provides three syntactic conveniences for making this easier to
    write.

    First, ('.') has higher precedence than ('>->'), so you can drop the
    parentheses:

>>> runEitherT $ runProxy $ promptInt2 >-> hoist lift . printer
...

    Second, 'lift' is such a common argument to 'hoist' that I provide the
    'raise' function:

> raise = hoist lift

>>> runEitherT $ runProxy $ promptInt2 >-> raise . printer
...

    Third, I provide the 'hoistK' and 'raiseK' functions in case you think
    composition looks ugly:

> hoistK f = (hoist f .)
>
> raiseK = (raise .)

>>> runEitherT $ runProxy $ promptInt2 >-> raiseK printer
...

    For more information on using 'MFunctor', consult the tutorial in the
    @Control.Monad.Morph@ module from the @mmorph@ package.
-}

{- $utilities
    The "Control.Proxy.Prelude" heirarchy provides several utility functions
    for common tasks.  We can redefine the previous example functions just by
    composing these utilities.

    For example, 'readLnS' reads values from user input:

> readLnS :: (Proxy p, Read a) => () -> Producer p a IO r

    ... so we can read 'Int's just by specializing its type:

> readIntS :: (Proxy p) => () -> Producer p Int IO r
> readIntS = readLnS

    The @S@ suffix indicates that it belongs in the \'@S@\'erver position.

    @(takeB_ n)@ allows at most @n@ values to pass through it in \'@B@\'oth
    directions:

> takeB_ :: (Monad m, Proxy p) => Int -> a' -> p a' a a' a m ()

    'takeB_' has a more general type than @take'@ because it allows any type of
    value to flow upstream.

     'printD' prints all values flowing \'@D@\'ownstream:

> printD :: (Proxy p, Show a) => x -> p x a x a IO r

    'printD' has a more general type than our original @printer@ because it
    forwards all values further downstream after 'print'ing them.  This means
    that you could use it as an intermediate stage as well.  However, 'printD'
    still type-checks as the most downstream stage, too, since 'runProxy' just
    discards any unused outbound values.

    These utilities do not clash with the Prelude namespace or common libraries
    because they all end with a capital letter suffix that indicates their
    directionality:

    * \'@D@\' suffix: interacts with values flowing \'@D@\'ownstream

    * \'@U@\' suffix: interacts with values flowing \'@U@\'pstream

    * \'@B@\' suffix: interacts with values flowing \'@B@\'oth ways (or:
      \'@B@\'idirectional)

    * \'@S@\' suffix: belongs furthest upstream in the \'@S@\'erver position

    * \'@C@\' suffix: belongs furthest downstream in the \'@C@\'lient position

    We can assemble these functions into a silent version of our previous
    'Session':

>>> runProxy $ readIntS >-> takeB_ 2 >-> printD
4<Enter>
4
39<Enter>
39

    Fortunately, we don't have to give up our previous useful diagnostics.
    We can use 'execU', which executes an action each time values flow upstream
    through it, and 'execD', which executes an action each time values flow
    downstream through it:

> promptInt :: (Proxy p) => () -> Producer p Int IO r
> promptInt = readLnS >-> execU (putStrLn "Enter an Integer:")
>
> printer :: (Proxy p, Show a) => x -> p x a x a IO r
> printer = execD (putStrLn "Received a value:") >-> printD

    Similarly, we can build our old @take'@ on top of 'takeB_':

> take' :: (Proxy p) => Int -> a' -> p a' a a' a IO ()
> take' n a' = runIdentityP $ do  -- Remember, we need 'runIdentityP' if
>     takeB_ n a'                 -- we use 'do' notation or 'lift'
>     lift $ putStrLn "You shall not pass!"

>>> runProxy $ promptInt >-> take' 2 >-> printer
<Exact same behavior>

    Or perhaps I want to skip user input for testing and mock @promptInt@ by
    replacing it with a predefined set of values:

>>> runProxy $ fromListS [4, 37, 1] >-> take' 2 >-> printer
Received a value:
4
Received a value:
37

    What about our original @lines'@ function?  That's just 'stdinS':

> stdinS :: (Proxy p) => () -> Producer p String IO ()

    You could hand-write loops that accomplish these same tasks, but proxies let
    you:

    * Rapidly swap in and out components for testing, debugging, and fast
      prototyping

    * Factor out common patterns into modular components

    * Mix and match simple stages to build sophisticated programs

    This compositional programming style emphasizes building a library of
    reusable components and connecting them like Unix pipes to assemble the
    desired streaming program.
-}

{- $mixmonadcomp
    Composition isn't the only way to assemble proxies.  You can also sequence
    predefined proxies using @do@ notation to generate more elaborate behaviors.

    Most commonly, you will sequence sources to combine their outputs, very
    similar to how the Unix @cat@ utility behaves:

> threeSources () = do
>     source1 ()
>     source2 ()
>     source3 ()
>
> -- or: threeSources = source1 >=> source2 >=> source3

    As a concrete example, we could create a 'Producer' where our first source
    presets the first few values and then we let the user take over to generate
    the remaining values:

> source1 :: (Proxy p) => () -> Producer p Int IO r
> source1 () = runIdentityP $ do
>     fromListS [4, 4] ()  -- Source 1
>     readLnS ()           -- Source 2
>
> -- or: source1 = runIdentityK (fromListS [4, 4] >=> readLnS)

>>> runProxy $ source1 >-> printD
4
4
70<Enter>
70
34<Enter>
34
...

    What if we only want the user to provide three values?  We can 
    selectively throttle it with 'takeB_':

> source2 :: (Proxy p) => () -> Producer p Int IO ()
> source2 () = runIdentityP $ do
>     fromListS [4, 4] ()
>     (readLnS >-> takeB_ 3) () -- You can compose inside a do block!
>
> -- or: source2 = runIdentityK (fromListS [4, 4] >=> (readLnS >-> takeB_ 3))

    Notice that composition works inside of a @do@ block!  This is a very handy
    trick!

>>> runProxy $ source2 >-> printD
4
4
56<Enter>
56
41<Enter>
41
80<Enter>
80

    You can also concatenate sinks, too:

> sink1 :: (Proxy p) => () -> Pipe p Int Int IO ()
> sink1 () = runIdentityP $ do
>     (takeB_ 3         >-> printD) () -- Sink 1
>     (takeWhileD (< 4) >-> printD) () -- Sink 2
>
> -- or: sink1 = (takeB_ 3 >-> printD) >=> (takeWhileD (< 4) >-> printD)

>>> runProxy $ source2 >-> sink1
4          -- The first sink
4          -- handles these
68<Enter>  --
68
1<Enter>   -- The second sink
1          -- handles these
5<Enter>   --

    ... but the above example is gratuitous because you can simply concatenate
    the intermediate stages:

> sink2 :: (Proxy p) => () -> Pipe p Int Int IO ()
> sink2 = intermediate >-> printD where
>     intermediate () = runIdentityP $ do
>         takeB_ 3 ()          -- Intermediate stage 1
>         takeWhileD (< 4) ()  -- Intermediate stage 2
>
> -- or: sink2 = (takeB_ 3 >=> takeWhileD (< 4)) >-> printD

>>> runProxy $ source2 >-> sink2
<Exact same behavior>

    These examples demonstrate the two principal ways to combine proxies:

    * \"Vertical\" composition, using ('>=>') from the Kleisli category

    * \"Horizontal\" composition: using ('>->') from the Proxy category

    You assemble most proxies simply by composing them in one or both of these
    two categories.
-}

{- $listT
    Proxies generalize lists by allowing you to interleave effects between list
    elements, but you might be surprised to learn that they also generalize the
    list monad, too! "Control.Proxy.ListT" provides the machinery necessary to
    convert back and forth between proxies and a @ListT@-like monad transformer
    that binds proxy outputs.

    For example, let's say that we want to select elements from two separate
    lists, like in the list monad, except interleaving side effects, too:

> --                          +-- ListT that will compile to a 'Producer'
> --                          |
> --                          v
> pairs :: (ListT p) => () -> ProduceT p IO (Int, Int)
> pairs () = do
>     x <- rangeS 1 3     -- Select a number betwen 1 and 3
>     lift $ putStrLn $ "Currently using: " ++ show x
>     y <- eachS [4,6,8]  -- Select one of 4, 6, or 8
>     return (x, y)

    We can compile the above 'ProduceT' code to a 'Producer' using
    'runRespondK':

> runRespondK :: (() -> ProduceT p m b) -> () -> Producer p b m ()
>
> runRespondK pairs :: (ListT p) => () -> Producer p (Int, Int) IO ()

    The return value of the 'ProduceT' becomes the output of the 'Producer',
    which generates one output for each permutation of elements that we could
    have selected:

>>> runProxy $ runRespondK pairs >-> printD
Currently using: 1
(1,4)
(1,6)
(1,8)
Currently using: 2
(2,4)
(2,6)
(2,8)
Currently using: 3
(3,4)
(3,6)
(3,8)

    Proxies actually form two separate 'ListT' monad transformers: one selects
    elements output from the proxy's downstream interface and one selects
    elements output from the proxy's upstream interface.  To distinguish them,
    I call the downstream one 'RespondT' (which generalizes 'ProduceT') and the
    upstream one 'RequestT' (which generalizes 'CoProduceT').

    "Control.Proxy.ListT" defines the 'ListT' class, which provides the two
    bind operators for 'RespondT' and 'RequestT':

    * ('>\\'): Equivalent to ('=<<') for 'RequestT'

    * ('//>'): Equivalent to ('>>=') for 'RespondT'

    Note that you don't need to use these bind operators directly.  You can
    wrap any proxy in the 'RespondT' newtype to automatically use the
    downstream @ListT@ monad or wrap it in the 'RequestT' newtype to
    automatically use the upstream @ListT@ monad.

    The 'RespondT' \/ 'RequestT' newtypes let you build your own custom 'ListT'
    actions simply by wrapping proxies:

> pairs2 :: (ListT p) => () -> ProduceT p IO (Int, Int)
> pairs2 () = do
>     x <- RespondT $ runIdentityP $ do
>         respond 1
>         lift $ putStrLn "Here"
>         respond 2
>     y <- RespondT $ runIdentityP $ do
>         respond 3
>         lift $ putStrLn "There"
>         respond 4
>     return (x, y)

    Binding a proxy in the 'RespondT' monad behaves like binding a list in the
    list monad: the return value selects one of the proxy's downstream outputs,
    interleaving side effects as necessary:

>>> runProxy $ runRespondK pairs2 >-> printD
(1,3)
There
(1,4)
Here
(2,3)
There
(2,4)

    Dually, binding a proxy in the 'RequestT' monad selects one of the proxy's
    upstream outputs.

    'RespondT' can do more than build 'Producer's: it can build arbitrary
    proxies.  We can modify the above example so that the 'RespondT' actions can
    'request' input from upstream, as long as we generalize 'ProduceT' to the
    more powerful 'RespondT' type:

> pairs3 :: (ListT p) => () -> RespondT p () Int () IO (Int, Int)
> pairs3 () = do
>     x <- RespondT $ runIdentityP $ replicateM_ 2 $ do
>         a <- request ()
>         lift $ putStrLn $ "Received " ++ show a
>         respond a
>     y <- RespondT $ runIdentityP $ replicateM_ 3 $ do
>         a <- request ()
>         lift $ putStrLn $ "Received " ++ show a
>         respond a
>     return (x, y)

>>> runProxy $ enumFromS 1 >-> runRespondK pairs3 >-> printD
Received 1
Received 2
(1,2)
Received 3
(1,3)
Received 4
(1,4)
Received 5
Received 6
(5,6)
Received 7
(5,7)
Received 8
(5,8)

    'Request' and 'RespondT' are correct by construction, meaning that they
    always satisfy the monad and monad transformer laws no matter what, unlike
    @ListT@ from @transformers@.
-}

{- $folds
    You can fold a stream of values in two ways, both of which use the base
    monad:

    * Use 'WriterT' in the base monad and 'tell' the values to fold

    * Use 'StateT' in the base monad and 'put' strict values

    'WriterT' is more elegant in principle but leaks space for a large number of
    values to fold.  'StateT' does not leak space if you keep the accumulator
    strict, but is less elegant and doesn't guarantee write-only behavior.  To
    remedy this, I am currently working on a stricter 'WriterT' implementation
    that does not leak space to add to the @transformers@ package.

    "Control.Proxy.Prelude.Base" provides several common folds using 'WriterT'
    as the base monad, such as:

    * 'lengthD': Count how many values flow downstream

> lengthD :: (Monad m, Proxy p) => x -> p x a x a (WriterT (Sum Int) m) r

    * 'toListD': Fold the values flowing downstream into a list.

> toListD :: (Monad m, Proxy p) => x -> p x a x a (WriterT [a] m) r

    * 'anyD': Determine whether any values satisfy the predicate

> anyD :: (Monad m, Proxy p) => (a -> Bool) -> x -> p x a x a (WriterT Any m) r

    These 'WriterT' versions demonstrate how the elegant approach should work in
    principle and they should be okay for folding a medium number of values
    until I release the fixed 'WriterT'.  If space leaks cause problems, you can
    temporarily rewrite the 'WriterT' folds using the following two strict
    'StateT' folds:

    * 'foldlD'': Strictly fold values flowing downstream

> foldlD'
>     :: (Monad m, Proxy p)
>     => (b -> a -> b) -> x -> p x a x a (StateT b m) r

    * 'foldlU'': Strictly fold values flowing upstream

> foldU'
>     :: (Monad m, Proxy p)
>     => (b -> a' -> b) -> a' -> p a' x a' x (StateT b m) r

    Now, let's try these folds out and see if we can build a list from user
    input:

>>> runWriterT $ runProxy $ raiseK promptInt >-> takeB_ 3 >-> toListD
Enter an Integer:
1<Enter>
Enter an Integer:
66<Enter>
Enter an Integer:
5<Enter>
((),[1,66,5])

    Notice that @promptInt@ uses 'IO' as its base monad, but 'toListD' uses
    @(WriterT [Int] m)@ as its base monad, so I use 'raiseK' to get the base
    monads to match.

    You can insert these folds anywhere in the middle of a pipeline and they
    still work:

>>> runWriterT $ runProxy $ fromListS [5, 7, 4] >-> lengthD >-> raiseK printD
5
7
4
((),Sum {getSum = 3})

    You can also run multiple folds at the same time just by adding more
    'WriterT' layers to your base monad:

>>> runWriterT $ runWriterT $ runProxy $ fromListS [9, 10] >-> anyD even >-> raiseK sumD
(((),Any {getAny = True}),Sum {getSum = 19}

    I designed certain special folds to terminate the 'Session' early if they
    can compute their result prematurely, in order to draw as little input as
    possible.  These folds end with an underscore, such as 'headD_', which
    terminates the stream once it receives an input:

> headD_ :: (Monad m, Proxy p) => x -> p x a x a (WriterT (First a) m) ()

>>> runWriterT $ runProxy $ fromListS [3, 4, 9] >-> raiseK printD >-> headD_
3
((),First {getFirst = Just 3})

    Compare this to 'headD' without underscore, which folds the entire input:

>>> runWriterT $ runProxy $ fromListS [3, 4, 9] >-> raiseK printD >-> headD
3
4
9
((),First {getFirst = Just 3})

    Use the versions that don't prematurely terminate if you are running
    multiple folds or if you want to continue to use the rest of the input when
    the fold is done.  Use the versions that do prematurely terminate if
    collecting that single fold is the entire purpose of the session.
-}

{- $resource
    This core library provides utilities for lazily streaming from resources,
    but does not provide utilities for lazily managing resource allocation and
    deallocation.  To frame the problem, let's assume that we try to be clever
    and write a streaming utility that lazily opens a file only in response to
    a 'request', such as the following 'Producer':

> readFileS :: () -> FilePath -> () -> Producer p String IO ()
> readFileS file () = runIdentityP $ do
>     h <- lift $ openFile file ReadMode
>     lift $ putStrLn "Opening file"
>     hGetLineS h ()
>     lift $ putStrLn "Closing file"
>     lift $ hClose h

    This works well if we fully demand the file:

>>> runProxy $ readFileS "test.txt" >-> printD
Opening file
"Line 1"
"Line 2"
"Line 3"
Closing file

    This also works well if we never demand the file at all, in which case we
    never open it:

>>> runProxy $ readFileS "test.txt" >-> return
-- Outputs nothing

    But it gives exactly the wrong behavior if we partially demand the file:

>>> runProxy $ readFileS "test.txt" >-> takeB_ 1 >-> printD
Opening file
"Line 1"

    Notice that this does not close the file, because once @takeB_ 1@ terminates
    it terminates the entire 'Session' and @readFileS@ does not get a chance to
    finalize the file.

    The @pipes-safe@ library solves this problem by providing resource
    management abstractions built on top of @pipes@ and offers several other
    nice features:

    * It is completely exception safe, even against asynchronous exceptions

    * It is backwards compatible with \"unmanaged\" ordinary proxies

    Backwards compatibility means that you don't need to buy in to the
    @pipes-safe@ way of doing things.  This matters because another common
    approach is to just open all resources before running the session and close
    them all afterward.  For example,, if I wanted to emulate the Unix @cp@
    command, streaming one line at a time, I might write:

> cp :: FilePath -> FilePath -> IO ()
> cp inFile outFile =
>     withFile inFile  ReadMode  $ \hIn  ->
>     withFile outFile WriteMode $ \hOut ->
>     runProxy $ hGetLineS hIn >-> hPutStrLnD hOut

    Some people prefer that approach because it:

    * is straightforward, and

    * can reuse functions from @Control.Exception@

    The disadvantage is that this does not lazily allocate resources, nor does
    this promptly deallocate them.  Also, there is no way to recover from
    exceptions and resume the 'Session'.  On the other hand, @pipes-safe@ lets
    you do all of these.

    Fortunately, you can choose whichever approach you prefer and rest assured
    that the two approaches safely interoperate.  @Control.Proxy.Safe.Tutorial@
    from the @pipes-safe@ package provides a separate tutorial on how to:

    * extend @pipes@ with resource management,

    * handle exceptions natively within proxies, and

    * interoperate with unmanaged code.
-}

{- $extend
    This library provides several extensions that add features on top of the
    base 'Proxy' API.  These extensions behave like monad transformers, except
    that they also lift the 'Proxy' class through the extension so that the
    extended proxy can still 'request', 'respond', and compose with other
    proxies:

> instance (Proxy p) => Proxy (IdentityP p)  -- Equivalent to IdentityT
> instance (Proxy p) => Proxy (EitherP e p)  -- Equivalent to EitherT
> instance (Proxy p) => Proxy (MaybeP    p)  -- Equivalent to MaybeT
> instance (Proxy p) => Proxy (StateP  s p)  -- Equivalent to StateT
> instance (Proxy p) => Proxy (WriterP w p)  -- Equivalent to WriterT

    Each of these proxy transformers provides the same API as the equivalent
    monad transformer (sometimes even more).  The following sections show some
    common problems that these proxy transformers solve.
-}

{- $error

    Our previous @promptInt@ example suffered from one major flaw:

> promptInt2 :: (Proxy p) => () -> Producer p Int (EitherT String IO) r
> promptInt2 () = runIdentityP $ forever $ do
>     str <- lift $ lift $ do
>         putStrLn "Enter an Integer:"
>         getLine
>     case readMay str of
>         Nothing -> lift $ left "Could not read an Integer"
>         Just n  -> respond n

    There is no way to recover from the error and resume streaming data.  You
    can only handle the 'Left' value after using 'runProxy', but by then it is
    too late.

    We can solve this by switching the order of the two monad transformers, but
    using 'EitherP' this time instead of 'EitherT':

> import qualified Control.Proxy.Trans.Either as E
>
> --               Proxy transformers play
> --               nice with type synonyms --+
> --                                         |
> --                                         v
> promptInt3 :: (Proxy p) => () -> Producer (E.EitherP String p) Int IO r
> -- i.e.       (Proxy p) => () -> EitherP String p C () () Int IO r
>
> promptInt3 () = forever $ do
>     str <- lift $ do
>         putStrLn "Enter an Integer:"
>         getLine
>     case readMay str of
>         Nothing -> E.throw "Could not read an Integer"
>         Just n  -> respond n

    This example does not need 'runIdentityP' (nor would that type-check)
    because the 'EitherP' proxy transformer gives the compiler enough
    information to generalize the constraints.

    We've swapped the order of the transformers, so now we use 'runEitherK'
    first to unwrap the 'EitherP' followed by 'runProxy'.

> runEitherK
>     :: (q -> EitherP p a' a b' b m r) -> (q -> p a' a b' b m (Either e r))

>>> runProxy $ E.runEitherK $ promptInt3 >-> printer :: IO (Either String r)
Enter an Integer:
Hello<Enter>
Left "Could not read an Integer"

    Notice how we can directly compose @printer@ with @promptInt@.
    This works because @printer@'s base proxy type is completely polymorphic
    over the 'Proxy' type class and doesn't use any features specific to any
    proxy transformers:

>                  'p' type-checks as anything --+
>                   that implements 'Proxy'      |
>                                                v
> printer :: (Proxy p, Show a) => () -> Consumer p a IO r

    This means that you can compose @printer@ with anything that implements the
    'Proxy' type class, including 'EitherP', without any lifting.

    'EitherP' lets us catch and handle errors locally without disturbing other
    proxies.  For example, I can define a heartbeat function that just restarts
    a given proxy each time it raises an error:

> heartbeat
>     :: (Proxy p)
>     => E.EitherP String p a' a b' b IO r
>     -> E.EitherP String p a' a b' b IO r
> heartbeat p = p `E.catch` \err -> do
>     lift $ putStrLn err  -- Print the error
>     heartbeat p          -- Restart 'p'

    This uses the 'E.catch' function from "Control.Proxy.Trans.Either", which
    lets you catch and handle errors locally without disturbing other proxies.

>>> runProxy $ E.runEitherK $ (heartbeat . promptInt3) >-> takeB_ 2 >-> printer
Enter an Integer:
Hello<Enter>
Could not read an Integer
Enter an Integer
8
Received a value:
8
Enter an Integer
0
Received a value:
0

    It's very easy to prove that 'EitherP' has only a local effect.  In fact,
    we can run it locally on a subset of the pipeline like so:

>>> runProxy $ (E.runEitherK $ heartbeat . promptInt3 >-> takeB_ 2) >-> printer

    Proxy transformers do not use the base monad at all, so you can use them to
    isolate effects from other proxies, as the next section demonstrates.
-}

{- $state
    The 'StateP' proxy lets you embed local state into any 'Proxy' computation.
    For example, we might want to gratuitously use state to generate successive
    numbers:

> import qualified Control.Proxy.Trans.State as S
>
> increment :: (Monad m, Proxy p) => () -> Producer (S.StateP Int p) Int m r
> increment () = forever $ do
>     n <- S.get
>     respond n
>     S.put (n + 1)

    We could then embed it locally into any 'Proxy', such as the following one:

> numbers :: (Monad m, Proxy p) => () -> Producer p Int m ()
> numbers () = runIdentityP $ do
>     (takeB_ 5 <-< S.evalStateK 10 increment) ()
>     S.evalStateK 1  (takeB_ 3 <-< increment) () -- This works, too!

>>> runProxy $ numbers >-> printD
10
11
12
13
14
1
2
3

    We can also prove the effect is local even when you directly compose two
    'StateP' proxies before running them.  Let's define a stateful consumer:

> increment2 :: (Proxy p) => () -> Consumer (S.StateP Int p) Int IO r
> increment2 () = forever $ do
>     nOurs   <- S.get
>     nTheirs <- request ()
>     lift $ print (nTheirs, nOurs)
>     S.put (nOurs + 2)

    .. and hook it up directly to @increment@:

>>> runProxy $ S.evalStateK 0 $ increment >-> takeB_ 3 >-> increment2
(0, 0)
(1, 2)
(2, 4)

    They each share the same initial state, but they isolate their own side
    effects completely from each other.
-}

{- $branch
    So far we've only considered linear chains of proxies, but @pipes@ allows
    you to branch these chains and generate more sophisticated topologies.  The
    trick is to simply nest the 'Proxy' monad transformer within itself.

    For example, if I want to zip two inputs, I can just define the following
    triply nested proxy:

> zipD
>     :: (Monad m, Proxy p1, Proxy p2, Proxy p3)
>     => () -> Consumer p1 a (Consumer p2 b (Producer p3 (a, b) m)) r
> zipD () =
>     runIdentityP . hoist (runIdentityP . hoist runIdentityP) $ forever $ do
>     -- Yes, this 'runIdentityP' mess is necessary.  Sorry!
>
>         a <- request ()               -- Request from the outer 'Consumer'
>         b <- lift $ request ()        -- Request from the inner 'Consumer'
>         lift $ lift $ respond (a, b)  -- Respond to the 'Producer'

    'zipD' behaves analogously to a curried function.  We partially apply it to
    each layer using composition and 'runProxyK' or 'runProxy':

> -- 1st application
> p1 = runProxyK $ zipD <-< fromListS [1..3]
>
> -- 2nd application
> p2 = runProxyK $ p1 <-< fromListS [4..]
>
> -- 3rd application
> p3 = runProxy $ printD <-< p2

>>> p3
(1,4)
(2,5)
(3,6)

    You can use this trick to fork output, too:

> fork
>     :: (Monad m, Proxy p1, Proxy p2, Proxy p3)
>     => () -> Consumer p1 a (Producer p2 a (Producer p3 a m)) r
> fork () =
>     runIdentityP . hoist (runIdentityP . hoist runIdentityP) $ forever $ do
>         a <- request ()          -- Request output from the 'Consumer'
>         lift $ respond a         -- Send output to the outer 'Producer'
>         lift $ lift $ respond a  -- Send output to the inner 'Producer'

    Again, we just keep partially applying it until it is fully applied:

> -- 1st application
> p1 = runProxyK $ fork <-< fromListS [1..3]
>
> -- 2nd application
> p2 = runProxyK $ raiseK printD <-< mapD (> 2) <-< p1
>
> -- 3rd application
> p3 = runProxy  $ printD <-< mapD show <-< p2

>>> p3
False
"1"
False
"2"
True
"3"

    You can even merge or fork proxies that use entirely different feature sets:

> p1 = runProxyK $ S.evalStateK 0 $ fork <-< increment
>
> p2 = runProxyK $ raiseK printD <-< mapD (+ 10) <-< p1
>
> p3 = runProxy  $ E.runEitherK $ printD <-< (takeB_ 3 >=> E.throw) <-< p2

>>> p3
10
0
11
1
12
2
Left ()

    We just forked a @(StateP p1)@ proxy and read out the result in both a
    generic @p2@ proxy and an @(EitherP p3)@ proxy.  That's pretty crazy, but it
    gives you a sense of how versatile and robust proxies can be.

    You can implement arbitrary branching topologies using this trick.  However,
    I want to mention a few caveats:

    * The intermediate partially applied type signatures will be ugly as sin.
      I warned you.

    * You cannot implement cyclic topologies (and cyclic topologies do not make
      sense for proxies anyway)

    * You cannot use this trick to implement a polymorphic zip function of the
      following form:

> zip'  -- You can't define this
>     :: (Monad m, Proxy p)
>     => (() -> Producer p a      m r)
>     -> (() -> Producer p b      m r)
>     -> (() -> Producer p (a, b) m r)

    Partial application requires selecting a 'Proxy' instance, which is why you
    cannot define @zip'@.  You /can/ define a @zip'@ specialized to a concrete
    'Proxy' instance, but I don't really recommend doing that since you should
    always strive to write polymorphic proxies to avoid locking your user into
    a particular feature set.

    With those caveats out of the way, this approach affords many indispensable
    features that other approaches do not allow:

    * It does not require extending the 'Proxy' type class

    * It handles almost every branching scenario, including more complicated
      situations like concurrent interleavings

    * You can branch and merge mixtures of 'Server's, 'Client's, and 'Proxy's

    * You can branch and merge heterogeneous feature sets

    * It is completely polymorphic over the 'Proxy' class and uses no
      implementation-specific details
-}

{- $proxytrans
    There is one last scenario that you will eventually encounter: mixing
    proxies that have incompatible proxy transformer stacks.  You solve this the
    exact same way you mix different monad transformer stacks, except that
    instead of using 'lift' and 'hoist' you use 'liftP' and 'hoistP'.

    For example, we might want to mix @promptInt3@ and @increment2@:

> promptInt3 :: (Proxy p) => () -> Producer (EitherP String p) Int IO r
>
> increment2 :: (Proxy p) => () -> Consumer (StateP Int p) Int IO r

    Unfortunately, they use two different feature sets so neither one is fully
    polymorphic over the 'Proxy' class and we cannot directly compose them.

    Fortunately, all proxy transformers implement the 'ProxyTrans' class,
    analogous to the 'MonadTrans' class for transformers:

> class ProxyTrans t where
>     liftP
>          :: (Monad m, Proxy p)
>          => p a' a b' b m r -> t p a' a b' b m r
>
> -- mapP is slightly more elegant
> mapP
>     :: (Monad m, Proxy p, ProxyTrans t)
>     => (q -> p a' a b' b m r) -> (q -> t p a' a b' b m r)
> mapP = (liftP .)

    It's very easy to use.  Just use 'mapP' to lift one proxy transformer to
    match another one.  For example, we can 'mapP' @increment2@ to match
    @promptInt3@:

> promptInt3
>     :: (Proxy stateP)
>     => () -> Producer (EitherP String  stateP       ) Int IO r
>
> mapP increment2
>     :: (Proxy p, ProxyTrans eitherP)
>     => () -> Consumer (eitherP        (StateP Int p)) Int IO r
>
> promptInt3 >-> mapP increment2
>     :: (Proxy p)
>     => () -> Session  (EitherP String (StateP Int p))     IO r

    'mapP' creates a new 'ProxyTrans' layer that type-checks as 'EitherP', and
    @StateP Int p@ type-checks as the 'Proxy' in @promptInt3@'s signature.

>>> runProxy $ S.evalStateK 0 $ E.runEitherK $ promptInt3 >-> mapP increment2
Enter an Integer:
4<Enter>
(4, 0)
Enter an Integer:
5<Enter>
(5, 2)
Enter an Integer:
Hello<Enter>
Left "Could not read an Integer"

    ... or we could instead 'mapP' @promptInt3@ to match @increment2@ and switch
    the order of the two proxy transformers:

> mapP promptInt3
>     :: (Proxy p, ProxyTrans stateP)
>     => () -> Producer (stateP     (EitherP String p)) Int IO r
>
> increment2
>     :: (Proxy eitherP)
>     => () -> Consumer (StateP Int  eitherP          ) Int IO r
>
> mapP promptInt3 >-> increment2
>     :: (Proxy p)
>     => () -> Session  (StateP Int (EitherP String p))     IO r

    'mapP' creates a new 'ProxyTrans' layer that type-checks as 'StateP', and
    @EitherP Int p@ type-checks as the 'Proxy' in @increment2@'s signature.

>>> runProxy $ E.runEitherK $ S.evalStateK 0 $ mapP promptInt3 >-> increment2
Enter an Integer:
4<Enter>
(4, 0)
Enter an Integer:
5<Enter>
(5, 2)
Enter an Integer:
Hello<Enter>
Left "Could not read an Integer"

    Like monad transformers, proxy transformers lift a base 'Monad' instance
    to an extended 'Monad' instance and 'liftP' exactly mirrors the 'lift'
    function from 'MonadTrans'.  'liftP' takes some base proxy, @p@, that
    implements 'Monad' and \"lift\"s it to an extended proxy, @(t p)@, that also
    implements 'Monad'.

    This means you can seamlessly mix effects from different proxy transformer
    layers just by using 'liftP' to access inner layers:

> twoLayers
>     :: (Proxy p)
>     => () -> Consumer (E.EitherP String (S.StateP Int p)) Int IO r
> twoLayers () = forever $ do
>     a <- request ()
>     if (a >= 0)
>         then liftP $ S.modify (+ a)
>         else E.throw "Negative number"

>>> runProxy $ S.runStateK 0 $ E.runEitherK $ fromListS [1, 2, -4] >-> twoLayers
(Left "Negative number",3)

    This exactly resembles how you use 'lift' to access inner layers of a monad
    transformer stack.
-}

{- $proxymorph
    Monad transformers impose certain laws to ensure that 'lift' behaves
    correctly.  These are known as the \"monad morphism laws\":

> (lift .) (f >=> g) = (lift .) f >=> (lift .) g
>
> (lift .) return = return

    If you convert these laws to @do@ notation, they just say:

> do  x <- lift m  =  lift $ do x <- m
>     lift (f x)                f x
>
> lift (return r) = return r

    Proxy transformers require the exact same laws to ensure that they lift the
    base monad to the extended monad correctly.  Just replace 'lift' with
    'liftP':

> (liftP .) (f >=> g) = (liftP .) f >=> (liftP .) g
>
> (liftP .) return = return

    The only difference is 'mapP' sweetens these laws a little bit:

> mapP = (liftP .)
>
> mapP (f >=> g) = mapP f >=> mapP g  -- These are functor laws!
>
> mapP return = return

    However, proxy transformers do one extra thing above and beyond ordinary
    monad transformers.  Proxy transformers lift the 'Proxy' type class, meaning
    that if the base type implements 'Proxy', so does the extended type.

    This means that we need a set of laws that guarantee that the proxy
    transformer lifts the 'Proxy' instance correctly.  I call these laws the
    \"proxy morphism laws\":

> mapP (f >-> g) = mapP f >-> mapP g  -- These are functor laws, too!
>
> mapP idT = idT

    In other words, a proxy transformer defines a functor from the base
    composition to the extended composition!  Neat!

    But we're not even done, because proxies actually form three other
    categories, only one of which I have mentioned so far, and proxy
    transformers lift these three other categories correctly, too:

> -- The push-based category
>
> mapP (f >~> g) = mapP f >~> mapP g
>
> mapP coidT = coidT

> -- The upstream ListT Kleisli category
>
> mapP (f \>\ g) = mapP f \>\ mapP g
>
> mapP request = request

> -- The downstream ListT Kleisli category
>
> mapP (f />/ g) = mapP f />/ mapP g
>
> mapP respond = respond

    I want to highlight two of the above laws:

> mapP request = request
>
> mapP respond = respond

    We can translate those back to 'liftP' to get:

> liftP (request a') = request a'
>
> liftP (respond b)  = respond b

    In other words, 'request' and 'respond' in the extended proxy must behave
    exactly the same as lifting 'request' and 'respond' from the base proxy.

    All the proxy transformers in this library obey the proxy morphism laws,
    which ensure that 'liftP' / 'mapP' always do \"the right thing\".
-}

{- $proxyfunctor
    Proxy transformers also implement 'hoistP' from the 'PFunctor' class in
    "Control.Proxy.Morph".  This exactly parallels 'hoist' from
    @Control.Monad.Morph@.

    You will most commonly use 'hoistP' to insert arbitrary proxy transformer
    layers to get two mismatched proxy transformer stacks to type-check.

    For example, consider the following two very different proxy transformer
    stacks:

> p1 :: (Monad m, Proxy p) => a' -> StateP s (ReaderP i p) a' a a' a m a'
> p2 :: (Monad m, Proxy p) => a' -> MaybeP   (WriterP w p) a' a a' a m a'

    I can normalize them to use same proxy transformer stack by judiciously
    inserting extra proxy transformer layers using a combination of 'hoistP'
    and 'liftP':

> p1' :: (Monad m, Proxy p)
>     => a' -> StateP s (MaybeP (ReaderP i (WriterP w p))) a' a a' a m a'
> p1' = hoistP liftP . p1
> 
> p2' :: (Monad m, Proxy p)
>     => a' -> StateP s (MaybeP (ReaderP i (WriterP w p))) a' a a' a m a'
> p2' = liftP . hoistP liftP . p2

    Now that I've made them agree on a common proxy transformer stack, I can
    sequence them or compose them:

> pSequence
>     :: (Proxy p)
>     => a' -> StateP s (MaybeP (ReaderP i (WriterP w p))) a' a a' a m a'
> pSequence = p1' >=> p2'
>
> pCompose
>     :: (Proxy p)
>     => a' -> StateP s (MaybeP (ReaderP i (WriterP w p))) a' a a' a m a'
> pCompose  = p1' >-> p2'
-}

{- $conclusion
    The @pipes@ library emphasizes the reuse of a small set of core abstractions
    grounded in theory to implement all functionality:

    * Monads

    * Proxies: ('>->'), 'request', and 'respond'

    * Monad Transformers and Functors on Monads: 'lift' and 'hoist'

    * Proxy Transformers and Functors on Proxies: 'liftP' and 'hoistP'

    However, I don't expect everybody to immediately understand how so few
    primitives can implement such a wide variety of features.  This tutorial
    gives a taste of how many interesting ways you can combine these few
    abstractions, but these examples barely scratch the surface, despite this
    tutorial's length.  So if you don't know how to implement something using
    @pipes@, just ask me and I will be happy to help.
-}

-- $appendix
-- I've provided the full code for the above examples here so you can easily
-- play with the examples yourself:
-- 
-- > import Control.Monad
-- > import Control.Proxy hiding (zipD)
-- > import System.IO
-- > import qualified Data.Map as M
-- > import Control.Monad.Trans.Either
-- > import Safe (readMay)
-- > import qualified Control.Proxy.Trans.Either as E
-- > import qualified Control.Proxy.Trans.State as S
-- > 
-- > lines' :: (Proxy p) => Handle -> () -> Producer p String IO ()
-- > lines' h () = runIdentityP loop where
-- >     loop = do
-- >         eof <- lift $ hIsEOF h
-- >         if eof
-- >         then return ()
-- >         else do
-- >             str <- lift $ hGetLine h
-- >             respond str  -- Produce the string
-- >             loop
-- > 
-- > promptInt :: (Proxy p) => () -> Producer p Int IO r
-- > promptInt () = runIdentityP $ forever $ do
-- >     lift $ putStrLn "Enter an Integer:"
-- >     n <- lift readLn  -- 'lift' invokes an action in the base monad
-- >     respond n
-- > {-
-- > promptInt = readLnS >-> execU (putStrLn "Enter an Integer:")
-- > -}
-- > 
-- > printer :: (Proxy p, Show a) => () -> Consumer p a IO r
-- > printer () = runIdentityP $ forever $ do
-- >     a <- request ()  -- Consume a value
-- >     lift $ putStrLn "Received a value:"
-- >     lift $ print a
-- > {-
-- > printer :: (Proxy p, Show a) => x -> p x a x a IO r
-- > printer = execD (putStrLn "Received a value:") >-> printD
-- > -}
-- > 
-- > take' :: (Proxy p) => Int -> () -> Pipe p a a IO ()
-- > take' n a' = runIdentityP $ do  -- Remember, we need 'runIdentityP' if
-- >     takeB_ n a'                 -- we use 'do' notation or 'lift'
-- >     lift $ putStrLn "You shall not pass!"
-- > 
-- > threeReqs :: (Proxy p) => () -> Client p Int Bool IO ()
-- > threeReqs () = runIdentityP $ forM_ [1, 3, 1] $ \argument -> do
-- >     lift $ putStrLn $ "Client Sends:    " ++ show (argument :: Int)
-- >     result <- request argument
-- >     lift $ putStrLn $ "Client Receives: " ++ show (result :: Bool)
-- >     lift $ putStrLn "*"
-- > 
-- > comparer :: (Proxy p) => Int -> Server p Int Bool IO r
-- > comparer = runIdentityK $ foreverK $ \argument -> do
-- >     lift $ putStrLn $ "Server Receives: " ++ show argument
-- >     let result = argument > 2
-- >     lift $ putStrLn $ "Server Sends:    " ++ show result
-- >     respond result
-- > 
-- > cache :: (Proxy p, Ord key) => key -> p key val key val IO r
-- > cache = runIdentityK (loop M.empty) where
-- >     loop _map key = case M.lookup key _map of
-- >         Nothing -> do
-- >             val  <- request key
-- >             key2 <- respond val
-- >             loop (M.insert key val _map) key2
-- >         Just val -> do
-- >             lift $ putStrLn "Used cache!"
-- >             key2 <- respond val
-- >             loop _map key2
-- > 
-- > downstream :: (Proxy p) => () -> Consumer p () IO ()
-- > downstream () = runIdentityP $ do
-- >     lift $ print 1
-- >     request ()  -- Switch to upstream
-- >     lift $ print 3
-- >     request ()  -- Switch to upstream
-- > 
-- > upstream :: (Proxy p) => () -> Producer p () IO ()
-- > upstream () = runIdentityP $ do
-- >     lift $ print 2
-- >     respond () -- Switch to downstream
-- >     lift $ print 4
-- > 
-- > promptInt2 :: (Proxy p) => () -> Producer p Int (EitherT String IO) r
-- > promptInt2 () = runIdentityP $ forever $ do
-- >     str <- lift $ lift $ do
-- >         putStrLn "Enter an Integer:"
-- >         getLine
-- >     case readMay str of
-- >         Nothing -> lift $ left "Could not read an Integer"
-- >         Just n  -> respond n
-- > 
-- > readIntS :: (Proxy p) => () -> Producer p Int IO r
-- > readIntS = readLnS
-- > 
-- > source1 :: (Proxy p) => () -> Producer p Int IO r
-- > source1 () = runIdentityP $ do
-- >     fromListS [4, 4] ()  -- Source 1
-- >     readLnS ()           -- Source 2
-- > 
-- > source2 :: (Proxy p) => () -> Producer p Int IO ()
-- > source2 () = runIdentityP $ do
-- >     fromListS [4, 4] ()
-- >     (readLnS >-> takeB_ 3) () -- You can compose inside a do block!
-- > 
-- > sink1 :: (Proxy p) => () -> Pipe p Int Int IO ()
-- > sink1 () = runIdentityP $ do
-- >     (takeB_ 3         >-> printD) () -- Sink 1
-- >     (takeWhileD (< 4) >-> printD) () -- Sink 2
-- > 
-- > sink2 :: (Proxy p) => () -> Pipe p Int Int IO ()
-- > sink2 = intermediate >-> printD where
-- >     intermediate () = runIdentityP $ do
-- >         takeB_ 3 ()          -- Intermediate stage 1
-- >         takeWhileD (< 4) ()  -- Intermediate stage 2
-- > 
-- > pairs :: (ListT p) => () -> ProduceT p IO (Int, Int)
-- > pairs () = do
-- >     x <- rangeS 1 3     -- Select a number betwen 1 and 3
-- >     lift $ putStrLn $ "Currently using: " ++ show x
-- >     y <- eachS [4,6,8]  -- Select one of 4, 6, or 8
-- >     return (x, y)
-- > 
-- > pairs2 :: (ListT p) => () -> ProduceT p IO (Int, Int)
-- > pairs2 () = do
-- >     x <- RespondT $ runIdentityP $ do
-- >         respond 1
-- >         lift $ putStrLn "Here"
-- >         respond 2
-- >     y <- RespondT $ runIdentityP $ do
-- >         respond 3
-- >         lift $ putStrLn "There"
-- >         respond 4
-- >     return (x, y)
-- > 
-- > readFileS :: (Proxy p) => FilePath -> () -> Producer p String IO ()
-- > readFileS file () = runIdentityP $ do
-- >     h <- lift $ openFile file ReadMode
-- >     lift $ putStrLn "Opening file"
-- >     hGetLineS h ()
-- >     lift $ putStrLn "Closing file"
-- >     lift $ hClose h
-- > 
-- > cp :: FilePath -> FilePath -> IO ()
-- > cp inFile outFile =
-- >     withFile inFile  ReadMode  $ \hIn  ->
-- >     withFile outFile WriteMode $ \hOut ->
-- >     runProxy $ hGetLineS hIn >-> hPutStrLnD hOut
-- > 
-- > promptInt3 :: (Proxy p) => () -> Producer (E.EitherP String p) Int IO r
-- > promptInt3 () = forever $ do
-- >     str <- lift $ do
-- >         putStrLn "Enter an Integer:"
-- >         getLine
-- >     case readMay str of
-- >         Nothing -> E.throw "Could not read an Integer"
-- >         Just n  -> respond n
-- > 
-- > heartbeat
-- >     :: (Proxy p)
-- >     => E.EitherP String p a' a b' b IO r
-- >     -> E.EitherP String p a' a b' b IO r
-- > heartbeat p = p `E.catch` \err -> do
-- >     lift $ putStrLn err  -- Print the error
-- >     heartbeat p          -- Restart 'p'
-- > 
-- > increment :: (Monad m, Proxy p) => () -> Producer (S.StateP Int p) Int m r
-- > increment () = forever $ do
-- >     n <- S.get
-- >     respond n
-- >     S.put (n + 1)
-- > 
-- > numbers :: (Monad m, Proxy p) => () -> Producer p Int m ()
-- > numbers () = runIdentityP $ do
-- >     (takeB_ 5 <-< S.evalStateK 10 increment) ()
-- >     S.evalStateK 1  (takeB_ 3 <-< increment) () -- This works, too!
-- > 
-- > increment2 :: (Proxy p) => () -> Consumer (S.StateP Int p) Int IO r
-- > increment2 () = forever $ do
-- >     nOurs   <- S.get
-- >     nTheirs <- request ()
-- >     lift $ print (nTheirs, nOurs)
-- >     S.put (nOurs + 2)
-- > 
-- > zipD
-- >     :: (Monad m, Proxy p1, Proxy p2, Proxy p3)
-- >     => () -> Consumer p1 a (Consumer p2 b (Producer p3 (a, b) m)) r
-- > zipD () =
-- >     runIdentityP . hoist (runIdentityP . hoist runIdentityP) $ forever $ do
-- >     -- Yes, this 'runIdentityP' mess is necessary.  Sorry!
-- > 
-- >         a <- request ()               -- Request from the outer 'Consumer'
-- >         b <- lift $ request ()        -- Request from the inner 'Consumer'
-- >         lift $ lift $ respond (a, b)  -- Respond to the 'Producer'
-- > 
-- > p1 = runProxyK $ zipD <-< fromListS [1..3]
-- > p2 = runProxyK $ p1 <-< fromListS [4..]
-- > p3 = runProxy $ printD <-< p2
-- > 
-- > fork
-- >     :: (Monad m, Proxy p1, Proxy p2, Proxy p3)
-- >     => () -> Consumer p1 a (Producer p2 a (Producer p3 a m)) r
-- > fork () =
-- >     runIdentityP . hoist (runIdentityP . hoist runIdentityP) $ forever $ do
-- >         a <- request ()          -- Request output from the 'Consumer'
-- >         lift $ respond a         -- Send output to the outer 'Producer'
-- >         lift $ lift $ respond a  -- Send output to the inner 'Producer'
-- > 
-- > {-
-- > p1 = runProxyK $ fork <-< fromListS [1..3]
-- > p2 = runProxyK $ raiseK printD <-< mapD (> 2) <-< p1
-- > p3 = runProxy  $ printD <-< mapD show <-< p2
-- > -}
-- > 
-- > {-
-- > p1 = runProxyK $ S.evalStateK 0 $ fork <-< increment
-- > p2 = runProxyK $ raiseK printD <-< mapD (+ 10) <-< p1
-- > p3 = runProxy  $ E.runEitherK $ printD <-< (takeB_ 3 >=> E.throw) <-< p2
-- > -}
-- > 
-- > twoLayers
-- >     :: (Proxy p)
-- >     => () -> Consumer (E.EitherP String (S.StateP Int p)) Int IO r
-- > twoLayers () = forever $ do
-- >     a <- request ()
-- >     if (a >= 0)
-- >         then liftP $ S.modify (+ a)
-- >         else E.throw "Negative number"
