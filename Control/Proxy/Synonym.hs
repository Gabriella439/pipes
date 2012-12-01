{-# LANGUAGE KindSignatures #-}

{-| These type synonyms simplify type signatures when proxies do not use all
    their type variables. -}

module Control.Proxy.Synonym (
    -- * Synonyms
    Pipe,
    Producer,
    Consumer,
    CoPipe,
    CoProducer,
    CoConsumer,
    Client,
    Server,
    Session,

    -- * Closed
    C
    ) where

-- | A unidirectional 'Proxy'.
type Pipe (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a b = p () a () b

{-| A 'Pipe' that produces values

    'Producer's never 'request'. -}
type Producer (p :: * -> * -> * -> * -> (* -> *) -> * -> *) b = p C () () b

{-| A 'Pipe' that consumes values

    'Consumer's never 'respond'. -}
type Consumer (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a = p () a () C

-- | A 'Pipe' where everything flows upstream
type CoPipe (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' b' = p a' () b' ()

{-| A 'CoPipe' that produces values flowing upstream

    'CoProducer's never 'respond'. -}
type CoProducer (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' = p a' () () C

{-| A 'CoConsumer' that consumes values flowing upstream

    'CoConsumer's never 'request'. -}
type CoConsumer (p :: * -> * -> * -> * -> (* -> *) -> * -> *) b' = p C () b' ()

{-| @Server b' b@ receives requests of type @b'@ and sends responses of type
    @b@.

    'Server's never 'request'. -}
type Server (p :: * -> * -> * -> * -> (* -> *) -> * -> *) b' b = p C () b' b

{-| @Client a' a@ sends requests of type @a'@ and receives responses of
    type @a@.

    'Client's never 'respond'. -}
type Client (p :: * -> * -> * -> * -> (* -> *) -> * -> *) a' a = p a' a () C

{-| A self-contained 'Session', ready to be run by 'runSession'

    'Session's never 'request' or 'respond'. -}
type Session (p :: * -> * -> * -> * -> (* -> *) -> * -> *) = p C () () C

-- | The empty type, denoting a \'@C@\'losed end
data C = C -- Constructor not exported, but I include it to avoid EmptyDataDecls
