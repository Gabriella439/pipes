{-# LANGUAGE KindSignatures #-}

{-| These type synonyms simplify type signatures when proxies do not use all
    their type variables. -}

module Control.Proxy.Synonym (
    Pipe,
    Producer,
    Consumer,
    CoPipe,
    CoProducer,
    CoConsumer,
    Pipeline,
    Client,
    Server
    ) where

import Data.Closed (C)

-- | A unidirectional 'Proxy'.
type Pipe p a b (m :: * -> *) r = p () a () b m r

{-| A 'Pipe' that produces values

    'Producer's never 'request'. -}
type Producer p b (m :: * -> *) r = p C () () b m r

{-| A 'Pipe' that consumes values

    'Consumer's never 'respond'. -}
type Consumer p a (m :: * -> *) r = p () a () C m r

{-| A self-contained 'Pipeline' that is ready to be run

    'Pipeline's never 'request' nor 'respond'. -}
type Pipeline p (m :: * -> *) r = p C () () C m r

-- | A 'Pipe' where everything flows upstream
type CoPipe p a' b' (m :: * -> *) r = p a' () b' () m r

{-| A 'CoPipe' that produces values flowing upstream

    'CoProducer's never 'respond'. -}
type CoProducer p a' (m :: * -> *) r = p a' () () C m r

{-| A 'CoConsumer' that consumes values flowing upstream

    'CoConsumer's never 'request'. -}
type CoConsumer p b' (m :: * -> *) r = p C () b' () m r

{-| @Server b' b@ receives requests of type @b'@ and sends responses of type
    @b@.

    'Server's never 'request'. -}
type Server p b' b (m :: * -> *) r = p C () b' b m r

{-| @Client a' a@ sends requests of type @a'@ and receives responses of
    type @a@.

    'Client's never 'respond'. -}
type Client p a' a (m :: * -> *) r = p a' a () C m r

{-| A self-contained 'Session', ready to be run by 'runSession'

    'Session's never 'request' or 'respond'. -}
type Session p (m :: * -> *) r = p C () () C m r
