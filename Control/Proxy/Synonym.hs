{-# LANGUAGE KindSignatures #-}

{-| These type synonyms simplify type signatures when proxies do not use all
    their type variables. -}

module Control.Proxy.Synonym (
    Pipe,
    Producer,
    Consumer,
    Pipeline,
    Client,
    Server
    ) where

import Data.Closed (C)

{-| The type variables of @Pipe a b m r@ signify:

    * @a@ - The type of input received from upstream pipes

    * @b@ - The type of output delivered to downstream pipes

    * @m@ - The base monad

    * @r@ - The type of the return value -}
type Pipe   p a b (m :: * -> *) r = p () a () b m r

-- | A pipe that produces values
type Producer p b m r = Pipe p () b m r

-- | A pipe that consumes values
type Consumer p a m r = Pipe p a C m r

-- | A self-contained pipeline that is ready to be run
type Pipeline p   m r = Pipe p () C m r

{-| @Server req resp@ receives requests of type @req@ and sends responses of
    type @resp@.

    'Server's only 'respond' and never 'request' anything. -}
type Server p req resp m r = p C   ()   req resp m r

{-| @Client req resp@ sends requests of type @req@ and receives responses of
    type @resp@.

    'Client's only 'request' and never 'respond' to anything. -}
type Client p req resp m r = p req resp ()  C    m r

{-| A self-contained 'Session', ready to be run by 'runSession'

    'Session's never 'request' anything or 'respond' to anything. -}
type Session p         m r = p C   ()   ()  C    m r
