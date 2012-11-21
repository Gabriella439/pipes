{-# LANGUAGE Rank2Types, KindSignatures #-}

module Control.Proxy.Synonym (
    Pipe,
    Producer,
    Consumer,
    Pipeline,
    Client,
    Server
    ) where

{-| The type variables of @Pipe a b m r@ signify:

    * @a@ - The type of input received from upstream pipes

    * @b@ - The type of output delivered to downstream pipes

    * @m@ - The base monad

    * @r@ - The type of the return value -}
type Pipe   p a b (m :: * -> *) r = p () a () b m r

-- | A pipe that produces values
type Producer p b m r = forall a   . Pipe p a b m r

-- | A pipe that consumes values
type Consumer p a m r = forall   b . Pipe p a b m r

-- | A self-contained pipeline that is ready to be run
type Pipeline   p m r = forall a b . Pipe p a b m r

{-| @Server req resp@ receives requests of type @req@ and sends responses of
    type @resp@.

    'Server's only 'respond' and never 'request' anything. -}
type Server p req resp m r = forall a' a      . p a'  a    req resp m r

{-| @Client req resp@ sends requests of type @req@ and receives responses of
    type @resp@.

    'Client's only 'request' and never 'respond' to anything. -}
type Client p req resp m r = forall      b' b . p req resp b'  b    m r

{-| A self-contained 'Session', ready to be run by 'runSession'

    'Session's never 'request' anything or 'respond' to anything. -}
type Session p         m r = forall a' a b' b . p a'  a    b'  b    m r
