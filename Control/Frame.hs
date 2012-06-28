{-# LANGUAGE GADTs, TypeOperators #-}

module Control.Frame (
    C,
    O,
    M,
    FrameF(..),
    Frame,
    yieldF,
    awaitF,
    close,
    yield,
    await,
    (<-<),
    idF,
    FrameC(..),
    runFrame
    ) where

import Control.Category
import Control.IMonad
import Control.IMonad.Trans
import Control.IMonad.Trans.Free
import Control.Monad.Instances ()
import Data.Maybe
import Data.Void
import Prelude hiding ((.), id)

-- | Input end is open, receiving values of type @a@
data O a

-- | Input end is closed
data C

-- | Input end is open, receiving values of type @Maybe a@
type M a = O (Maybe a)

{-|
    Base functor for a pipe that can close its input end

    * @b@ - Output type
    * @x@ - Next step
    * @i@ - Current step's index
-}
data FrameF b x i where
    Yield ::  b -> x    i   -> FrameF b x  i
    Await :: (a -> x (O a)) -> FrameF b x (O a)
    Close ::       x    C   -> FrameF b x (O a)

instance IFunctor (FrameF b) where
    fmapI f p = case p of
        Yield b y -> Yield b (f y)
        Await a   -> Await (f . a)
        Close c   -> Close (f c)

type Frame b m i j r = IFreeT (FrameF (m (), b)) (U m) (r := j) i

yieldF :: (Monad m) => m () -> b -> Frame b m i i ()
yieldF m x = liftF $ Yield (m, x) (V ())

awaitF :: (Monad m) => Frame b m (M a) (M a) (Maybe a)
awaitF = liftF $ Await V

close :: (Monad m) => Frame b m (M a) C ()
close = liftF $ Close (V ())

yield :: (Monad m) => b -> Frame b m i i ()
yield = yieldF (return ())

await :: (Monad m) => Frame b m (M a) (M a) a
await = awaitF !>= maybe await returnR

(<~<) :: (Monad m)
 => IFreeT (FrameF c) (U m) (r := C) (O b)
 -> IFreeT (FrameF b) (U m) (r := C) (O a)
 -> IFreeT (FrameF c) (U m) (r := C) (O a)
p1 <~< p2 = IFreeT $ U $ do
    x1 <- unU $ runIFreeT p1
    unU $ runIFreeT $ case x1 of
        Wrap (Close   p1') -> wrap $ Close p1'
        Wrap (Yield c p1') -> wrap $ Yield c (p1' <~< p2)
        Wrap (Await   f1 ) -> IFreeT $ U $ do
            x2 <- unU $ runIFreeT p2
            let p1' = IFreeT $ returnI x1
            unU $ runIFreeT $ case x2 of
                Wrap (Close p2')   -> wrap $ Close $ p1' <~| p2'
                Wrap (Yield b p2') -> f1 b <~< p2'
                Wrap (Await f2) -> wrap $ Await $ fmap (\p2'-> p1' <~< p2') f2

(<~|) :: (Monad m)
 => IFreeT (FrameF c) (U m) (r := C) (O b)
 -> IFreeT (FrameF b) (U m) (r := C)  C
 -> IFreeT (FrameF c) (U m) (r := C)  C
p1 <~| p2 = IFreeT $ U $ do
    x1 <- unU $ runIFreeT p1
    unU $ runIFreeT $ case x1 of
        Wrap (Close   p1') -> p1'
        Wrap (Yield c p1') -> wrap $ Yield c (p1' <~| p2)
        Wrap (Await   f1 ) -> IFreeT $ U $ do
            x2 <- unU $ runIFreeT p2
            let p1' = IFreeT $ returnI x1
            unU $ runIFreeT $ case x2 of
                Return r           -> returnI r
                Wrap (Yield b p2') -> f1 b <~| p2' 

heap :: (Monad m)
 => m ()
 -> IFreeT (FrameF (m (), c)) (U m) (r := C) (M        b )
 -> IFreeT (FrameF (m (), c)) (U m) (r := C) (M (m (), b))
heap m p = IFreeT $ U $ do
    x <- unU $ runIFreeT p
    unU $ runIFreeT $ case x of
        Wrap (Close         p') -> wrap $ Close $ liftU m !> p'
        Wrap (Yield (m', c) p') -> wrap $ Yield (m >> m', c) (heap m p')
        Wrap (Await         f ) -> wrap $ Await $ \e -> case e of
            Nothing      -> heap (return ()) (f  Nothing)
            Just (m', b) -> heap m'          (f $ Just b)

stack :: (Monad m)
 => Bool
 -> IFreeT (FrameF        b ) (U m) (r := C) (M a)
 -> IFreeT (FrameF (Maybe b)) (U m) (r := C) (M a)
stack t p = IFreeT $ U $ do
    x <- unU $ runIFreeT p
    unU $ runIFreeT $ case x of
        Wrap (Close   p') -> wrap $ Close $ warn p'
        Wrap (Yield b p') -> wrap $ Yield (Just b) (stack t p')
        Wrap (Await   f ) -> wrap $ Await $ \e -> case t of
            False ->                        stack (isJust e) (f e)
            True  -> wrap $ Yield Nothing $ stack (isJust e) (f e)

warn :: (Monad m)
 => IFreeT (FrameF        b ) (U m) (r := C) C
 -> IFreeT (FrameF (Maybe b)) (U m) (r := C) C
warn p = IFreeT $ U $ do
    x <- unU $ runIFreeT p
    unU $ runIFreeT $ case x of
        Return r -> wrap $ Yield Nothing (returnI r)
        Wrap (Yield b p') -> wrap $ Yield (Just b) (warn  p')

(<-<) :: Monad m
 => Frame c m (M b) C r -- IFreeT (FrameF (m (), c)) (U m) (r := C) (M b)
 -> Frame b m (M a) C r -- IFreeT (FrameF (m (), b)) (U m) (r := C) (M a)
 -> Frame c m (M a) C r -- IFreeT (FrameF (m (), c)) (U m) (r := C) (M a)
p1 <-< p2 = heap (return ()) p1 <~< stack False p2

idF :: (Monad m) => Frame a m (M a) C r -- IFreeT (FrameF (m (), a)) (U m) (r := C) (M a)
idF = foreverR $ await !>= yield

newtype FrameC m r a b = FrameC { unFrameC :: Frame b m (M a) C r }

instance (Monad m) => Category (FrameC m r) where
    id = FrameC idF
    (FrameC p1) . (FrameC p2) = FrameC (p1 <-< p2)

runFrame :: (Monad m)
 => Frame Void m (M ()) C r -> m r -- IFreeT (FrameF (m (), Void)) (U m) (r := C) (M ()) -> m r
runFrame p = do
    x <- unU $ runIFreeT p
    case x of
        Wrap (Close   p') -> runFrame' p'
        Wrap (Yield _ p') -> runFrame  p'
        Wrap (Await   f ) -> runFrame (f $ Just ())

runFrame' :: (Monad m)
 => Frame Void m C C r -> m r -- IFreeT (FrameF (m (), Void)) (U m) (r := C) C -> m r
runFrame' p = do
    x <- unU $ runIFreeT p
    case x of
        Return (V r)      -> return r
        Wrap (Yield _ p') -> runFrame'  p'
