{-# LANGUAGE GADTs, TypeOperators #-}

import Control.Monad.Instances ()
import Control.IMonad
import Control.IMonad.Trans
import Control.IMonad.Trans.Free
import Data.Maybe
import Data.Void

data C
data O a
type M a = O (Maybe a)

data FrameF b x i where
    Yield ::  b -> x    i   -> FrameF b x  i
    Await :: (a -> x (O a)) -> FrameF b x (O a)
    Close ::       x    C   -> FrameF b x (O a)

instance IFunctor (FrameF b) where
    fmapI f p = case p of
        Yield b y -> Yield b (f y)
        Await a   -> Await (f . a)
        Close c   -> Close (f c)

yieldF :: (Monad m) => m () -> b -> IFreeT (FrameF (m (), b)) (U m) (() := i) i
yieldF m x = liftF $ Yield (m, x) (V ())

awaitF :: (Monad m) => IFreeT (FrameF (m (), b)) (U m) (Maybe a := M a) (M a)
awaitF = liftF $ Await V

close :: (Monad m) => IFreeT (FrameF b) (U m) (() := C) (M a)
close = liftF $ Close (V ())

yield :: (Monad m) => b -> IFreeT (FrameF (m (), b)) (U m) (() := i) i
yield = yieldF (return ())

await :: (Monad m) => IFreeT (FrameF (m (), b)) (U m) (a := M a) (M a)
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

idP :: (Monad m) => IFreeT (FrameF (m (), a)) (U m) (r := C) (M a)
idP = foreverR $ await !>= yield

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
 => IFreeT (FrameF (m (), c)) (U m) (r := C) (M b)
 -> IFreeT (FrameF (m (), b)) (U m) (r := C) (M a)
 -> IFreeT (FrameF (m (), c)) (U m) (r := C) (M a)
p1 <-< p2 = heap (return ()) p1 <~< stack False p2

runFrame :: (Monad m)
 => IFreeT (FrameF (m (), Void)) (U m) (r := C) (M ()) -> m r
runFrame p = do
    x <- unU $ runIFreeT p
    case x of
        Wrap (Close   p') -> runFrame' p'
        Wrap (Yield _ p') -> runFrame  p'
        Wrap (Await   f ) -> runFrame (f $ Just ())

runFrame' :: (Monad m)
 => IFreeT (FrameF (m (), Void)) (U m) (r := C) C -> m r
runFrame' p = do
    x <- unU $ runIFreeT p
    case x of
        Return (V r)      -> return r
        Wrap (Yield _ p') -> runFrame'  p'
