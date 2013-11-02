module Main (main) where

import Data.Function                        (on)
import Data.List                            (intercalate)
import Control.Monad                        ((>=>))
import Control.Monad.Trans.Writer           (Writer, runWriter, tell)
import Test.QuickCheck                      (Gen, Arbitrary(..), choose)
import Test.Framework                       (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Pipes
import Pipes.Core
import Prelude hiding (log)


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [
      testGroup "Kleisli Category"        $ testCategory (>=>) return
    , testGroup "Respond Category"        $ testCategory (/>/) respond
       ++ [
            testProperty "Distributivity"  prop_respond_Distributivity
          ]
    , testGroup "Request Category"        $ testCategory (\>\) request
      ++ [
            testProperty "Distributivity"  prop_request_Distributivity
          , testProperty "Zero Law"        prop_request_ZeroLaw
         ]
    , testGroup "Pull Category"           $ testCategory (>+>) pull
    , testGroup "Push Category"           $ testCategory (>~>) push
    , testGroup "Push/Pull" [
        testProperty "Associativity"       prop_pushPull_Associativity
      ]
    , testGroup "Duals" [
          testGroup "Request" [
            testProperty "Composition"     prop_dual_RequestComposition
          , testProperty "Identity"        prop_dual_RequestIdentity
          ]
        , testGroup "Respond" [
            testProperty "Composition"     prop_dual_RespondComposition
          , testProperty "Identity"        prop_dual_RespondIdentity
          ]
      , testProperty "Distributivity"      prop_dual_ReflectDistributivity
      , testProperty "Zero Law"            prop_dual_ReflectZeroLaw
      , testProperty "Involution"          prop_dual_Involution
      ]
    , testGroup "Functor Laws" [
        testProperty "Identity"            prop_FunctorIdentity
      ]
    ]

arbitraryBoundedEnum' :: (Bounded a, Enum a) => Gen a
arbitraryBoundedEnum' =
  do let mn = minBound
         mx = maxBound `asTypeOf` mn
     n <- choose (fromEnum mn, fromEnum mx)
     return (toEnum n `asTypeOf` mn)

data ClientStep
    = ClientRequest
    | ClientLog
    | ClientInc
      deriving (Enum, Bounded)

instance Arbitrary ClientStep where
    arbitrary = arbitraryBoundedEnum'
    shrink _  = []

instance Show ClientStep where
    show x = case x of
        ClientRequest -> "request"
        ClientLog     -> "log"
        ClientInc     -> "inc"

data ServerStep
    = ServerRespond
    | ServerLog
    | ServerInc
      deriving (Enum, Bounded)

instance Arbitrary ServerStep where
    arbitrary = arbitraryBoundedEnum'
    shrink _  = []

instance Show ServerStep where
    show x = case x of
        ServerRespond -> "respond"
        ServerLog     -> "log"
        ServerInc     -> "inc"

data ProxyStep
    = ProxyRequest
    | ProxyRespond
    | ProxyLog
    | ProxyInc deriving (Enum, Bounded)

instance Arbitrary ProxyStep where
    arbitrary = arbitraryBoundedEnum'
    shrink _  = []

instance Show ProxyStep where
    show x = case x of
        ProxyRequest -> "request"
        ProxyRespond -> "respond"
        ProxyLog     -> "log"
        ProxyInc     -> "inc"

log :: Int -> Proxy a' a b' b (Writer [Int]) Int
log n = do
    lift (tell [n])
    return n

inc :: (Monad m) => Int -> Proxy a' a b' b m Int
inc n = return (n + 1)

correct :: String -> String
correct str = case str of
    [] -> "return"
    _  -> str

newtype AClient = AClient { unAClient :: [ClientStep] }

instance Arbitrary AClient where
    arbitrary = fmap AClient arbitrary
    shrink    = map AClient . shrink . unAClient

instance Show AClient where
    show = correct . intercalate " >=> " . map show . unAClient

aClient :: AClient -> Int -> Client Int Int (Writer [Int]) Int
aClient = foldr (>=>) return . map f . unAClient
  where
    f x = case x of
        ClientRequest -> request
        ClientLog     -> log
        ClientInc     -> inc

newtype AServer = AServer { unAServer :: [ServerStep] }

instance Arbitrary AServer where
    arbitrary = fmap AServer arbitrary
    shrink    = map AServer . shrink . unAServer

instance Show AServer where
    show = correct . intercalate " >=> " . map show . unAServer

aServer :: AServer -> Int -> Server Int Int (Writer [Int]) Int
aServer = foldr (>=>) return . map f . unAServer
  where
    f x = case x of
        ServerRespond -> respond
        ServerLog     -> log
        ServerInc     -> inc

newtype AProxy = AProxy { unAProxy :: [ProxyStep] }

instance Arbitrary AProxy where
    arbitrary = fmap AProxy arbitrary
    shrink    = map AProxy . shrink . unAProxy

instance Show AProxy where
    show = correct . intercalate " >=> " . map show . unAProxy


aProxy :: AProxy -> Int -> Proxy Int Int Int Int (Writer [Int]) Int
aProxy = foldr (>=>) return . map f . unAProxy
  where
    f x = case x of
        ProxyRequest -> request
        ProxyRespond -> respond
        ProxyLog     -> log
        ProxyInc     -> inc

type ProxyK    = Int -> Proxy Int Int Int Int (Writer [Int]) Int
type Operation = ProxyK -> ProxyK -> ProxyK

infix 0 >==<

(>==<) :: ProxyK -> ProxyK -> AServer -> AClient -> Bool
(>==<) pl pr p0 p1 =
  let sv  = aServer p0
      cl  = aClient p1
      f p = runWriter (runEffect (p 0))
  in on (==) f (sv >+> pl >+> cl) (sv >+> pr >+> cl)

gen_prop_RightIdentity, gen_prop_LeftIdentity
  :: Operation
  -> ProxyK -- right/left identity element
  -> AProxy -> AServer -> AClient -> Bool
gen_prop_RightIdentity (>>>) idt f' =
    let f = aProxy  f'
    in (f >>> idt) >==< f

gen_prop_LeftIdentity (>>>) idt f' =
    let f = aProxy f'
    in (idt >>> f) >==< f

gen_prop_Associativity
  :: Operation
  -> AProxy -> AProxy -> AProxy -> AServer -> AClient -> Bool
gen_prop_Associativity (>>>) f' g' h' =
    let f = aProxy  f'
        g = aProxy  g'
        h = aProxy  h'
    in f >>> (g >>> h) >==< (f >>> g) >>> h

testCategory :: Operation -> ProxyK -> [Test]
testCategory op idt =
    [
      testProperty "Left Identity"  $ gen_prop_LeftIdentity  op idt
    , testProperty "Right Identity" $ gen_prop_RightIdentity op idt
    , testProperty "Associativity"  $ gen_prop_Associativity op
    ]

-- Respond Category

prop_respond_Distributivity f' g' h' =
    let f = aProxy  f'
        g = aProxy  g'
        h = aProxy  h'
    in (f >=> g) />/ h >==< (f />/ h) >=> (g />/ h)

-- Request Category

prop_request_Distributivity f' g' h' =
    let f = aProxy  f'
        g = aProxy  g'
        h = aProxy  h'
    in f \>\ (g >=> h) >==< (f \>\ g) >=> (f \>\ h)

prop_request_ZeroLaw f' =
    let f = aProxy  f'
    in (f \>\ return) >==< return

-- Push/Pull

prop_pushPull_Associativity f' g' h' =
    let f = aProxy f'
        g = aProxy g'
        h = aProxy h'
    in (f >+> g) >~> h >==< f >+> (g >~> h)

-- Duals

prop_dual_RequestComposition f' g' =
    let f = aProxy f'
        g = aProxy g'
    in reflect . (f \>\ g) >==< reflect . g />/ reflect . f

prop_dual_RequestIdentity = reflect . request >==< respond

prop_dual_RespondComposition f' g' =
    let f = aProxy f'
        g = aProxy g'
    in  reflect . (f />/ g) >==< reflect . g \>\ reflect . f

prop_dual_RespondIdentity = reflect . respond >==< request

prop_dual_ReflectDistributivity f' g' =
    let f = aProxy f'
        g = aProxy g'
    in reflect . (f >=> g) >==< reflect . f >=> reflect . g

prop_dual_ReflectZeroLaw = reflect . return >==< return

prop_dual_Involution f' =
    let f = aProxy f'
    in (reflect . reflect) . f >=> return >==< f

-- Functor Laws

prop_FunctorIdentity p' =
    let p = aProxy p'
    in fmap id p >==< id p
