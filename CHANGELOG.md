4.3.12

* Fix space leak introduced in version 4.3.10
    * This leak primarily affects the use of `forever`

4.3.11

* Fix documentation for `scanM`

4.3.10

* Relax `Monad` constraints to `Functor`
* Support GHC 8.8

4.3.9

* Increase upper bound on `exceptions`

4.3.8

* Increase upper bound on `exceptions`

4.3.7

* Documentation fix

4.3.6

* Fix implementation of `pass` in `MonadWriter` instance for `Proxy`

4.3.5

* Support `Semigroup` being a super-class of `Monoid`

4.3.4

* Increase upper bound on `mmorph`

4.3.3

* Make `X` a synonym for `Data.Void.Void`

4.3.2

* BUG FIX: Fix `MMonad` instance for `ListT`
    * The old instance was an infinite loop

4.3.1

* Support building against `ghc-7.4`

4.3.0

* BREAKING CHANGE: Remove `Alternative`/`MonadPlus` instances for `Proxy`
    * See commit 08e7302f43dbf2a40bd367c5ee73ee3367e17768 which explains why
* Add `Traversable` instance for `ListT`
* New `MonadThrow`/`MonadCatch`/`MMonad`/`Semigroup`/`MonadZip` instances for
  `ListT`
* New `MonadThrow`/`MonadCatch` instances for `Proxy`
* Fix lower bound on `mtl`
* Increase upper bound on `optparse-applicative`

4.2.0

* BREAKING CHANGE: Switch from `ErrorT` to `ExceptT`
* Add `Foldable` instance for `ListT`
* Fix all warnings
* Enable foldr/build fusion for `toList`

4.1.9

* Increase lower bound on `criterion`
* Increase upper bound on `transformers` for tests/benchmarks
* Optimize code by delaying `INLINABLE` annotations

4.1.8

* Increase upper bound on `transformers`
* Prepare for MRP (Monad of no Return Proposal)

4.1.7

* Increase lower bound on `deepseq`
* Add `unfoldr`
* Add `loop`
* Add `toListM'`
* Improve efficiency of `drop`
* License tutorial under Creative Commons license

4.1.6

* Increase lower bound on `base`
* Add diagrams to `Pipes.Core` documentation
* Add `mapM_`
* Add `takeWhile'`
* Add `seq`
* Improve efficiency of `toListM`

4.1.5

* Increase upper bound on `criterion`

4.1.4

* Increase upper bound on `criterion`
* Add `Monoid` instance for `Proxy`

4.1.3

* Increase lower bound on `mtl`
* Re-export `void`
* Add `fold'`
* Add `foldM'`

4.1.2

* Increase upper bounds on `transformers` and `mtl`

4.1.1

* Add `runListT`
* Add `MMonad` instance for `Proxy`
* Add `repeatM`
* Add laws to documentation of `Pipes.Prelude` utilities

4.1.0

* Remove Haskell98 support
* Use internal `X` type instead of `Data.Void`
* Document `Pipes.Lift` module:w
* Add `drain`
* Add `sequence`

4.0.2

* Improve performance of `each`
* Add tutorial appendix explaining how to work around quadratic time complexity

4.0.1

* Remove `WriterT` and `RWST` benchmarks
* Add `Enumerable` instance for `ErrorT`
* Add cabal flag for Haskell98 compilation
* Add several rewrite rules
* Add `mtl` instances for `ListT`
* Fix implementation of `pass`, which did not satisfy `Writer` laws
* Implement `fail` for `ListT`
* Add type synonym table to tutorial appendix
* Add QuickCheck tests for `pipes` laws
* Add `mapFoldable`
* Add `Monoid` instance for `ListT`
* Add manual proofs of `pipes` laws in `laws.md`

4.0.0

Major upgrade of `pipes` to no longer use `Proxy` type class
