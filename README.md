# Pipes v3.2.0

`pipes` provides an easy, elegant, and powerful framework for describing streaming behaviors.

## Features

* *Simple API*: Use three simple commands: `(>->)`, `request`, and `respond`
* *Bidirectionality*: Implement duplex channels 
* *Blazing fast*: Implementation tuned for speed 
* *Elegant semantics*: Use practical category theory
* *Extension Framework*: Mix and match extensions and create your own
* *ListT*: Correct implementation of `ListT` that interconverts with pipes 
* *Lightweight Dependency*: `pipes` depends only on `transformers` and compiles rapidly
* *Extensive Documentation*: Second to none!

## Quick start

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* `cabal install pipes`

Then fire up ` ghci`:

    Prelude> import Control.Proxy as P

... and stream standard input to standard output until you enter `quit`.

    Prelude P> runProxy $ stdinS >-> takeWhileD (/= "quit") >-> stdoutD
    Test[Enter]
    Test
    Apple[Enter]
    Apple
    quit[Enter]
    Prelude P> -- Done!

Now read [the official tutorial](http://hackage.haskell.org/packages/archive/pipes/3.2.0/doc/html/Control-Proxy-Tutorial.html).
