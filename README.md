# Pipes v4.3.7

`pipes` is a clean and powerful stream processing library that lets you build
and connect reusable streaming components.

## Quick start

* Install the [Haskell Platform](http://www.haskell.org/platform/)
* `cabal install pipes`

Then fire up `ghci`:

    $ ghci
    Prelude> import Pipes
    Prelude Pipes> import qualified Pipes.Prelude as P

... and echo standard input to standard output until you enter `quit`.

    Prelude Pipes P> runEffect $ P.stdinLn >-> P.takeWhile (/= "quit") >-> P.stdoutLn
    Test[Enter]
    Test
    Apple[Enter]
    Apple
    quit[Enter]
    Prelude P> -- Done!

To learn more, read the
[pipes tutorial](http://hackage.haskell.org/package/pipes/docs/Pipes-Tutorial.html).

## Features

* *Concise API*: Use simple commands like `for`, `(>->)`, `await`, and `yield`

* *Blazing fast*: Implementation tuned for speed, including shortcut fusion

* *Lightweight Dependency*: `pipes` is small and compiles very rapidly,
  including dependencies

* *Elegant semantics*: Use practical category theory

* *ListT*: Correct implementation of `ListT` that interconverts with pipes

* *Bidirectionality*: Implement duplex channels

* *Extensive Documentation*: Second to none!

## Philosophy

The `pipes` library emphasizes the following three design precepts:

* Emphasize elegance - Elegant libraries replace inelegant libraries

* Theory everywhere - Principled design promotes intuitive behavior

* Minimize dependencies - Small dependency profiles maximize portability

## Outline

The core `pipes` ecosystem consists of the following four libraries:

* `pipes`: The elegant, theoretically-principled core.

* `pipes-concurrency`: Message passing and reactive programming

* `pipes-parse`: Utilities for parsing streams

* `pipes-safe`: Resource management and exception safety

These represent the core areas that I envision for `pipes`.  The latter three
libraries represent the more pragmatic face of the ecosystem and make design
tradeoffs where there is no known elegant solution.

Derived libraries that build on top of these include:

* `pipes-network` and `pipes-network-tls`: Networking support

* `pipes-attoparsec`: High-efficiency streaming parsing

* `pipes-zlib`: Compression and decompression

* `pipes-binary`: Streaming serialization and deserialization

* `pipes-aeson`: Streaming JSON encoding and decoding

## Development Status

[![Build Status](https://travis-ci.org/Gabriel439/Haskell-Pipes-Library.png)](https://travis-ci.org/Gabriel439/Haskell-Pipes-Library)

`pipes` is stable, and current work focuses on packaging `pipes` for various
package managers.  The long term goal is to get `pipes` into the Haskell
platform and become the basic building block for streaming APIs.

## Community Resources

* [Haskell wiki page](http://www.haskell.org/haskellwiki/Pipes)

* [Mailing list](mailto:haskell-pipes@googlegroups.com) ([Google Group](https://groups.google.com/forum/?fromgroups#!forum/haskell-pipes))

## How to contribute

* Contribute code

* Build derived libraries

* Write `pipes` tutorials

## License (BSD 3-clause)

Copyright (c) 2012-2014 Gabriel Gonzalez
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Gabriel Gonzalez nor the names of other contributors may
  be used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
