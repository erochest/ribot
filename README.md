
# Overview

This is a toy IRC bot written using Haskell.

# Architecture

This is a chance for me to play with structuring a program using [monads][1].

Here are the monads I plan to stack up:

* `IO` provides network interaction and IRC parsing and commands.
* `Writer` provides logging.
* `Either` provides error handling.

[1]: http://en.wikipedia.org/wiki/Monad_(functional_programming) "Monad (functional programming)"

