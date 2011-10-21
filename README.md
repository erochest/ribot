
# Overview

This is a toy IRC bot written using Haskell.

The site for this is on [my site](http://www.ericrochester.com/ribot).

# Features

* <del>Chat logging</del>;
* <del>Log searches</del>;
* <del>Mimicking</del>.

# Architecture

This is a chance for me to play with structuring a program using [monads][1].

# IRC

For future reference, the IRC is defined by [RFC 1459][2].

# Database

## TODOs

* <del>add a database connection to the monad</del>;
* <del>figure out how to locate the database</del>;
* open the database, creating the schema if needed;
* log to the database;
* search the database.

## Schema

**user**

* name
* last seen

**message**

* user
* text
* posted

**token**

* text

**position**

* token
* message
* offset
* length

[1]: http://en.wikipedia.org/wiki/Monad_(functional_programming) "Monad (functional programming)"
[2]: http://tools.ietf.org/html/rfc1459 "RFC 1459"

