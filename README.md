
# Overview

This is a toy IRC bot written using Haskell.

# Features

* <span style='color:silver';>Chat logging;</span>
* <span style='color:silver';>Log searches.</span>

# Architecture

This is a chance for me to play with structuring a program using [monads][1].

# IRC

For future reference, the IRC is defined by [RFC 1459][2].

# Database

## TODOs

* add a database connection to the monad;
* figure out how to location the database;
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

