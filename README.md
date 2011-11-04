
# Overview

This is a toy IRC bot written using Haskell.

The site for this is on [my site](http://www.ericrochester.com/ribot).

# Features

* Chat logging;
* <del>Log searches</del>;
* <del>Mimicking</del>.

# Commands

* `!help` — print this list.
* `!version` — print out the version of the bot.
* `!uptime` — print how long the bot has been running.
* `!log off` — turn off logging for the user.
* `!log on` — turn on logging for the user.
* `!echo STRING` — echo back.
* `!search QUERY` — search the log of messages.

# Search Queries

The search queries are at the moment intentionally simple. At some point in the
future, I may add one or two facets (`nick` or `date` come to mind), but at the
moment, it simply allows you to include a list of terms, possibly with
wild-cards, to search for.

For example, here are some queries and what they would search for:

* `youtube` — any messages with the term *youtube*;
* `youtube video` — any messages with both terms *youtube* and *video*;
* `youtube video*` — any messages with both the term *youtube* and any terms
  beginning with *video*.

# TODOs

* <del>add a database connection to the monad</del>;
* <del>figure out how to locate the database</del>;
* <del>open the database, creating the schema if needed</del>;
* <del>log to the database</del>;
* <del>`!log off` and `!log on`</del>;
* `!search QUERY`;
* `!mimic NICK`;
* automatically reconnect after network outages.

# Architecture

This is a chance for me to play with structuring a program using [monads][1].

# IRC

For future reference, the IRC is defined by [RFC 1459][2].

# Source Documentation

The annotated source is at these locations:

* [Ribot.hs](http://www.ericrochester.com/ribot/docs/Ribot.html)
* [Network.Ribot.Core](http://www.ericrochester.com/ribot/docs/Core.html)
* [Network.Ribot.Db](http://www.ericrochester.com/ribot/docs/Db.html)
* [Network.Ribot.Message](http://www.ericrochester.com/ribot/docs/Message.html)
* [Network.Ribot.Utils](http://www.ericrochester.com/ribot/docs/Utils.html)
* [Text.Ribot.Tokenizer](http://www.ericrochester.com/ribot/docs/Tokenizer.html)

[1]: http://en.wikipedia.org/wiki/Monad_(functional_programming) "Monad (functional programming)"
[2]: http://tools.ietf.org/html/rfc1459 "RFC 1459"

