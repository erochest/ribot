
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
* <del>`!search QUERY`</del>;
* `!mimic NICK`;
* automatically reconnect after network outages.

# Architecture

This is a chance for me to play with structuring a program using [monads][1].

# IRC

For future reference, the IRC is defined by [RFC 1459][2].

# Source Documentation

The annotated source is at these locations:

* [Ribot](http://www.ericrochester.com/ribot/docs/Ribot.html)
* [Ribot.Cli](http://www.ericrochester.com/ribot/docs/Cli.html)
* [Ribot.Modes](http://www.ericrochester.com/ribot/docs/Modes.html)
* [Ribot.Listen](http://www.ericrochester.com/ribot/docs/Listen.html)
* [Ribot.Reindex](http://www.ericrochester.com/ribot/docs/Reindex.html)
* [Control.Ribot](http://www.ericrochester.com/ribot/docs/Ribot.html)
* [Database.Ribot](http://www.ericrochester.com/ribot/docs/Ribot.html)
* [Network.Ribot.Irc](http://www.ericrochester.com/ribot/docs/Irc.html)
* [Network.Ribot.Irc.Types](http://www.ericrochester.com/ribot/docs/Types.html)
* [Text.Ribot.Search](http://www.ericrochester.com/ribot/docs/Search.html)
* [Text.Ribot.Tokenizer](http://www.ericrochester.com/ribot/docs/Tokenizer.html)
* [Text.Ribot.Utils](http://www.ericrochester.com/ribot/docs/Utils.html)

[1]: http://en.wikipedia.org/wiki/Monad_(functional_programming) "Monad (functional programming)"
[2]: http://tools.ietf.org/html/rfc1459 "RFC 1459"

