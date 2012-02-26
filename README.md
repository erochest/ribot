
# Overview

This is a toy IRC bot written using Haskell.

The site for this is on [my site](http://www.ericrochester.com/ribot).

# Work Plan

1. <del>Connect/IRC</del>;
1. <del>CLI (`cmdargs`)</del>;
1. <del>Read configuration from a file (`configurator`)</del>;
1. <del>Daemonize (`hsdaemonzie`)</del> and automatically re-connect;
1. Refactor into modules;
1. Add documentation on what there is so far;
1. Run long commands in another thread;
1. PasteBin;
1. `!echo`;
1. Database (`persistent`);
1. Log messages;
1. Log topics;
1. `!log (off|on)`;
1. `!uptime`;
1. `!version`;
1. `!help`;
1. Tokenize and index messages;
1. Reindex command;
1. `!search`;
1. `!mimic`;
1. Convert data from old database;
1. Deploy task;
1. `!stats`;
1. Notifications and messages;
1. Browsing site (`yesod`).

# Commands

* `!help` — print this list.
* `!version` — print out the version of the bot.
* `!uptime` — print how long the bot has been running.
* `!log off` — turn off logging for the user.
* `!log on` — turn on logging for the user.
* `!echo STRING` — echo back.
* `!search QUERY` — search the log of messages.
* `!mimic NICK` — mimic the language of the user nick.

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

# IRC

For future reference, the IRC is defined by [RFC 1459][spec].

# Source Documentation

The annotated source branches off of this page:

* [Ribot](http://www.ericrochester.com/ribot/docs/)

[spec]: http://tools.ietf.org/html/rfc1459 "RFC 1459"

