
# Overview

This is a toy IRC bot written using Haskell.

The site for this is [here](http://www.ericrochester.com/ribot).

# Work Plan

1. <del>Connect/IRC</del>;
1. <del>CLI (`cmdargs`)</del>;
1. <del>Read configuration from a file (`configurator`)</del>;
1. <del>Daemonize (`hsdaemonzie`) and automatically re-connect</del>;
1. <del>Refactor into modules</del>;
1. <del>Add documentation on what there is so far</del>;
1. <del>PasteBin</del>;
1. <del>`!echo`</del>;
1. <del>Use absolute paths for files specified in config files</del>;
1. <del>Database (`persistent`)</del>;
1. <del>Log messages</del>;
1. <del>Log topics</del>;
1. `!log (off|on)`;
1. `!uptime`;
1. `!version`;
1. `!help`;
1. Tokenize and index messages;
1. Run indexing and search commands in another thread;
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

