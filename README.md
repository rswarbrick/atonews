atonews
=======

This is a program designed to get news (in a general sense) and turn
it into news (in a specific sense: stored on an nntp server). One can
think of it as an RSS reader for sites that don't do RSS...

What it does
------------

atonews consists of various "news sources". Each one of them grabs
news (usually from a website). Currently, they all work by looking at
some sort of index page and then getting a page per actual
article. Finally there's an HTML washing step, where the actual
article to be posted is generated.

What then happens is that the article gets pushed to a news
server. Here, I'm using a leafnode instance on my machine with the
groups in the local heirarchy.

Configuration
-------------

At the moment, it's only going to be useful for me since there's no
configuration file. But that would be easy to change if anyone else
thinks this is interesting.

Whatever happens, there's a certain amount of basic configuration that
will always be required outside of atonews proper, since leafnode
stores its list of local groups in a file in /etc and (rightly!) you
need root privileges to alter that list.

Comparison with nnshimbun
-------------------------

atonews runs outside of Emacs, so you don't have to have Gnus (or
whatever) freeze for ages every time you want to update your mail
groups.
