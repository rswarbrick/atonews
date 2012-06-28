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

How messages are fetched
------------------------

The top-level function called is UPDATE-NEWS-SOURCE, which calls
FORCE-UPDATE-NEWS-SOURCE. The latter calls NEW-HEADERS to check for
new messages. The default implementation for this fetches all
(recent?) headers for the news source (eg by parsing an index page),
then checks on the news server to see whether they've been seen
before.

The function that finds all new headers is LIST-HEADERS. This works by
getting the relevant data (with GET-HEADER-DATA) and parsing it for
message fragments with FIND-MESSAGE-FRAGMENTS. The default
implementation of FIND-MESSAGE-FRAGMENTS is designed to make
regexp-based solutions easy to write: it repeatedly calls
NEXT-MESSAGE-FRAGMENT. For a more tree-like approach (eg if you've
parsed the data from XML), you probably want to implement all of
FIND-MESSAGE-FRAGMENTS.

Either way, the message fragments contain all the information you
might expect in a header of a message: the message ID, the From
adress, the date the message was created and the subject. In order for
the next code to expand this into a complete message, some more data
is needed. The only subclass I'm using at the moment is
HTTP-MESSAGE-FRAGMENT, which has a URL slot, which tells the code from
where to download the rest of the information for the message.

Writing a new NEWS-SOURCE, you probably only need to implement
FIND-MESSAGE-FRAGMENTS so far. Test that with calling NEW-HEADERS on
the news source.

Once the message fragments have been filtered, we expand them into
complete (MIME!) messages. The top-level code is
MAKE-MESSAGE-FROM-FRAGMENT. This calls EXPAND-MESSAGE-FRAGMENT, which
works by getting the contents for the fragment with
CONTENTS-FOR-MESSAGE-FRAGMENT (for HTTP-MESSAGE-FRAGMENTs, this works
by downloading the data at URL). Then these contents are filtered by
FILTER-SOURCE-CONTENTS, which extracts the relevant data for the post
and returns the MIME type of the data that was extracted (often
"text/html").

You almost certainly need to implement FILTER-SOURCE-CONTENTS.

After EXPAND-MESSAGE-FRAGMENT has run, if the mime type is text/html
then code in MAKE-MESSAGE-FROM-FRAGMENT makes a multipart message by
downloading all linked data (images and stylesheets) and fixes up the
links to cid:xxxx form.

To test this works correctly, call NEW-HEADERS and then call
MAKE-MESSAGE-FROM-FRAGMENT on the header fragments. If you want to
check your FILTER-SOURCE-CONTENTS code and aren't worried about the
MIME code (yet), you can just use EXPAND-MESSAGE-FRAGMENT, which is
simpler to call.
