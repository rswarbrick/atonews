(in-package :atonews)

(defclass tiered-source (http-source)
  ()
  (:documentation
   "A tiered source is like an HTTP-SOURCE but can be used for things like
journals, where the toplevel list of items actually points to pages listing
articles.

Implement the NEXT-ISSUE method and create message-fragments like with
NEXT-MESSAGE-FRAGMENT. However, the only fields that will have any effect are
the URL and the message ID. So that we don't have to download each issue's page
on every update, we make the assumption that the message ID returned by
NEXT-ISSUE will be a valid ID for one of the articles downloaded inside the
issue. This lets us query the news server to see whether we've grabbed this one
yet.

You should also implement the NEXT-MESSAGE-FRAGMENT method, but this is called
with the page that you give for the issue."))

(defgeneric next-issue (source html pos other)
  (:documentation
   "A version of NEXT-MESSAGE-FRAGMENT relevant to TIERED-SOURCE instances. See
the documentation for these two."))

(defmethod list-headers ((source tiered-source) (server nntp-server))
  (mapcan (lambda (issue)
            (find-message-fragments source (http-get (url issue))))
          (filter-headers (generic-find-message-fragments
                           source (get-header-data source) #'next-issue)
                          server)))
