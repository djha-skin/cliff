(clpm-client:activate-context (merge-pathnames (uiop/os:getcwd) "clpmfile") :activate-asdf-integration t)
(asdf:load-system "cl-i")