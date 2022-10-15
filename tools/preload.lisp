(let* ((clpmfile (merge-pathnames (uiop/os:getcwd) "clpmfile"))
       (lockfile (merge-pathnames clpmfile "clpmfile.lock"))
       (name "cl-i")
       (asds (list (concatenate 'string name ".asd"))))
  (when (not (probe-file clpmfile))
    (clpm-client:bundle-init clpmfile :asds
                             asds))
  (when (not (probe-file lockfile))
    (clpm-client:install
      :context clpmfile))
  (when (not (clpm-client:active-context))
    (clpm-client:activate-context clpmfile :activate-asdf-integration t))
(asdf:load-system name)
(asdf:load-system (format nil "~A/tests" name)))