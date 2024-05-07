#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.cliff (:use #:cl)
  (:documentation
    "
    package that has a function, `execute-program`, which does the following:
    - registers functions mapped to specific subcommands
    - reads configuration files in standard locations
    - reads environment variables
    according to specific rules and from these rules constructs a hash table
    which is then passed to the subcommands.
    ")
    (:import-from #:com.djhaskin.nrdl)
    (:import-from #:dexador)
    (:import-from #:uiop/pathname)
    (:import-from #:quri)
    (:import-from #:cl-reexport)
    (:import-from #:uiop/stream)
    (:import-from #:com.djhaskin.cliff/errors)
    (:local-nicknames
      (#:cliff/errors #:com.djhaskin.cliff/errors)
      (#:cliff #:com.djhaskin.cliff)
      (#:nrdl #:com.djhaskin.nrdl))
    (:export
      generate-string
      parse-string
      slurp-stream
      data-slurp
      invalid-subcommand
      make-windows-path
      make-unix-path
      make-os-specific-path
      necessary-env-var-absent
      os-specific-home
      os-specific-config-dir
      find-file
      consume-arguments
      consume-environment
      config-file-options
      system-environment-variables
      default-help
      execute-program
      ensure-option-exists))

(in-package #:com.djhaskin.cliff)
(cl-reexport:reexport-from '#:com.djhaskin.cliff/errors)

(defun repeatedly-eq
  (func
    arg
    &optional
    (eeq #'equal)
    (repeats 256))
  "
  list consisting of calls to func using arg, then calling func using that
  result, etc.
  stops when subsequent calls return equal.
  "
  (declare (type integer repeats))
  (when
    (<= repeats 0)
    (error "ran repeats times without terminating. arg: ~a" arg))
  (let
    ((next (funcall func arg)))
    (if (funcall eeq next arg)
      (list arg)
      (cons
        arg
        (repeatedly-eq
          func next eeq (- repeats 1))))))

(defun repeatedly
    (func
      arg
      &optional (check-p (lambda (arg) (equal arg nil)))
      (repeats 256))
  "
  list consisting of calls to func on arg, then calling func on that result,
  etc.
  doesn't stop until check-p returns t when given `arg`.
  "
  (declare (type integer repeats))
  (labels
      ((helper
           (arg
             on)
         (when
             (<= on 0)
           (error
             (concatenate
               'string
               "ran ~a times without terminating.~%"
               "  final iteration arg: ~a~%")
             repeats
             arg))
         (if  (funcall check-p arg)
             nil
             (cons
               arg (helper
                     (funcall func arg)
                     (- on 1))))))
    (helper arg repeats)))

(defun url-to-pathname
  (url)
  (declare (type string url))
  (multiple-value-bind
      (match groups)
      (cl-ppcre:scan-to-strings "^file://(.+)$" url)
    (if match
        (pathname (elt groups 0))
        url)))

(defun slurp-stream (f)
  (with-output-to-string
    (out)
    (loop
      do
      (let
        ((char-in
           (read-char
             f nil)))
        (if
          char-in
          (write-char
            char-in out)
          (return))))))

(defun
  base-slurp
  (loc)
  (let
    ((input
       (if
         (equal
           loc "-")
         *standard-input*
         loc)))
    (if
      (typep
        input 'stream)
      (slurp-stream
        input)
      (with-open-file
        (in-stream
          loc
          :direction :input
          :external-format :utf-8)
        (slurp-stream
          in-stream)))))

;;  takes alist
;; handles http.
;  :content-type 'application/json
(defun http-expanded-url (dexfun resource &rest more-args)
  (flet
    ((http-call
       (&rest
         args)
       (apply
         dexfun
         (concatenate
           'list
           args
           more-args
           '(:force-string
              t)))))
    (or
      (cl-ppcre:register-groups-bind
        (protocol
          username password rest-of-it)
        ("^(https?://)([^@:]+):([^@:]+)@(.+)$"
         resource)
        (http-call
          (concatenate
            'string
            protocol
            rest-of-it)
          :basic-auth
          (cons
            (quri:url-decode
              username)
            (quri:url-decode
              password))))
      (cl-ppcre:register-groups-bind
        (protocol
          header headerval rest-of-it)
        ("^(https?://)([^@=]+)=([^@=]+)@(.+)$"
         resource)
        (http-call
          (concatenate
            'string
            protocol
            rest-of-it)
          :headers
          (list
            (cons
              (quri:url-decode
                header)
              (quri:url-decode
                headerval)))))
      (cl-ppcre:register-groups-bind
        (protocol
          token rest-of-it)
        ("^(https?://)([^@]+)@(.+)"
         resource)
        (http-call
          (concatenate
            'string
            protocol
            rest-of-it)
          :headers
          (list
            (cons
              "Authorization"
              (format
                nil "Bearer ~a" (quri:url-decode
                                  token))))))
      (cl-ppcre:register-groups-bind
        ()
        ("^https?://.*$"
         resource)
        (http-call
          resource))
      nil)))

(defun data-slurp (resource &rest more-args)
  "
  Slurp config, using specified options.

  if `:resource` is a url, download the contents according to the following
  rules:
  - if it is of the form `http(s)://user:password@url`,
  it uses basic auth;
  - if it is of the form `http(s)://header=val@url`,
  a header is set;
  - if it is of the form `http(s)://<token>@url`,
  a bearer token is used;
  - if it is of the form `file://<location>`, it is loaded as a normal file;
  - if it is of the form `-`, the nrdl is loaded from standard input;
  - otherwise, the nrdl is loaded from the string or pathname as if it named
  a file.
  "
  (declare (type (or pathname string) resource))
  (or
    (when (equal
            'pathname (type-of
                        resource))
      (base-slurp
        resource))
    (apply
      #'http-expanded-url
      (concatenate
        'list
        (list #'dexador:get resource)
        more-args))
    (base-slurp (url-to-pathname resource))))

;; TODO: TEST
(defun extract-path (path-part-str pathsep)
  ; https://unix.stackexchange.com/a/596656/9696
  ; The function `cl-ppcre:split` Needs a limit for some reason
  ; in order to work with trailing slashes
  (let* ((path-parts-of (cl-ppcre:split pathsep path-part-str :limit 4097))
         (partition-point (- (length path-parts-of) 1))
         (path-parts (subseq path-parts-of 0 partition-point))
         (fdesignator-lst (subseq path-parts-of partition-point)))
    (let ((fdesignator (car fdesignator-lst)))
      (multiple-value-bind (_ groups)
        (cl-ppcre:scan-to-strings "(.+)\\.([^.]+)$" fdesignator)
        (declare (ignore _))
        (let ((f-name (if (null groups)
                        (if (not (string= "" fdesignator))
                          fdesignator
                          nil)
                        (aref groups 0)))
              (f-type (when (not (null groups))
                        (aref groups 1))))
          (apply
            #'concatenate
            (list 'list
                  (list :directory
                        (if (string= "" (car path-parts))
                            (cons :absolute
                                  (cdr path-parts))
                            (cons :relative path-parts)))
                  (when (not (null f-name))
                    (list :name f-name))
                  (when (not (null f-type))
                    (list :type f-type)))))))))
;; TODO: TEST
(defun make-unix-path
  (pstr)
  (apply #'make-pathname
         (extract-path pstr "/+")))

; (setf pp (make-windows-path "C:\\a\\b\\"))
; (setf pp (make-windows-path "C:\\a\\b\\c.d"))
; (setf pp (make-windows-path "a\\b\\c.d"))
; (setf pp (make-windows-path "a\\c.d"))
; (setf pp (make-windows-path "a\\"))
; (setf pp (make-windows-path "a"))
; (setf pp (make-windows-path ".c")) -> Should not have a type set, only name
; (setf pp (make-windows-path ""))
;; TODO: TEST
(defun make-windows-path (pathstr)
  (let* ((device-parts (cl-ppcre:split ":" pathstr))
         (device-name (when (> (length device-parts) 1) (car device-parts)))
         (path-part-str (if (> (length device-parts) 1)

                          (cadr device-parts)
                          (car device-parts))))
    (apply #'make-pathname
           (apply #'concatenate
                  (list 'list
                        (when (not (null device-name))
                          (list :device device-name))
                        (extract-path path-part-str "\\\\+"))))))

(setf (fdefinition 'display-config)
      (function identity))

;; TODO: TEST
(setf (fdefinition 'make-os-specific-path)
      (function
        #+windows make-windows-path
        #-windows make-unix-path))

(defparameter *dirsep*
  #+windows "\\"
  #-windows "/")

(define-condition necessary-env-var-absent (error)

  ((env-var :initarg :env-var
            :initform (error "Need to give argument `:env-var`.")
            :reader env-var))
  (:documentation "An required environment variable is not present.")
  (:report (lambda (condition stream)
             (format stream "Environment variable `~A` not specified.~&"
                     (env-var condition)))))

(defmethod cliff/errors:exit-status ((this necessary-env-var-absent))
  :configuration-error)

(defmethod cliff/errors:exit-map-members ((this necessary-env-var-absent))
  `((:env-var . ,(env-var this))))

(defun os-specific-home (getenv)
  (make-os-specific-path
    #+windows (concatenate
                'string
                (funcall
                  getenv
                  "USERPROFILE")
                "\\")
    #-windows (concatenate
                'string
                (funcall getenv "HOME")
                "/")))

#+(or)
(os-specific-config-dir "halo" (lambda (x &optional y)
                                 (or (uiop/os:getenv x) y)))
(defun os-specific-config-dir (program-name getenv)
  "
  Deteremines the OS-specific configuration folder base.
  "
  (make-os-specific-path
    #+windows (concatenate
                'string
                (funcall getenv "LOCALAPPDATA"
                         (concatenate
                           'string
                           (funcall
                             getenv
                             "USERPROFILE")
                           "\\AppData\\Local"))
                "\\"
                program-name
                "\\")
    #+darwin (concatenate
               'string
               (funcall getenv "HOME")
               "/Library/Preferences/"
               program-name
               "/")
    #-(or darwin windows)
    (concatenate
      'string
      (funcall getenv "XDG_CONFIG_HOME"
               (concatenate 'string
                            (funcall getenv "HOME")
                            "/.config"))
      "/"
      program-name
      "/")))


; get file
; (find-file (uiop/os:getcwd) (pathname ".git/"))
; (find-file (uiop/os:getcwd) (pathname ".config/"))
; (find-file (uiop/os:getcwd) (pathname ".gitconfig"))
(defun
  find-file
  (from marker)
  "
  Starting at the directory given,
  find the marking file.
  "
  (if (null marker)
    nil
    (let* ((* from)
           (* (repeatedly-eq
        #'uiop/pathname:pathname-parent-directory-pathname *))
           (* (mapcar
                (lambda
                  (path)
                  (merge-pathnames
                    marker
                    path)) *))
           (* (some
        (lambda (f)
          (or (uiop/filesystem:file-exists-p f)
              (uiop/filesystem:directory-exists-p f))) *)))
      *)))

(defun
  generate-string
  (thing &optional &key (pretty 0))
  (with-output-to-string (strm)
    (nrdl:generate-to strm thing :pretty-indent pretty)))

(defun parse-string (thing)
  (with-input-from-string (strm thing)
    (nrdl:parse-from strm)))

(defparameter
  +find-tag+
  (cl-ppcre:create-scanner
    "^--([^-]+)-(.+)$"))

(defparameter
  +multiple-arg-actions+
  '(
    :set
    :add
    :join
    :nrdl
    :file
    :raw
    )
  "actions that consume additional argument."
  )

(defparameter
  +single-arg-actions+
  '(
    :enable
    :disable
    :reset
    )
  "actions that consume no additional arguments."
  )

(define-condition unknown-directive (error)
  ((context :initarg :context
            :initform (error "Please specify context.")
            :reader context)
   (directive :initarg :directive
              :initform (error "Please specify directive.")
              :reader directive)
   (option :initarg :option
           :initform (error "Please specify an option.")
           :reader option))
  (:documentation
   "The directive given was not valid.")
  (:report (lambda (this strm)
             (format strm
                     "Directive `~A` for option `~A` invalid in ~A.~&"
                     (directive this)
                     (option this)
                     (context this)))))

(defmethod cliff/errors:exit-status ((this unknown-directive))
  :cl-usage-error)

(defmethod cliff/errors:exit-map-members ((this unknown-directive))
    `((:context . ,(context this))
      (:directive . ,(directive this))
      (:option . ,(option this))))


(defun ingest-option
  (opts
    map-sep-pat hash-init-args kact kopt &optional value)
  (cond
    ((eql kact :enable)
     (setf (gethash kopt opts) t))
    ((eql kact :disable)
     (setf (gethash kopt opts) nil))
    ((eql kact :reset)
     (remhash kopt opts))
    ((eql kact :set)
     (setf (gethash kopt opts) value))
    ((eql kact :add)
     (setf (gethash kopt opts)
           (cons
             value (gethash
                     kopt opts))))
    ((eql kact :join)
     (let ((sresults
             (cl-ppcre:split
               map-sep-pat value)))
       (if
         (eql (length sresults) 2)
         (destructuring-bind
           (k v)
           (coerce sresults 'list)
           (when (not (gethash kopt opts))
             (setf (gethash kopt opts)
                   (apply #'make-hash-table hash-init-args)))
           (setf (gethash (nrdl:string-symbol k) (gethash kopt opts)) v))
         (error "not a k/v pair, check map sep pattern: ~a" value))))
    ((eql kact :nrdl)
     (setf (gethash kopt opts)
           (parse-string value)))
    ((eql kact :file)
     (setf (gethash kopt opts)
           (parse-string (data-slurp value))))
    ((eql kact :raw)
     (setf (gethash kopt opts)
           (data-slurp value)))
    (:else
      (error "uknown action ~a" kact))))

(defun
  consume-arguments
  (args &optional &key initial-hash hash-init-args (map-sep "="))
  "
  Consume arguments, presumably collected from the command line. Assumes any
  aliases have already been expanded (that is, it uses the arguments as-is,
  with no transformation).

  Assumes an open-world. For each argument, it examines its form.

  If the argument is of the form `--(enable|disable)-(?P<argument>[^ ]*)`,
  the keyword named `argument` is associated with `t` or `nil` in the resulting
  hash-table, respectively.

  If the argument is of the form `--(?P<act>set|add|join|nrdl|file|raw)-(?P<argument>[^
  ]*)`, the next argument is consumed as the value.

  If the first part (`act`) is `set`, associate the value as a string to the
  keyword `argument` in the resulting hash table.

  If the `act` is `add`, cons the value onto a list and ensure that list
  is associated with the `argument` keyword in the resulting list.

  If the `act` is `join`, split the value using an equals sign (or something
  else as specified by `map-sep-pat`) into two parts, a key and a val.
  Associate the key with the val in a hash-table, and ensure that hash table is
  the value associated with the keyword `argument` in the resulting options
  hash-table.

  If the `act` is `nrdl`, parse the value string as if it were a nrdl string.
  Set the value as found to the `argument` keyword in the resulting options
  hash-table.

  If the `act` is `file`, read the file found at the given location and parse
  it as a nrdl document. Set the option to the contents of that nrdl document.

  If the `act` is `raw`, read the file found at the given location and
  use the raw bytes as the value of the option.
  "
  (let ((map-sep-pat (cl-ppcre:create-scanner map-sep))
        (consumable (copy-list args))
        (other-args nil)
        (opts (if (null initial-hash)
                (apply #'make-hash-table hash-init-args)
                initial-hash))
        (context "CLI arguments"))
    (loop while consumable do
          (let
            ((arg (first consumable))
             (rargs (rest consumable)))
            (or
              (cl-ppcre:register-groups-bind
                ((#'nrdl:string-symbol
                  kact)
                 (#'nrdl:string-symbol
                  kopt))
                (+find-tag+
                  arg)
                (cond
                  ((some (lambda (x) (eql kact x)) +single-arg-actions+)
                   (ingest-option
                     opts map-sep-pat hash-init-args kact kopt)
                   (setf consumable rargs))
                  ((some (lambda (x) (eql kact x)) +multiple-arg-actions+)
                   (when (not rargs)
                     (error "not enough arguments for action ~a on option ~a"
                            kact kopt))
                   (let ((value
                           (first rargs))
                         (rrargs (rest rargs)))
                     (ingest-option
                       opts map-sep-pat hash-init-args kact kopt value)
                     (setf consumable rrargs)))
                  (:else
                   (error 'unknown-directive
                          :context context
                          :directive kact
                          :option kopt)))
                t)
              (progn
                (push
                  arg other-args)
                (setf
                  consumable rargs)))))
    (values
      opts (reverse other-args))))


(defun
  ingest-var
  (opts
    map-sep-pat list-sep-pat hash-init-args ktag kopt context value)
  (cond
    ((eql ktag :flag)
     (if (some
           (lambda (v)
             (eql value v))
           '("0" "false" "False" "FALSE" "no" "NO" "No"))
       (ingest-option opts map-sep-pat hash-init-args :disable kopt)
       (ingest-option opts map-sep-pat hash-init-args :enable kopt)))
    ((eql ktag :item)
     (ingest-option opts map-sep-pat hash-init-args :set kopt value))
    ((eql ktag :list)
     (loop for piece in (reverse (cl-ppcre:split list-sep-pat value)) do
           (ingest-option
             opts
             map-sep-pat
             hash-init-args
             :add
             kopt
             piece)))
    ((eql ktag :table)
     (loop for piece in (cl-ppcre:split list-sep-pat value) do
           (ingest-option opts
                          map-sep-pat
                          hash-init-args
                          :join
                          kopt
                          piece)))
    ((eql ktag :nrdl)
     (ingest-option opts
                    map-sep-pat
                    hash-init-args
                    ktag
                    kopt
                    value))
    ((eql ktag :file)
     (ingest-option opts
                    map-sep-pat
                    hash-init-args
                    :file
                    kopt
                    value))
    ((eql ktag :raw)
     (ingest-option opts
                    map-sep-pat
                    hash-init-args
                    :raw
                    kopt
                    value))
    (t
     (error 'unknown-directive
            :context context
            :option kopt
            :directive ktag))))

(defun expand-cli-aliases (aliases args)
  "
  Expand argument aliases.
  "
  (declare (type hash-table aliases)
           (type list args))
  (mapcar
    (lambda (arg)
      (or
        (gethash
          arg aliases)
        arg))
    args))

;; TODO: TEST THIS
(defun expand-env-aliases (aliases env &key hash-init-args)
  "
  Replace the names of environment variables according to the map
  found in `aliases`. The keys of the `aliases` hash table are matched
  as strings to the name of the environment variable names (keys in the env var
                                                                 map). If they match, the key is removed from the hash table and a key with
  the value of the associated entry in `aliases` is used as the new key,
  associating it with the value of the old removed key.
  "
  (declare (type hash-table aliases env))
  (let ((result (apply #'make-hash-table hash-init-args)))
    (loop
      for key being the hash-key of env
      using (hash-value value)
      do
      (setf (gethash
              (or (gethash key aliases)
                  key) result)
            value))
    result))


;(setf cincuenta (consume-environment "hello-world" (alexandria:alist-hash-table '(("HELLO_WORLD_LIST_IRON_MAN" . "1,2,3,4,5") ("HELLO_WORLD_NRDL_DISEASE" . "{'he': 'could', 'do': false, 'it': 'rightnow'}")))))

; =>

; (list (:DISEASE . (alexandria:hash-table-alist '(("it" . "rightnow") ("do") ("he" . "could")) ))
; (:IRON_MAN "5" "4" "3" "2" "1"))

(defun
  consume-environment
  (program-name
    env
    &optional
    &key
    hash-init-args
    (list-sep ",")
    (map-sep "="))
  "
  Consume variables, presumably collected from the OS.
  Assumes any aliases have already been expanded (that is,
                                                       it uses the variables as-is, with no transformation).

  Assumes an open-world. For each environment variable, it examines its form.

  If the variable is of the form
  `^(<PROGRAM_NAME>)_(?P<opt>LIST|TABLE|ITEM|FLAG|NRDL)_(?P<arg>.*)$`, then the
  variable will be used to add to the resulting options hash table.

  If the `opt` is `LIST`, the value of the variable will be split using
  `list-sep` and the resulting list of strings will be associated with the
  keyword `arg` in the options.

  If the `opt` is `TABLE`, the value of the variable will be split using
  `list-sep`, then each entry in that list will also be split using `map-sep`.
  The resulting key/value pair list is turned into a hash-table and this hash
  table is associated to the keyword `arg` in the options.

  If the `opt` is `ITEM`, the value of the variable will be set to the keyword
  `arg` in the options.

  If the `opt` is `NRDL`, the value of the variable will be parsed as a NRDL
  string and its resultant value set as the value of the keyword `arg` in the
  returned options hash table.
  "
  (declare (type hash-table env)
           (type string program-name)
           (type list hash-init-args)
           (type string list-sep)
           (type string map-sep))
  (let*
    ((result
       (apply
         #'make-hash-table hash-init-args))
     (clean-name
       (string-upcase
         (cl-ppcre:regex-replace-all
           "\\W" program-name "_")))
     (var-pattern
       (cl-ppcre:create-scanner
         `(:sequence
            :start-anchor
            ,clean-name
            "_"
            (:register
              (:alternation
                "LIST"
                "TABLE"
                "ITEM"
                "FLAG"
                "NRDL"))
            "_"
            (:register
              (:greedy-repetition
                1
                nil
                :word-char-class))
            :end-anchor)))
     (map-sep-pat
       (cl-ppcre:create-scanner
         `(:sequence
            ,map-sep)))
     (list-sep-pat
       (cl-ppcre:create-scanner
         `(:sequence
            ,list-sep))))
    (loop
      for key being the hash-key of env
      using (hash-value
              value)
      do
      (cl-ppcre:register-groups-bind
        ((#'nrdl:string-symbol ktag)
         (#'nrdl:string-symbol kopt))
        (var-pattern key)
        (ingest-var
          result
          map-sep-pat
          list-sep-pat
          hash-init-args
          ktag
          kopt
          "environment"
          value)))
    result))

(defun update-hash
  (to from)
  (loop for key being the hash-key of from
        using (hash-value value)
        do
        (setf (gethash key to) value)))

(defun config-file-options
  (program-name
    environment
    defaults
    &optional
    reference-file
    root-path)
  (declare (type string program-name)
           (type hash-table environment)
           (type hash-table defaults)
           (type (or null pathname) reference-file)
           (type (or null pathname) root-path))
  (let* ((result (alexandria:copy-hash-table defaults))
         (effective-root (if (null root-path)
                           (uiop/os:getcwd)
                           root-path))
         (home-config-path (os-specific-config-dir
                             program-name
                             (lambda (var &optional default)
                               (let ((result (gethash var environment default)))
                                 (if (null result)
                                   (if (null default)
                                     (error 'necessary-env-var-absent
                                            :env-var var)
                                     default)
                                   result)))))
         (home-config-path-file
           (merge-pathnames
             (make-pathname
               :name
               "config"
               :type
               "nrdl")
             home-config-path))
         (marked-config-file-name
           (make-pathname
             :name
             (concatenate
               'string
               "."
               program-name)
             :type
             "nrdl"))
         (marked-config-path
           (or
             (when reference-file
               (let ((marked-file (find-file
                                    effective-root
                                    reference-file)))
                 (when marked-file
                   (merge-pathnames
                     marked-config-file-name
                     (uiop/pathname:pathname-parent-directory-pathname
                       marked-file)))))
             (find-file
               effective-root
               marked-config-file-name))))
    (when (uiop/filesystem:file-exists-p home-config-path)
      (update-hash result (parse-string (data-slurp home-config-path-file))))
    (when (uiop/filesystem:file-exists-p marked-config-path)
      (update-hash result (parse-string (data-slurp marked-config-path))))
  result))

(define-condition invalid-subcommand (error)
  ((given-subcommand :initarg :given-subcommand
                     :initform nil
                     :reader given-subcommand))
  (:documentation
    "The subcommand given was invalid; no functions were found for it.")
  (:report (lambda (this strm)
             (format strm
                     "The subcommand `~{~A~^ ~}` has no actions defined for it."
                     (given-subcommand this)))))

(defmethod cliff/errors:exit-status ((this invalid-subcommand))
  :cl-usage-error)

(defmethod cliff/errors:exit-map-members ((this invalid-subcommand))
  `((:given-subcommand . ,(given-subcommand this))))

(defun system-environment-variables ()
  "
  Get the system environment variables.
  "
  (let* ((output (uiop:run-program
                   #+windows
                   "cmd.exe /C set"
                   #-windows
                   "env" :output :string))
         (stripped-output (string-trim " " output))
         (kv-pairs (uiop:split-string stripped-output :separator '(#\Newline)))
         (raw-alist
           (mapcar
             (lambda (s)
               (let ((split-at (position #\= s)))
                 (if split-at
                   (cons
                     (subseq s 0 split-at)
                     (subseq s (1+ split-at)))
                   (cons s nil))))
             kv-pairs))
         (alist (remove-if (lambda (pair) (or
                                            (null (car pair))
                                            (equal (car pair) "")))
                           raw-alist)))
         (alexandria:alist-hash-table alist :test #'equal)))

; (alexandria:hash-table-alist
;  (system-environment-variables))

#+(or)
(alexandria:hash-table-alist
  (default-help
  t
  "halo"
  (alexandria:alist-hash-table
    '((:a . 1)
      (:b . 2)
      (:c . 3)))
  '("hello" "world")
  nil
  (uiop/os:getcwd)
  '(
    (("hello" "world") . "
This is nonsense.
"))
  ","
  "="))

(defun default-help
    (strm
      program-name
      options
      other-args
      reference-file
      root-path
      helps
      functions
      list-sep
      map-sep
      )
  (declare (type (or stream boolean) strm)
           (type string program-name)
           (type hash-table options)
           (type list other-args)
           (type (or null pathname) reference-file)
           (type (or null pathname) root-path)
           (type list helps)
           (type string list-sep)
           (type string list-sep))
  (format
    strm "~@{~@?~}"
    "~&Welcome to `~A`!~%" program-name
    "~%"
    "This is a CL-I program.~%"
    "~%"
    "It uses NRDL to communicate with the calling program. Its config files~%"
    "and, optionally, environment variables can be set using NRDL, and its~%"
    "output will be a NRDL document. More information about~%"
    "NRDL can be found here:~%"
    "~%"
    "https://git.sr.ht/~~skin/nrdl~%"
    "~%"
    "Options can be given via:~%"
    "  - Config file, as in `{ <option> <value> }`~%"
    "  - Environment variable, as in `~A_<KIND>_<OPTION>`~%" (string-upcase
                                                               (cl-ppcre:regex-replace-all
                                                                 "\\W" program-name "_"))
    "  - Command line, as in `--<action>-<option>`~%"
    "~%"
    "Config files are consulted first, then environment variables, then~%"
    "the command line, with later values overriding earlier ones.~%~%")
  ; Config files section
  (format strm "Config files can be in the following locations:~%")
  (if reference-file
      (format strm "~@{~@?~}"
              "  - A config file named `.~A.nrdl` found in the same directory~%"
              program-name
              "    as `~A`, which is searched for in `~A`~%"
              (namestring reference-file)
              (namestring root-path)
              "    (or any of its parents)~%")
      (format strm "~@{~@?~}"
              "  - A config file named `.~A.nrdl` in the current directory~%"
              program-name
              "    (or any of its parents)~%")
    )
  (format
    strm "~@{~@?~}"
    "  - A home-directory config file in an OS-specific location:~%"
    "      - On Linux: `${XDG_CONFIG_HOME}/~A/config.nrdl`~%" program-name
    "        (by default ~~/.config/~A/config.nrdl)` ~%" program-name
    "      - On Mac:   `~~/Library/Preferences/~A/.config.nrdl`~%" program-name
    "      - On Windows: %LOCALAPPDATA%\\~A\\config.nrdl`~%" program-name
    "        (by default `%USERPROFILE%\\AppData\\Local\\~A\\config.nrdl)`~%~%"
    program-name)
  ; Environment variables section
  (let ((clean-program-name
          (string-upcase
            (cl-ppcre:regex-replace-all
              "\\W" program-name "_"))))
    (format
      strm "~@{~@?~}"
      "Options can be set via environment variable as follows:~%"
      "  - `~A_FLAG_<OPTION>=1` to enable a boolean flag~%"
      clean-program-name
          "  - `~A_ITEM_<OPTION>=<VALUE>` to set a string value~%"
          clean-program-name
          "  - `~A_LIST_<OPTION>=<VAL1>~A<VAL2>~A...` to set a list option~%"
          clean-program-name list-sep list-sep
          "  - `~A_TABLE_<OPTION>=<KEY1>~A<VAL1>~A<KEY2>~A<VAL2>~A...` to set~%"
          clean-program-name list-sep map-sep list-sep map-sep
          "     a key/value table option~%"
          "  - `~A_NRDL_<OPTION>=<NRDL_STRING>` to set a value using~%"
          clean-program-name
          "     a NRDL string~%"
          "  - `~A_FILE_<OPTION>=<FILE_PATH>` to set a value using the contents~%"
          "     of a NRDL document from a file as if by the `--file-*` flag~%"
          "  - `~A_RAW_<OPTION>=<FILE_PATH>` to set a value using the raw bytes~%"
          "    of a file as if by the `--raw-*` flag~%"
          "~%"))
  (format
    strm
    "~@{~@?~}"
    "Options can be changed or set on the command line in the following ways:~%"
    "  - To enable a flag, use `--enable-<option>`.~%"
    "  - To disable a flag, use `--disable-<option>`.~%"
    "  - To reset any value, use `--reset-<option>`.~%"
    "  - To add to a list, use `--add-<option> <value>`.~%"
    "  - To set a string value, use `--set-<option> <value>`.~%"
    "  - To set using a NRDL string, use `--nrdl-<option> <value>`.~%"
    "  - To set using NRDL contents from a nrdl document from file,~%"
    "    use `--file-<option> <url>`.~%"
    "  - To set using the raw bytes of a file, use `--raw-<option> <url>`.~%"
    "~%"
    "  - For `--file-<option>` and `--raw-<option>`, URLs are also supported:~%"
    "      - `http(s)://user:password@url` for basic auth~%"
    "      - `http(s)://header=val@url` for a header~%"
    "      - `http(s)://token@url` for a bearer token~%"
    "      - `file://location` for a local file~%"
    "      - `-` for standard input~%"
    "    Anything else is treated as a file name.~%"
    "~%")
  (format strm "The following options have been detected:~%")
  (nrdl:generate-to strm options :pretty-indent 4)
  (finish-output strm)
  (let* ((entry (assoc other-args helps :test #'equal))
         (helpstring (cdr entry))
         (callable-function (assoc other-args functions :test #'equal)))


    (format strm "~%~%Available subcommands:~%")

    (loop for (key . _) in functions
          if (not (null key))
          do
          (format strm "  - `~{~A~^ ~}`~%" key))

    (when (assoc nil functions)
      (format strm "~%The bare command `~A` (with no subcommands) performs an ~
              action as well.~%" program-name))

    (if (not (null helpstring))
        (format strm "~%Documentation~@[ for subcommand `~{~A~^ ~}`~]: ~%~%~A~%~%"
                other-args
                helpstring)
        (format strm "~%Documentation~@[ for subcommand `~{~A~^ ~}`~] not found.~%~%"
                other-args))
    (unless callable-function
        (format strm "~v[No action exists for the command.~:;The subcommand `~{~A~^ ~}` does not exist.~]~%~%"
                (length other-args)
                other-args)))
  (alexandria:alist-hash-table
    '(
      (:status . :successful)
      )))

#+(or)
(multiple-value-bind (exit-code exit-map)
  (execute-program
    "hi"
    (alexandria:alist-hash-table
      '(("HOME" . "/home/skin")
        ("HI_ITEM_FOO" . "25")
        ("HI_LIST_BAR" . "1,2,3,4,5")
        ("HI_TABLE_BAZ" . "a=1,b=2,c=3")
        ("HI_FLAG_QUUX" . "1")
        ("HI_NRDL_DISEASE" . "{he \"could\" do false it \"rightnow\"}")))
    (list
      (cons '("foo") (lambda (opts) (format t "foo: ~a~%" (gethash :foo opts)))))
    :helps
    (list
      (cons '("foo") "This is the foo subcommand."))
    :cli-arguments '("foo"))
  (alexandria:hash-table-alist exit-map))

#+(or)
(multiple-value-bind (exit-code exit-map)
  (execute-program
    "hi"
    (alexandria:alist-hash-table
      '(("HOME" . "/home/skin")
        ("HI_ITEM_FOO" . "25")
        ("HI_LIST_BAR" . "1,2,3,4,5")
        ("HI_TABLE_BAZ" . "a=1,b=2,c=3")
        ("HI_FLAG_QUUX" . "1")
        ("HI_NRDL_DISEASE" . "{he \"could\" do false it \"rightnow\"}")))
    (list
      (cons '("foo") (lambda (opts)
                       (format t "foo: ~a~%" (gethash :foo opts))
                       (alexandria:alist-hash-table
                         '((:status . :successful)
                           (:foo . (gethash :foo opts))))
                       )))
    :helps
    (list
      (cons '("foo") "This is the foo subcommand."))
    :cli-arguments '("foo"))
  (alexandria:hash-table-alist exit-map))

(defun
  execute-program
  (program-name
    environment-variables
    functions
    &key
    helps
    (strm *standard-output*)
    (err-strm *error-output*)
    (suppress-final-output nil)
    cli-arguments
    cli-aliases
    defaults
    (setup #'identity)
    (teardown #'identity)
    root-path
    reference-file
    environment-aliases
    (list-sep ",")
    (map-sep "=")
    (enable-help t)
    (str-hash-init-args
      `(:test ,#'equal))
    kw-hash-init-args)
  (declare (type string program-name)
           (type hash-table environment-variables)
           (type list functions)
           (type list cli-arguments)
           (type list cli-aliases)
           (type list environment-aliases)
           (type list str-hash-init-args)
           (type list kw-hash-init-args)
           (type (or null pathname) root-path)
           (type (or null pathname) reference-file)
           (type list defaults)
           (type string list-sep)
           (type string map-sep)
           (type function setup)
           (type function teardown))
  (handler-case
      (let ((effective-defaults (if (null defaults)
                                    (apply #'make-hash-table kw-hash-init-args)
                                    (apply #'alexandria:alist-hash-table
                                           defaults
                                           kw-hash-init-args)))
            (effective-environment
              (expand-env-aliases
                (apply #'alexandria:alist-hash-table
                       environment-aliases
                       str-hash-init-args)
                environment-variables
                :hash-init-args str-hash-init-args))
            (effective-cli
              (expand-cli-aliases
                (apply #'alexandria:alist-hash-table
                       cli-aliases
                       str-hash-init-args)
                cli-arguments)))
        (multiple-value-bind (result found-config-files)
            (config-file-options
              program-name
              effective-environment
              effective-defaults
              reference-file
              root-path)
          (declare (ignore found-config-files))
          (update-hash
            result
          (consume-environment
            program-name
            effective-environment
            :hash-init-args kw-hash-init-args
            :list-sep list-sep
            :map-sep map-sep))
        (multiple-value-bind
            (opts-from-args other-args)
            (consume-arguments
              effective-cli
              :initial-hash result
              :hash-init-args kw-hash-init-args
              :map-sep map-sep)
              (let* ((setup-result (funcall setup opts-from-args))
                     (subcommand-function
                       (or
                         (and enable-help
                              (equal (first other-args) "help")
                              (lambda (opts)
                                (default-help
                                  err-strm
                                  program-name
                                  opts
                                  (cdr other-args)
                                  reference-file
                                  root-path
                                  helps
                                  functions
                                  list-sep
                                  map-sep)))
                         (cdr (assoc other-args functions :test #'equal))
                         (error 'invalid-subcommand
                                :given-subcommand other-args)))
                     (intermediate-result
                       (funcall subcommand-function setup-result))
                     (final-result
                       (funcall
                         teardown intermediate-result))
                     (status (gethash :status final-result :successful)))
                        (unless suppress-final-output
                          (nrdl:generate-to strm final-result :pretty-indent 4)
                          (terpri strm))
                         (values
                           (gethash status cliff/errors:*exit-codes*
                                    (gethash
                                      :unknown-error
                                      cliff/errors:*exit-codes*
                                      128))
                           final-result)))))
          (serious-condition (e)
            (let* ((status (cliff/errors:exit-status e))
                   (final-result
                     (alexandria:alist-hash-table
                       (concatenate
                         'list
                         `((:status .  ,status)
                           (:error-message . ,(format nil "~A" e)))
                         (cliff/errors:exit-map-members e)))))
              (nrdl:generate-to err-strm final-result :pretty-indent 4)
              (terpri err-strm)
              (values
                (gethash status cliff/errors:*exit-codes*
                         (gethash :unknown-error cliff/errors:*exit-codes*
                                  128))
                final-result)))))

(defun ensure-option-exists (key options)
  (restart-case
      (progn
        (let ((value (gethash key options)))
          (unless value
            (error 'cliff/errors:exit-error
                   :status :cl-usage-error
                   :map-members `((:missing-option . ,key))))
            value))
    (continue ()
      :report "Continue with the option set to nil."
      nil)
    (use-value (value)
      :report "Use a value provided."
      :interactive (lambda ()
                     (format *query-io*
                             "Enter a value for ~A: " key)
                     (read *query-io*))
      value)))
