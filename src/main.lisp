(or)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.cliff (:use #:cl)
  (:documentation
    "
    Package that has a function, @c(execute-program), which does the following:

    @begin(list)
      @item(Registers subcommand-functions mapped to specific subcommands)
      @item(Reads configuration files in standard locations)
      @item(Reads environment variables)
    @end(list)

    According to specific rules and from these rules constructs a hash table
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
      *find-tag*
      invalid-subcommand
      necessary-env-var-absent
      find-file
      execute-program
      ensure-option-exists))

(in-package #:com.djhaskin.cliff)
(cl-reexport:reexport-from '#:com.djhaskin.cliff/errors)

(defparameter *str-hash-init-args*
  (list :test #'equal))

(defparameter *kv-hash-init-args* nil)

(defun repeatedly-eq
  (func
    arg
    &optional
    (eeq #'equal)
    (repeats 256))
  "
  List consisting of calls to func using arg, then calling func using that
  result, etc. Stops when subsequent calls return equal.
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
  Slurp a resource, using specified options. Return the contents of the
  resource as a string.

  If @cl:param(resource) is a URL, download the contents according to the
  following rules:
  @begin(list)
    @item(If it is of the form `http(s)://user:password@url`,
        it performs basic HTTP authentication using the provided username and
        password;)
    @item(If it is of the form `http(s)://header=val@url`,
        the provided header is set when downloading the contents;)
    @item(If it is of the form `http(s)://<token>@url`,
        bearer authorization is used with the provided token;)
    @item(If it is of the form @c(file://<location>),
        it is loaded as a normal file;)
    @item(If it is of the form @c(-) the contents are loaded from standard
        input;)
  @end(list)

  Otherwise, the contents are loaded from the resource as if it named
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
  (:documentation
    "
    Condition used by CLIFF to signal that a required environment variable
    is not present.

    Implmements @c(exit-status) and @c(exit-map-members).
    ")
  (:report (lambda (condition stream)
             (format stream "Environment variable `~A` not specified.~&"
                     (env-var condition)))))

(defmethod cliff/errors:exit-status ((this necessary-env-var-absent))
  :configuration-error)

(defmethod cliff/errors:exit-map-members ((this necessary-env-var-absent))
  `((:env-var . ,(env-var this))))


#+(or)
(os-specific-config-dir "halo" (lambda (x &optional y)
                                 (or (uiop/os:getenv x) y)))


(defun os-specific-system-config-dir (program-name getenv)
  "
  Determines the OS-specific system configuration folder base.
  "
  (make-os-specific-path
    #+windows (concatenate
                'string
                (funcall getenv "PROGRAMDATA"
                         "C:\\ProgramData")
                "\\"
                program-name
                "\\")
    #+darwin (concatenate
               'string
               "/Library/Preferences/"
               program-name
               "/")
    #-(or darwin windows)
    (concatenate
      'string
      "/etc/"
      program-name
      "/")))

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
  Starting at the directory @cl:param(from), look for the file
  @cl:param(marker).

  Continue looking for the file in successive parents of @cl:param(from)
  until no more parents exist or the file is found.

  Returns the pathname of the found file or nil if no file could be found.
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
  "
  Serialize @cl:param(thing) to @link[uri=\"https://github.com/djha-skin/nrdl\"](NRDL).
  "
  (with-output-to-string (strm)
    (nrdl:generate-to strm thing :pretty-indent pretty)))

(defun parse-string (thing)
  "
  Parse the @link[uri=\"https://github.com/djha-skin/nrdl\"](NRDL) string @cl:param(thing).
  "
  (with-input-from-string (strm thing)
    (nrdl:parse-from strm)))

(defparameter
  *find-tag*
  (cl-ppcre:create-scanner
    "^--([^-]+)-(.+)$")
  "
  @c(cl-ppcre) regex scanner containing two capture groups, the first of which
  must capture the CLI verb (one of @c(enable), @c(disable), @c(reset),
  @c(add), @c(set), @c(nrdl), or @c(file)), and the second of which is the name
  of the variable. This is used ultimately by @c(execute-program) to recognize
  command line options (as opposed to subcommands). Currently its value corresponds
  to the regular expression @c(\"^--([^-]+)-(.+)$\").
  ")

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
    map-sep-pat kact kopt &optional value)
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
                   (apply #'make-hash-table *kv-hash-init-args*)))
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
  (args &optional &key initial-hash (map-sep "="))
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
                (apply #'make-hash-table *kv-hash-init-args*)
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
                (*find-tag*
                  arg)
                (cond
                  ((some (lambda (x) (eql kact x)) +single-arg-actions+)
                   (ingest-option
                     opts map-sep-pat kact kopt)
                   (setf consumable rargs))
                  ((some (lambda (x) (eql kact x)) +multiple-arg-actions+)
                   (when (not rargs)
                     (error "not enough arguments for action ~a on option ~a"
                            kact kopt))
                   (let ((value
                           (first rargs))
                         (rrargs (rest rargs)))
                     (ingest-option
                       opts map-sep-pat kact kopt value)
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
    map-sep-pat list-sep-pat ktag kopt context value)
  (cond
    ((eql ktag :flag)
     (if (some
           (lambda (v)
             (eql value v))
           '("0" "false" "False" "FALSE" "no" "NO" "No"))
       (ingest-option opts map-sep-pat :disable kopt)
       (ingest-option opts map-sep-pat :enable kopt)))
    ((eql ktag :item)
     (ingest-option opts map-sep-pat :set kopt value))
    ((eql ktag :list)
     (loop for piece in (reverse (cl-ppcre:split list-sep-pat value)) do
           (ingest-option
             opts
             map-sep-pat
             :add
             kopt
             piece)))
    ((eql ktag :table)
     (loop for piece in (cl-ppcre:split list-sep-pat value) do
           (ingest-option opts
                          map-sep-pat
                          :join
                          kopt
                          piece)))
    ((eql ktag :nrdl)
     (ingest-option opts
                    map-sep-pat
                    ktag
                    kopt
                    value))
    ((eql ktag :file)
     (ingest-option opts
                    map-sep-pat
                    :file
                    kopt
                    value))
    ((eql ktag :raw)
     (ingest-option opts
                    map-sep-pat
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
(defun expand-env-aliases (aliases env)
  "
  Replace the names of environment variables according to the map
  found in `aliases`. The keys of the `aliases` hash table are matched
  as strings to the name of the environment variable names (keys in the env var
                                                                 map). If they match, the key is removed from the hash table and a key with
  the value of the associated entry in `aliases` is used as the new key,
  associating it with the value of the old removed key.
  "
  (declare (type hash-table aliases env))
  (let ((result (apply #'make-hash-table *str-hash-init-args*)))
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
      (list-sep ",")
      (map-sep "="))
  "
  Consume variables, presumably collected from the OS.
  Assumes any aliases have already been expanded (that is,
  it uses the variables as-is, with no transformation).

  Assumes an open-world configuration.

  For each environment variable, it examines its form.

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
           (type string list-sep)
           (type string map-sep))
  (let*
    ((result
       (apply
         #'make-hash-table *kv-hash-init-args*))
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
  (flet ((envvar (var &optional default)
           (let ((result (gethash var environment default)))
             (if (null result)
                 (if (null default)
                     (error 'necessary-env-var-absent
                            :env-var var)
                     default)
                 result))))
    (let* ((result (alexandria:copy-hash-table defaults))
           (system-config-path (os-specific-system-config-dir
                                 program-name #'envvar))
           (system-config-path-file
             (merge-pathnames
               (make-pathname
                 :name
                 "config"
                 :type
                 "nrdl")
               system-config-path))
           (home-config-path (os-specific-config-dir program-name #'envvar))
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
                                      root-path
                                      reference-file)))
                   (when marked-file
                     (merge-pathnames
                       marked-config-file-name
                       (uiop/pathname:pathname-parent-directory-pathname
                         marked-file)))))
               (when root-path
                 (find-file
                   root-path
                   marked-config-file-name)))))
      (when (uiop/filesystem:file-exists-p system-config-path-file)
        (update-hash result (parse-string (data-slurp system-config-path-file))))
      (when (uiop/filesystem:file-exists-p home-config-path-file)
        (update-hash result (parse-string (data-slurp home-config-path-file))))
      (when (and
              marked-config-path
              (uiop/filesystem:file-exists-p marked-config-path))
        (update-hash result (parse-string (data-slurp marked-config-path))))
      result)))

(define-condition invalid-subcommand (error)
  ((given-subcommand :initarg :given-subcommand
                     :initform nil
                     :reader given-subcommand))
  (:documentation
    "
    Condition used by CLIFF to signal there were no functions given to
    CLIFF that correspond the subcommand given.

    Implmements @c(exit-status) and @c(exit-map-members).
   ")
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
      subcommand-functions
      cli-aliases
      environment-aliases
      list-sep
      map-sep)
  (declare (type (or stream boolean) strm)
           (type string program-name)
           (type hash-table options)
           (type list other-args)
           (type (or null pathname) reference-file)
           (type pathname root-path)
           (type list helps)
           (type string list-sep)
           (type string list-sep))
  (format
    strm "~@{~@?~}"

    "~&Welcome to `~A`!~%" program-name
    "~%"
    "This is a CLIFF-powered program.~%"
    "~%"
    "Configuration files and, optionally, environment variables can be set~%"
    "using NRDL, a JSON superset language. Output will be a NRDL document.~%"
    "More information about NRDL can be found here:~%"
    "~%"
    "https://github.com/djha-skin/nrdl~%"
    "~%"
    "Options can be given via:~%"
    "  - Configuration file, as in `{ <option> <value> }`~%"
    "  - Environment variable, as in `~A_<KIND>_<OPTION>`~%" (string-upcase
                                                               (cl-ppcre:regex-replace-all
                                                                 "\\W" program-name "_"))
    "  - Command line, as in `--<action>-<option>`~%"
    "~%"
    "Configuration files are consulted first, then environment variables, then~%"
    "the command line, with later values overriding earlier ones.~%~%")
  ; Config files section
  (format strm "Configuration files can be in the following locations:~%")
  (format
    strm "~@{~@?~}"
    "  - A system-wide config file in an OS-specific location:~%"
    "      - On Windows: `%PROGRAMDATA%\\~A\\config.nrdl`~%" program-name
    "        (by default `C:\\ProgramData\\~A\\config.nrdl`)~%~%"
    "      - On Mac:   `/Library/Preferences/~A/config.nrdl`~%" program-name
    "      - On Linux/POSIX: `/etc/~A/config.nrdl`~%" program-name
    "        (by default ~~/.config/~A/config.nrdl)` ~%" program-name
    "  - A home-directory config file in an OS-specific location:~%"
    "      - On Windows: `%LOCALAPPDATA%\\~A\\config.nrdl`~%" program-name
    "        (by default `%USERPROFILE%\\AppData\\Local\\~A\\config.nrdl`)~%~%"
    program-name
    "      - On Mac:   `~~/Library/Preferences/~A/config.nrdl`~%" program-name
    "      - On Linux/POSIX: `${XDG_CONFIG_HOME}/~A/config.nrdl`~%" program-name
    "        (by default ~~/.config/~A/config.nrdl)` ~%" program-name)
  (if reference-file
      (format strm "~@{~@?~}"
              "  - A file named `.~A.nrdl` found in the same directory~%"
              program-name
              "    as `~A`, which is searched for in `~A`~%"
              (namestring reference-file)
              (namestring root-path)
              "    (or any of its parents)~%")
      (format strm "~@{~@?~}"
              "  - A file named `.~A.nrdl` in the directory `~A`~%"
              program-name
              (namestring root-path)
              "    (or any of its parents)~%")
    )
  ; Environment variables section
  (let ((clean-program-name
          (string-upcase
            (cl-ppcre:regex-replace-all
              "\\W" program-name "_"))))
    (format
      strm "~@{~@?~}"
      "Options can be set via environment variable as follows:~%"
      "  - `~A_FLAG_<OPTION>=1` to enable a flag~%"
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
      clean-program-name
      "     of a NRDL document from a file as if by the `--file-*` flag~%"
      "  - `~A_RAW_<OPTION>=<FILE_PATH>` to set a value using the raw bytes~%"
      clean-program-name
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
  (when environment-aliases
    (format
      strm
      "~@{~@?~}"
      "The following environment variable aliases have been defined:~%"
      "~%"
      "~10@A ~49@A~%" "This" "Translates To")

    (loop for ( this . that ) in environment-aliases
          do
          (format strm "~10@A ~49@A~%" this that)
          finally
          (format strm "~%")))
  (when cli-aliases
    (format
      strm
      "~@{~@?~}"
      "The following command line aliases have been defined:~%"
      "~%"
      "~10@A ~49@A~%" "This" "Translates To")

    (loop for ( this . that ) in cli-aliases
          do
          (format strm "~10@A ~49@A~%" this that)
          finally
          (format strm "~%")))

  (format strm "The following options have been detected:~%")
  (nrdl:generate-to strm options :pretty-indent 4)
  (finish-output strm)
  (let* ((entry (assoc other-args helps :test #'equal))
         (helpstring (cdr entry))
         (callable-function (assoc other-args subcommand-functions :test #'equal)))

    (when (assoc nil subcommand-functions)
      (progn
        (format strm "~%The bare command `~A` (with no subcommands) performs an ~
                action (rather than just printing a help page).~%" program-name)))

    (when (> (length subcommand-functions)
             (if (assoc nil subcommand-functions :test #'equal)
                 1
                 0))
      (format strm "~%~%Available subcommands:~%")
      (loop for (key . _) in subcommand-functions
            if (not (null key))
            do
            (format strm "  - `~{~A~^ ~}`~%" key)))

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
    '((:status . :successful))))

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
    :environment-variables
      '(("HOME" . "/home/skin")
        ("HI_ITEM_FOO" . "25")
        ("HI_LIST_BAR" . "1,2,3,4,5")
        ("HI_TABLE_BAZ" . "a=1,b=2,c=3")
        ("HI_FLAG_QUUX" . "1")
        ("HI_NRDL_DISEASE" . "{he \"could\" do false it \"rightnow\"}"))
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
      &key
      (cli-arguments t)
      (err-strm *error-output*)
      (list-sep ",")
      (map-sep "=")
      (setup #'identity)
      (strm *standard-output*)
      (teardown #'identity)
      cli-aliases
      default-func-help
      default-function
      defaults
      disable-help
      environment-aliases
      environment-variables
      reference-file
      root-path
      subcommand-functions
      subcommand-helps
      suppress-final-output)
  "
  @begin(section)
  @title(Overview)

  The function @c(execute-program) aims to be a simple to use one stop shop for
  all your command line needs.

  The function gathers options from the Option Tower out of configuration
  files, environment variables, and the command line arguments into an options
  table. Then it calls the action function, which is a user-defined function
  based on what subcommand was specified; either the
  @cl:param(default-function) if no subcommands were given, or the function
  corresponding to the subcommand as given in @cl:param(subcommand-functions)
  will be called. Expects that function to return a results map, with at least
  the @c(:status) key set to one of the values listed in the @c(*exit-codes*)
  map.

  @end(section)

  @begin(section)
  @title(The Options Tower)

  @begin(section)
  @title(Configuration Files)

  The function first builds, in successive steps, the options table which will
  be passed to the function in question.

  It starts with the options hash table given by the @cl:param(defaults)
  parameter.

  The function next examines the given @cl:param(environment-variables),
  which should be given as an alist with keys as variable names and values as
  their values. If no list is given, the currently environment variables will
  be queried from the OS.

  Uses those environment variables to find an OS-specific system-wide
  configuration file in one of the following locations:

  @begin(list)
    @item(@b(Windows): @c(%PROGRAMDATA%\<program-name>\config.nrdl), or
        @c(C:\ProgramData\<program-name>\config.nrdl) if that environment
        variable is not set.)
    @item(@b(Mac): @c(/Library/Preferences/<program-name>/config.nrdl))
    @item(@b(Linux/POSIX): @c(/etc/<program-name>/config.nrdl))
  @end(list)

  It reads this file and deserializes the options from it, merging them into
  the options table, overriding any options when they exist both in the map and
  the file.

  Next, it looks for options in an OS-specific, user-specific
  home-directory-based configuration file in one of the following locations:

  @begin(list)
    @item(@b(Windows): @c(%LOCALAPPDATA%\<program-name>\config.nrdl), or
        @c(%USERPROFILE%\AppData\Local\<program-name>\config.nrdl) if that
        environment variable is not set.)
    @item(@b(Mac): @c($HOME/Library/Preferences/<program-name>/config.nrdl))
    @item(@b(Linux/POSIX): @c($XDG_CONFIG_HOME/<program-name>/config.nrdl), or
        @c($HOME/.config/<program-name>/config.nrdl) if XDG_CONFIG_HOME is not
        set.)
  @end(list)

  When performing this search, @c(execute-program) may signal an error of type
  @c(necessary-env-var-absent) if the HOME var is not set on non-Windows
  environments and the USERPROFILE variable if on Windows.

  If it finds a @link[uri=\"https://github.com/djha-skin/nrdl\"](NRDL) file in
  this location, it deserializes the contents and merges them into the options
  table, overriding options when they exist both in the map and the file.

  Finally, it searches for the @cl:param(reference-file) in the
  @cl:param(root-path). If it can't find the @cl:param(reference-file) in
  @cl:param(root-path), it searches successively in all of
  @cl:param(root-path)'s parent directories.

  If it finds such a file in one of these directories, it next looks for the
  file @c(.<program-name>.nrdl) in that exact directory where
  @cl:param(reference-file) was found. If that file exists, @c(execute-program)
  deserializes the contents and merges them into the options table, overriding
  options when they exist both in the table and the file.

  If @cl:param(reference-file) is not given, it is simply taken to be the
  configuration file itself, namely @c(.<program-name>.nrdl). If
  @cl:param(root-path) is not given, it is taken to be the present working
  directory.

  @end(section)

  @begin(section)
  @title(Environment Variables)

  Next, it examines the @cl:param(environment-variables) for any options given
  by environment variable. Again, @cl:param(environment-variables) should be
  given as an alist with keys as variable names and values as their values. If
  no list is given, the currently environment variables will be queried from
  the OS.

  For each environment variable, it examines its form.

  If the variable matches the
  @link[uri=\"http://edicl.github.io/cl-ppcre/\"](regular expression)
  @c(^(<PROGRAM_NAME>)_(?P<opt>LIST|TABLE|ITEM|FLAG|NRDL)_(?P<arg>.*)$), then
  the variable's value will be used to add to the resulting options hash table,
  overriding any options which are already there.

  If the @c(opt) part of the regex is @c(LIST), the value of the variable will
  be split using @c(list-sep) and the resulting list of strings will be
  associated with the keyword @c(arg) in the options.

  If the @c(opt) is @c(TABLE), the value of the variable will be split using
  @cl:param(list-sep), then each entry in that list will also be split using
  @cl:param(map-sep). The resulting key/value pair list is turned into a
  hash-table and this hash table is associated to the keyword @c(arg) in the
  options.

  If the @c(opt) is @c(ITEM), the value of the variable will be set to the
  keyword @c(arg) in the options.

  If the @c(opt) is @c(NRDL), the value of the variable will be parsed as a NRDL
  string and its resultant value set as the value of the keyword @c(arg) in the
  returned options hash table.

  In addition, any environment variables whose names match any keys in the
  @cl:param(environment-aliases) alist will be treated as if their names were
  actually the value of that key's entry.

  @end(section)

  @begin(section)
  @title(Command Line Arguments)

  Finally, @c(execute-program) turns its attention to the command line.

  It examines each argument in turn. It looks for options of the form
  @c(--<action>-<option-key>), though this is configurable via @c(*find-tag*).
  It deals with options of this form according to the following rules:

  @begin(list)

    @item(If the argument's action is @c(enable) or @c(disable) the keyword
      named after the option key is associated with @c(t) or @c(nil) in the
      resulting hash table, respectively.)

    @item(If the argument's action is @c(set), the succeeding argument is taken
      as the string value of the key corresponding to the option key given in
      the argument, overriding any previously set value within the option
      table.)

    @item(If the argument's action is @c(add), the succeeding argument is taken
      as a string value which must be appended to the value of the option key
      within the option table, assuming that the value of such is already a
      list.)

    @item(If the argument's action is @c(join), the succeeding argument must be
      of the form @c(<key><map-sep><value>), where @c(map-sep) is the value of
      the parameter @cl:param(map-sep). The key specified becomes a keyword,
      and the value a string, set as a hash table entry of the hash table found
      under the option key within the parent option table, assuming the value
      of such is already a hash table.)

    @item(If the argument's action is @c(nrdl), the succeeding argument must be
      a valid NRDL document, specified as a string. This argument's deserialized
      value will be taken as the value of the option key within the option map,
      overriding any previously set value within the option table.)

    @item(If the argument's action is @c(file), it will be assumed that the
    succeeding argument names a resource consumable via @c(data-slurp). That
    resource will be slurped in via that function, then deserialized from NRDL.
    The resulting data will be taken as the value of the option key within the
    option table, overriding any previously set value within that table.)

    @item(If the argument's action is @c(raw), it will be assumed that the
      succeeding argument names a resource consumable via @c(data-slurp). That
      resource will be slurped in via that function, as a raw string. That
      string will be taken as the value of the option key within the option
      table, overriding any previously set value within that table.)

  @end(list)

  In addition, any argument of any form whose string value @cl:spec(equal)'s
  any keys in the @cl:param(cli-aliases) alist will be treated as if their
  string value were actually the value of that key's entry.

  @end(section)

  @begin(section)
  @title(The Setup Function)

  Finally, if a setup function is specified via @cl:param(setup), it is called
  with one argument: the options table so far. This function is expected to
  add or remove elements from the options table and return it.

  Having done all this, @c(execute-program) considers the option table is
  complete and prepares to feed it to the action function.

  @end(section)

  @end(section)

  @begin(section)
  @title(Determining the Action Function)

  Any other arguments which @c(execute-program) finds on the command line other
  than those recognized either as @cl:param(cli-aliases) or as options of the
  form matched by @c(*find-tag*) will be taken as subcommand terms.

  @c(execute-program) then finds all such terms puts them in a list in the
  order in which they were found. It attempts to find this list (using
  @cl:spec(equal)) in the alist @cl:param(subcommand-functions). If such a list
  exists as a key in that alist, the value corresponding to that key is taken
  to be a function of one argument. This function expects a hash table, the
  options map previously constructed.

  If no subcommand was given, it calls @cl:param(default-function) instead.

  @end(section)

  @begin(section)
  @title(The Help Page)

  By default, if @c(execute-program) sees the @c(help) subcommand on in the
  command line arguments, it will print a help page to @cl:param(err-strm).
  @cl:param(err-strm) may be given as @c(t), @c(nil), or otherwise must be a
  stream, just as when calling @cl:spec(format). If left unspecified,
  @cl:param(err-strm) defaults to standard error. This behavior may be
  suppressed by setting the @cl:param(disable-help) option to @c(nil). If
  disabled, Users may then define their own help pages by specifying functions
  that print them using @cl:param(subcommand-functions).

  This help page gives users the following information:

  @begin(list)
    @item(Details to users how they may specify options using the Options Tower
          for the program)
    @item(Lists all defined environment variable aliases)
    @item(Lists all defined command line interface aliases)
    @item(Prints out all options found within the Options Tower in NRDL format.)
    @item(Prints out whether there is a default action (function) defined.)
    @item(Prints out all available subcommands).
  @end(list)

  If there were any subcommand terms after that of the @c(help) term in the
  command line arguments, they are put in a list and @c(execute-program)
  attempts to find this list (again, using @cl:spec(equal)) as a key in the
  alist @cl:param(subcommand-helps). It then prints this help string as part of
  the documentation found in the help page. If it If there were no such terms
  after the @c(help) term in the command line arguments, @c(execute-program)
  prints the help string found in @cl:param(default-help), if any.

  @end(section)

  @begin(section)
  @title(Execution)

  If @c(execute-program) determines the user function to call and what options
  to put in the option table, it calls that function. This function is either
  that which prints the default help page as described above, it comes from
  @cl:param(subcommand-functions) and was chosen based on present subcommand
  terms on the command line, or comes from @cl:param(default-function) if no
  subcommand was given on the command line. If no match was found in
  @cl:param(subcommands-functions) matching the subcommands given on the
  command line, an error is printed. This function is called the action
  function.

  It computes the result hash table by taking the return value value of the
  action function passes it to the function specified in the
  @cl:param(teardown) parameter, if it was given. If not, the result from the
  action function is taken as the result hash table itself.

  By default, it then prints this hash table out to @cl:param(strm) as a
  prettified NRDL document. @cl:param(strm) may be given as @c(t), @c(nil), or
  otherwise must be a stream, just as when calling @cl:spec(format). If left
  unspecified, @cl:param(strm) defaults to standard output.

  This return value is expected to be a hash table using @cl:spec(eql)
  semantics. That table must contain at least one value under the @c(:status)
  key. The value of this key is expected to be one of the keys found in the
  @c(*exit-codes*) alist corresponding to what should be the exit status of the
  whole program. If the function was successful, the value is expected
  to be @c(:successful). This value will be used as the key to look up a
  numeric exit code from @c(*exit-codes*). The numeric exit code found will be taken as
  the desired exit code of the whole program, and will be the first value
  returned by the function @c(execute-program). The second value will be the
  result hash table itself.

  @end(section)

  @begin(section)
  @title(Error Handling)

  During the entirety of its run, @c(execute-program) handles any and all
  @cl:spec(serious-condition)s. If one is signaled, it computes the exit status
  of the condition using @c(exit-status) and creates a final result vector
  containing the return value of that function under the @c(:status) key. It
  then populates this table with a key called @c(error-message) and any
  key/value pair found in the alist computed by calling @c(exit-map-members) on
  the condition.

  It prints this table out in indented NRDL format to @cl:param(err-strm) (or
  standard error if that option is left unspecified) unless
  @cl:param(suppress-output) is given as @c(t).

  @c(execute-program) then returns two values: the numeric exit code
  corresponding to the exit status computed as described above, and the newly
  constructed result map containing the error information.

  @end(section)
  @begin(section)
  @title(Discussion)

  Command line tools necessarily need to do a lot of I/O. @c(execute-program)
  attempts to encapsulate much of this I/O while providing
  @link[uri=\"https://blog.cleancoder.com/uncle-bob/2012/08/13/the-clean-architecture.html\"](clean
  architecture) by default. Ideally, @c(execute-program) should enable an
  action function to be relatively pure, taking an options hash table and
  returning a result hash table with no other I/O required. This is why
  @c(execute-program) prints out the resulting hash table at the end. If a pure
  action function is called, hooking it up to a subcommand or as the default
  command action using @c(execute-program) should enable this function to
  interact with the outside world by means of its result table.

  It was also written with dependency injection, testing, and the REPL in mind.
  Since @c(execute-program) doesn't actually exit the process at the end, only
  returning values instead, @c(execute-program) may simply be called at the
  REPL. Many arguments to the function only exist for dependency injection,
  which enables both testing and REPL development. Generally, many arguments
  won't be specified in a function call, such as the @cl:param(cli-arguments),
  @cl:param(environment-variables), @cl:param(err-strm) and @cl:param(strm)
  parameters (though of course they may be specified if e.g. the user needs to
  redirect output to a file at need.)

  @end(section)
  "

  (declare (type string program-name)
           (type (or null list) environment-variables)
           (type list subcommand-functions)
           (type (or null function) default-function)
           (type (or boolean list) cli-arguments)
           (type list cli-aliases)
           (type list environment-aliases)
           (type (or null pathname) root-path)
           (type (or null pathname) reference-file)
           (type list defaults)
           (type string list-sep)
           (type string map-sep)
           (type function setup)
           (type function teardown))
  (handler-case
    (let* ((effective-defaults (if (null defaults)
                                   (apply #'make-hash-table *kv-hash-init-args*)
                                         (apply #'alexandria:alist-hash-table
                                                defaults
                                                *kv-hash-init-args*)))
                 (effective-environment-variables
                   (if environment-variables
                       (apply
                         #'alexandria:alist-hash-table environment-variables
                         *str-hash-init-args*)
                       (system-environment-variables)))
                 (effective-environment
                   (expand-env-aliases
                     (apply #'alexandria:alist-hash-table
                            environment-aliases
                            *str-hash-init-args*)
                     effective-environment-variables))
                 (effective-arguments
                   (if (eq cli-arguments t)
                       (uiop:command-line-arguments)
                       cli-arguments))
                 (effective-cli
                   (expand-cli-aliases
                     (apply #'alexandria:alist-hash-table
                            cli-aliases
                            *str-hash-init-args*)
                     effective-arguments))
                 (effective-root (or root-path (uiop/os:getcwd))))
        (multiple-value-bind (result found-config-files)
            (config-file-options
              program-name
              effective-environment
              effective-defaults
              reference-file
              effective-root)
          (declare (ignore found-config-files))
          (update-hash
            result
          (consume-environment
            program-name
            effective-environment
            :list-sep list-sep
            :map-sep map-sep))
        (multiple-value-bind
            (opts-from-args other-args)
            (consume-arguments
              effective-cli
              :initial-hash result
              :map-sep map-sep)
          (let* ((help-function
                   (lambda (opts)
                     (default-help
                       err-strm
                       program-name
                       opts
                       (cdr other-args)
                       reference-file
                       effective-root
                       (if default-func-help
                           (acons '() default-func-help subcommand-helps)
                           subcommand-helps)
                       (if default-function
                           (acons '() default-function subcommand-functions)
                           subcommand-functions)
                       cli-aliases
                       environment-aliases
                       list-sep
                       map-sep)))
                 (effective-functions
                   (acons
                     '()
                     (or
                       default-function help-function)
                     subcommand-functions))
                 (setup-result (funcall setup opts-from-args))
                     (subcommand-function
                       (or (and (not disable-help)
                                (equal (first other-args) "help")
                                help-function)
                           (cdr (assoc other-args effective-functions :test #'equal))
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
  "
  Check options hash table @cl:param(options) for the key @cl:param(key).

  Signal a restartable condition if the option is missing. Employs the
  @cl:spec(use-value) and @cl:spec(continue) restarts in that case.
  "
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
