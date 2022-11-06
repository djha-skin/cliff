(in-package
 #:cl-user)
(defpackage
    #:cl-i (:use #:cl)
    (:documentation
     "package that has a function,
    `start-cli`, which does the
    following: - registers functions
    mapped to specific subcommands -
    reads configuration files in
    standard locations - reads
    environment variables according
    to spefic rules and from these
    rules constructs a hash table
    which is then passed to the
    subcommands.")
    (:import-from #:alexandria)
    (:import-from #:fset)
    (:import-from #:arrows)
    (:import-from #:cl-yaml)
    (:import-from #:dexador)
    (:import-from #:flexi-streams)
    (:import-from #:uiop/pathname)
    (:import-from #:quri)
    (:export
     consume-arguments
     data-slurp dbg
     find-file
     generate-string
     parse-string
     repeatedly
     repeatedly-eq
     slurp-stream))
(in-package #:cl-i)

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
  (declare (integer repeats))
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
      &optional
      (check-p (lambda (arg) (equal arg nil)))
      (repeats 256))
  "
  list consisting of calls to func on arg, then calling func on that result,
  etc.
  doesn't stop until check-p returns t when given `arg`.
  "
  (declare (integer repeats))
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
         (if (funcall check-p arg)
             nil
             (cons
               arg (helper
                     (funcall func arg)
                     (- on 1))))))
    (helper arg repeats)))

					; get file
(defun
    find-file
    (from cmd-name)
  "
  starting at the directory given,
  find the marking file.
  "
  (arrows:as->
   from it
   (repeatedly-eq
    #'uiop/pathname:pathname-parent-directory-pathname it)
   (mapcar
    (lambda
        (path)
      (merge-pathnames
       path
       (pathname
        (format
         nil ".~a.yaml" cmd-name))))
    it)
   (some
    #'uiop/filesystem:file-exists-p it)))

(defmacro
    dbg
    (body
      &optional
      (destination
        *standard-output*))
  (let
      ((rbody
         (gensym
           "cl-i-dbg"))
       (rdest
         (gensym
           "cl-i-dbg")))
    `(let
         ((,rbody
            ,body)
          (,rdest
            ,destination))
       (format
         ,rdest "debug: type of `~a` = `~a`~%" (quote
                                                 ,body)
         (type-of
           ,rbody))
       (format
         ,rdest "debug: eval of `~a` = `~a`~%" (quote
                                                 ,body) ,rbody)
       (finish-output
         ,rdest)
       ,rbody)))

(defun
    generate-string
    (thing)
  (yaml:with-emitter-to-string
    (emit)
    (yaml:emit-pretty-as-document
      emit thing)))

(defun parse-string (thing)
  (yaml:parse thing))

(defun exit-error
    (status
      msg &optional (destination
                      *standard-output*))
  (format destination "~a~%" msg)
  status)

(defun
    slurp-stream (f)
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
(defun data-slurp (resource &rest more-args)
  "
  slurp config, using specified options.

  if `:resource` is a url, download the contents according to the following
  rules:
  - if it is of the form `http(s)://user:pw@url`,
  it uses basic auth;
  - if it is of the form `http(s)://header=val@url`,
  a header is set;
  - if it is of the form `http(s)://tok@url`,
  a bearer token is used;
  - if it is of the form `file://loc`, it is loaded as a normal file;
  - if it is of the form `-`, the data is loaded from standard input;
  - otherwise, the data is loaded from the string or pathname as if it named
  a file.
  "
  (flet
      ((http-call
           (&rest
             args)
         (apply
           #'dex:get
           (concatenate
             'list
             args
             more-args
             '(:force-string
               t)))))
    (or
      (when
          (equal
            'pathname (type-of
                        resource))
        (base-slurp
          resource))
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
              "authorization"
              (format
                nil "bearer ~a" (quri:url-decode
                                  token))))))
      (cl-ppcre:register-groups-bind
        ()
        ("^https?://.*$"
         resource)
        (http-call
          resource))

      (cl-ppcre:register-groups-bind
        (_
          device-name path)
        ("^file://(([^/]+):)?/(.*)$"
         resource)
        (let
            ((path-components
               (cl-ppcre:split
                 "/+" path)))
          (arrows:as->
            path-components *
            (cons
              :absolute *)
            (if
                device-name
                (list
                  :device device-name
                  :directory *)
                (list
                  :directory *))
            (apply
              #'make-pathname *)
            (base-slurp
              *))))
      (base-slurp
        resource))))

(defun
    string-keyword (str)
  (intern
   (string-upcase
    str) "KEYWORD"))

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
    :yaml
    :file
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


(defun
    consume-environment
    (environment
     slug
     &optional
     &key
       (list-sep
	",")
       (map-sep
	"=")
       (hash-init-args
	'())))

(defun
    ingest-option
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
             (setf (gethash k (gethash kopt opts)) v))
           (error "not a k/v pair, check map sep pattern: ~a" value))))
    ((eql kact :yaml)
     (setf (gethash kopt opts)
           (parse-string value)))
    ((eql kact :file)
     (setf (gethash kopt opts)
           (parse-string (data-slurp value))))
    (:else
     (error "uknown action ~a" kact))))

(defun
    consume-arguments
    (args &optional &key hash-init-args (map-sep "="))
  "
  Consume arguments, presumably collected from the command line.
  Assumes any aliases have already been expanded (that is,
  it uses the arguments as-is, with no transformation).
  "
      (let ((map-sep-pat (cl-ppcre:create-scanner map-sep))
            (consumable (copy-list args))
            (other-args nil)
            (opts (apply #'make-hash-table hash-init-args)))
        (loop while consumable do
          (let
              ((arg (first consumable))
               (rargs (rest consumable)))
            (or
              (cl-ppcre:register-groups-bind
                ((#'string-keyword
                  kact)
                 (#'string-keyword
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
                     (setf consumable rrargs))))
                t)
              (progn
                (push
                  arg other-args)
                (setf
                  consumable rargs)))))
    (values
      opts other-args)))

(defun
    ingest-var
    (program-name
      opts
      map-sep-pat list-sep-pat hash-init-args ktag kopt value)
  (cond
    ((eql ktag :flag)
     (if (some
           (mapcar
             (lambda (v)
               (eql value v))
             '("0" "false" "False" "FALSE" "no" "NO" "No")))
     (ingest-option opts map-sep-pat hash-init-args :disable kopt)
     (ingest-option opts map-sep-pat hash-init-args :enable kopt)))
    ((eql ktag :item)
     (ingest-option opts map-sep-pat hash-init-args :set kopt value))
    ((eql ktag :list)
     (loop for piece in (cl-ppcre:split list-sep-pat value) do
           (ingest-option opts map-sep-pat hash-init-args :add kopt piece)))
    ((eql ktag :table)
     (loop for piece in (cl-ppcre:split list-sep-pat value) do
           (ingest-option opts map-sep-pat hash-init-args :join kopt piece)))
    ((or (eql ktag :yaml)
         (eql ktag :file)
         (ingest-option opts map-sep-pat hash-init-args ktag piece)))
    (t
     (error "Unknown tag while parsing env vars for program `~A`: `~A`"
            program-name
            ktag))))



     (ingest-option

;; TODO: Test this
(defun
    expand-arg-aliases
    (aliases
     args)
  (mapcar
   (lambda
       (arg)
     (or
      (gethash
       arg aliases)
      arg))
   args))


;; TODO: TEST THIS
(defun
    expand-env-aliases
    (aliases
      env)
  "
  Replace the names of environment variables according to the map
  found in `aliases`.
  "
  (loop
    for key being the hash-key of env
    using (hash-value value)
    collect (cons (or (gethash key aliases) key) value)))

(defun
    consume-env-vars
    (program-name
      env
      &optional &key hash-init-args (list-sep
                                      ",")
                                      (map-sep
                                      "="))
  "
  Consume OS Environment variables as part of the options map.
  "
  (let
    ((result
       (apply
         #'make-hash-table hash-init-args))
     (clean-name
       (string-upcase
         (cl-ppcre:regex-replace-all
           "\\W" "_" program-name)))
     (var-pattern
       (cl-ppcre:create-scanner
         (concatenate
           'string
           "^"
           "\\Q"
           clean-name
           "\\E"
           "_"
           "(LIST|TABLE|ITEM|FLAG|YAML|FILE)"
           "_"
           "(\\w+)$")))
     (map-sep-pat
       (cl-ppcre:create-scanner
         (concatenate
           'string
           "\\Q"
           map-sep
           "\\E")))
     (list-sep-pat
       (cl-ppcre:create-scanner
         (concatenate
           'string
           "\\Q"
           list-sep
           "\\E"))))
    (loop
	  for key being the hash-key of environment
	  using (hash-value
		 value)
	  do
	  (cl-ppcre:register-groups-bind
           ((#'string-keyword
             ktag)
            (#'string-keyword
             kopt))
           (var-pattern
            key)
           (ingest-var
             program-name
             result
             map-sep-pat
             list-sep-pat
             hash-init-args
             ktag
             kopt
             value)))
    result))

(defun
    get-options-from-os
    (args
      environment
      cli-aliases
      env-aliases
      defaults
      functions
      list-sep
      map-sep
      program-name
      setup
      teardown
      hash-init-args
  "
  Get arguments from FS, Environment (provided), and arguments (provided).
  "
  (let ((result (apply #'make-hash-table hash-init-args))
        (cli-options (consume-arguments args :hash-init-args hash-init-args))
        (env-options (consume-environment env :hash-init-args hash-init-args))
        (config-files
          (list
            (let (app-data (gethash "AppData" environment))
              (when (not (null app-data))
                (probe-file
                  (merge-pathnames 
;; use `(make-pathname :directory '(:relative <things>))` to build direcotories
;; and `(uiop:pathname-exists-p f)` to check for existence of file.




                 ;config-files
                 ;(reduce
                 ;into
                 ;()
                 ;((if-let
                 ;(app-data
                 ; (System/getenv
                 ;  "AppData"))
                 ;(or
                 ;(try-file
                 ;(app-data
                 ;i
                 ;program-name
                 ;"config.json"))
                 ;(try-file
                 ; (app-data
                 ;program-name
                 ;"config.yaml"))))
                 ;(if-let
                 ;  (home
                 ;   (System/getenv
                 ;    "HOME"))
                 ;(or
                 ;(try-file
                 ;  (home
                 ;(str
                 ;"."
                 ;program-name
                 ;".json")))
                 ;(try-file
                 ;   (home
                 ;(str
                 ;"."
                 ;program-name
                 ;".yaml"))))
                 ;(if-let
                 ;    (home
                 ;     (get
                 ;      properties "user.home"))
                 ;(or
                 ;(try-file
                 ;    (home
                 ;(str
                 ;"."
                 ;program-name
                 ;".json")))
                 ;(try-file
                 ;     (home
                 ;(str
                 ;"."
                 ;program-name
                 ;".yaml"))))))
                 ;(if-let
                 ;      (pwd
                 ;       (get
                 ;        properties "user.dir"))
                 ;(or
                 ;(try-file
                 ;(pwd
                 ;      (str
                 ;program-name
                 ;".json")))
                 ;(try-file
                 ;(pwd
                 ;       (str
                 ;program-name
                 ;".yaml")))))
                 ;(:config-files
                   ;        expanded-env)
                   ;(:config-files
                     ;       expanded-cli)))
                     ;expanded-cfg
                     ;(reduce
                     ;    merge
                     ;(map
                     ;    (fn
                     ;     (file)
                     ;(expand-option-packs
                     ;available-option-packs
                     ;(parse-string
                     ;(data-slurp
                     ;     file))))
                     ;config-files))
                     ;effective-options
                     ;(as->
                     ; (given-defaults
                     ;expanded-cfg
                     ;(dissoc
                     ;  expanded-env :config-files)
                     ;(dissoc
                     ; expanded-cli :config-files)) it
                     ;(mapv
                     ;(fn
                     ; (x)
                     ;(into
                     ; {}
                     ;(filter
                     ;#(not
                     ; (nil?
                     ;  (second
                     ;   %)))
                     ;x))) it)
                     ;(reduce
                     ;merge (hash-map)
                     ;it))
                     ;effective-commands
                     ;(if-let
                     ;(commands
                     ; (:commands
                        ;  effective-options))
                        ;commands
                        ;())
                        ;output-style (let
                                          ;(of
                                          ; (:output-format
                                             ;  effective-options))
                                        ;(cond
                                        ;(=
                                          ; of "json") :json
                                          ;(=
                                            ;of "yaml") :yaml
                                            ;(nil?
                                              ;of) :json
                                              ;:else (throw
                                                      ;(ex-info
                                                      ; (str
                                                      ;  "Invalid output format: "
                                                      ;of)
                                                      ;{:function ::go!
                                                      ;:parameters params})))))
                ;(if-let
                ;  (func
                ;   (get
                ;    effective-functions
                ;effective-commands))
                ;(try
                ;(let
                     ;    (ret
                     ;(if
                          ;     (and
                          ;(not
                          ;      (nil?
                          ;       setup))
                          ;(or
                          ;(empty?
                          ;     effective-commands)
                          ;(not
                          ;    (=
                          ;     (nth
                          ;      effective-commands (dec
                          ;(count
                          ;                          effective-commands)))
                          ;"help"))))
                          ;(func
                          ;    (setup
                          ;     effective-options))
                        ;(func
                        ;   effective-options)))
                   ;(when
                        ;(not
                        ;(:suppress-return-output
                          ; ret))
                      ;(println
                      ;(generate-string
                      ; (dissoc
                      ;  ret :onecli) output-style)))
                   ;(when
                        ;(not
                        ; (nil?
                        ;  teardown))
                      ;(teardown
                      ; effective-options))
                   ;(if-let
                   ;(return-code
                   ; (:exit-code
                      ;  (:onecli
                          ;   ret)))
                          ;return-code
                          ;0))
                          ;(catch
                          ;clojure.lang.ExceptionInfo e
                          ;(exit-error
                          ;(if-let
                          ;(code
                          ; (:exit-code
                             ;  (:onecli
                                 ;   (ex-data
                                 ;    e))))
                                 ;code
                                 ;128)
                                 ;(generate-string
                                 ;(as->
                                 ;(ex-data
                                 ; e) it
                                 ;(assoc
                                 ;it :error (str
                                 ;           e))
                                 ;(assoc
                                 ;it :stacktrace
                                 ;(stacktrace-string
                                 ;e output-style))
                                 ;(assoc
                                 ;it :given-options effective-options))
                                 ;output-style)))
                                 ;(catch
                                 ;Exception e
                                 ;(exit-error
                                 ;128
                                 ;(generate-string
                                 ;{:error (str
                                 ;e)
                                 ;:problem :unknown-problem
                                 ;:stacktrace (stacktrace-string
                                 ;e output-style)
                                 ;:given-options effective-options}
                                 ;output-style))))
                                 ;(exit-error
                                 ;1
                                 ;(generate-string
                                 ;{:error "Unknown command"
                                 ;:problem :unknown-command
                                 ;:given-options effective-options}
                                 ;output-style)))))
