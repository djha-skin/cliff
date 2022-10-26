(in-package #:cl-user)
(defpackage #:cl-i
  (:use #:cl)
  (:documentation
   "
    Package that has a function, `start-cli`, which does the following:
    - Registers functions mapped to specific subcommands
    - Reads configuration files in standard locations
    - Reads environment variables according to spefic rules and from these
      rules constructs a hash table which is then passed to the subcommands.
    "
    )
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
    data-slurp
    dbg
    find-file
    generate-string
    parse-string
    repeatedly
    repeatedly-eq
    slurp-stream
    ))
(in-package #:cl-i)

(defun repeatedly-eq
  (func
    arg
    &optional
    (eeq #'equal)
    (repeats 256))
  "
  List consisting of calls to func using arg, then calling func using that
  result, etc.
  Stops when subsequent calls return equal.
  "
  (declare (integer repeats))
  (when (<= repeats 0)
    (error "Ran REPEATS times without terminating. Arg: ~A" arg))
  (let ((next (funcall func arg)))
    (if (funcall eeq next arg)
      (list arg)
      (cons arg (repeatedly-eq func next eeq (- repeats 1))))))

(defun repeatedly
  (func arg
    &optional
    (check-p (lambda (arg) (equal arg nil)))
    (repeats 256))
  "
  List consisting of calls to func on arg, then calling func on that result,
  etc.
  Doesn't stop until check-p returns t when given `arg`.
  "
  (declare (integer repeats))
  (labels ((helper (arg on)
                (when (<= on 0)
                  (error (concatenate
                           'string
                           "Ran ~A times without terminating.~%"
                           "  Final iteration arg: ~A~%")
                         repeats
                         arg))
                (if (funcall check-p arg)
                  nil
                  (cons arg (helper (funcall func arg)
                                    (- on 1))))))
    (helper arg repeats)))

; Get file
(defun find-file
  (from cmd-name)
    "
    Starting at the directory given,
    find the marking file.
    "
    (arrows:as->
      from it
      (repeatedly-eq #'uiop/pathname:pathname-parent-directory-pathname it)
      (mapcar (lambda (path)
                (merge-pathnames
                  path
                  (pathname (format nil ".~A.yaml" cmd-name))))
              it)
      (some #'uiop/filesystem:file-exists-p it)))

(defmacro dbg
  (body
    &optional
    (destination *standard-output*))
  (let ((rbody (gensym "cl-i-dbg"))
        (rdest (gensym "cl-i-dbg")))
    `(let ((,rbody ,body)
           (,rdest ,destination))
       (format ,rdest "Debug: Type of `~A` = `~A`~%" (quote ,body) (type-of ,rbody))
       (format ,rdest "Debug: Eval of `~A` = `~A`~%" (quote ,body) ,rbody)
       (finish-output ,rdest)
       ,rbody)))

(defun generate-string
  (thing)
  (yaml:with-emitter-to-string (emit)
                               (yaml:emit-pretty-as-document emit thing)))

(defun parse-string
  (thing)
  (yaml:parse thing))

(defun exit-error
  (status msg &optional (destination *standard-output*))
  (format destination "~A~%" msg)
  status)

(defun slurp-stream (f)
  (with-output-to-string (out)
    (loop do
          (let ((char-in (read-char f nil)))
            (if char-in
              (write-char char-in out)
              (return))))))

(defun base-slurp
  (loc)
  (let ((input (if (equal loc "-")
                *standard-input*
                loc)))
    (if (typep input 'stream)
           (slurp-stream input)
           (with-open-file (in-stream loc
                            :direction :input
                            :external-format :utf-8)
             (slurp-stream in-stream)))))

;;  Takes alist
;; Handles http.
;  :content-type 'application/json
(defun data-slurp
  (resource &rest more-args)
  "
  Slurp config, using specified options.

  If `:resource` is a URL, download the contents according to the following
  rules:
  - If it is of the form `http(s)://user:pw@url`, it uses basic auth;
  - If it is of the form `http(s)://header=val@url`, a header is set;
  - If it is of the form `http(s)://tok@url`, a bearer token is used;
  - If it is of the form `file://loc`, it is loaded as a normal file;
  - If it is of the form `-`, the data is loaded from standard input;
  - Otherwise, the data is loaded from the string or pathname as if it named
  a file.
  "
  (flet ((http-call
           (&rest args)
           (apply
             #'dex:get
              (concatenate
                'list
                args
                more-args
                '(:force-string t)))))
    (or
      (when (equal 'pathname (type-of resource))
        (base-slurp resource))
      (cl-ppcre:register-groups-bind
         (protocol username password rest-of-it)
         ("^(https?://)([^@:]+):([^@:]+)@(.+)$" resource)
         (http-call
           (concatenate
             'string
             protocol
             rest-of-it)
           :basic-auth
           (cons (quri:url-decode username) (quri:url-decode password))))
      (cl-ppcre:register-groups-bind
         (protocol header headerval rest-of-it)
         ("^(https?://)([^@=]+)=([^@=]+)@(.+)$" resource)
         (http-call
           (concatenate
             'string
             protocol
             rest-of-it)
           :headers
           (list (cons (quri:url-decode header) (quri:url-decode headerval)))))
      (cl-ppcre:register-groups-bind
         (protocol token rest-of-it)
         ("^(https?://)([^@]+)@(.+)" resource)
         (http-call
             (concatenate
               'string
               protocol
               rest-of-it)
             :headers
             (list
               (cons
                 "Authorization"
                 (format nil "Bearer ~A" (quri:url-decode token))))))
      (cl-ppcre:register-groups-bind
         ()
         ("^https?://.*$" resource)
         (http-call resource))

      (cl-ppcre:register-groups-bind
         (_ device-name path)
         ("^file://(([^/]+):)?/(.*)$" resource)
         (let ((path-components (cl-ppcre:split "/+" path)))
         (arrows:as->
           path-components *
           (cons :absolute *)
         (if device-name
           (list
             :device device-name
             :directory *)
           (list :directory *))
         (apply #'make-pathname *)
         (base-slurp *))))
      (base-slurp  resource))))

(defun string-keyword (str)
  (intern (string-upcase str) "KEYWORD"))

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
  "Actions that consume additional argument."
  )

(defparameter
  +single-arg-actions+
  '(
    :enable
    :disable
    :reset
    )
  "Actions that consume no additional arguments."
  )

(defun consume-arguments
  (args &optional &key hash-init-args (map-sep "="))
  (let ((map-sep-pat (cl-ppcre:create-scanner map-sep))
        (consumable (copy-list args))
        (opts (apply #'make-hash-table hash-init-args))
        (other-args nil))
    (loop
      while consumable do
      (let
        ((arg (first consumable))
         (rargs (rest consumable)))
        (or
          (cl-ppcre:register-groups-bind
            ((#'string-keyword kact)
             (#'string-keyword kopt))
            (+find-tag+ arg)
            (cond
              ((some (lambda (x) (eql kact x)) +single-arg-actions+)
               (if (eql kact :reset)
                 (remhash kopt opts)
                 (setf (gethash kopt opts) (eql kact :enable)))
               (setf consumable rargs))
              ((some (lambda (x) (eql kact x)) +multiple-arg-actions+)
               (when (not rargs)
                 (error
                   "Not enough arguments for action ~A on option ~A"
                   kact kopt))
               (let ((value (first rargs))
                     (rrargs (rest rargs)))
                 (cond
                   ((eql kact :set)
                    (setf (gethash kopt opts) value))
                   ((eql kact :add)
                    (setf (gethash kopt opts)
                          (cons arg (gethash kopt opts))))
                   ((eql kact :join)
                    (let ((sresults (cl-ppcre:split map-sep-pat value)))
                      (if (eql (length sresults) 2)
                        (destructuring-bind
                          (k v)
                          (when (not (gethash kopt opts))
                            (setf
                              (gethash kopt opts)
                              (apply #'make-hash-table hash-init-args)))
                          (setf (gethash
                                  k
                                  (gethash kopt opts))
                                v))
                        (error "Not a k/v pair, check map sep pattern: ~A"
                               arg))))
                   ((eql kact :yaml)
                    (setf (gethash kopt opts)
                          (parse-string arg)))
                   ((eql kact :file)
                    (setf (gethash kopt opts)
                          (parse-string (data-slurp value))))
                   (:else
                     (error "Uknown action ~A" kact)))
                 (setf consumable rrargs))))
            t)
          (progn
            (push arg other-args)
            (setf consumable rargs)))))
    (values opts other-args)))

(defun expand-aliases
  (aliases args)
  (mapcar
    (lambda (arg)
      (or (gethash arg aliases)
          arg))
    args))

;(defun parse-env-vars
;  ({:keys
;    (aliases
;     env-vars
;     list-sep
;     map-sep
;     program-name)
;    :or
;    {aliases
;     {}
;     map-sep
;     "="
;     list-sep
;     ","}})
;  (let (clean-name
;        (string/upper-case
;         (string/replace
;          program-name #"\W" "_"))
;        var-pattern
;        (re-pattern
;         (str
;          "\\Q"
;          clean-name
;          "\\E"
;          "_"
;          "(LIST|MAP|JSON|YAML|ITEM|FLAG)"
;          "_"
;          "(\\w+)"))
;        expanded-alias-env
;        (into (hash-map)
;              (map (fn ((k v))
;                     (if-let (other (get aliases k))
;                       (other v)
;                       (k v)))
;                   (filter (fn ((k _))
;                             (not (nil? (get aliases k))))
;                           env-vars)))
;        expanded-explicit-env
;        (filter (fn ((k _)) (nil? (get aliases k))) env-vars)
;        list-sep-pat
;        (re-pattern
;         (str
;          "\\Q"
;          list-sep
;          "\\E"))
;        map-sep-pat
;        (re-pattern
;         (str
;          "^((^\\Q"
;          map-sep
;          "\\E)+)\\Q"
;          map-sep
;          "\\E(.*)$")))
;    (letfn ((process
;              (e)
;              (reduce (fn (m (k v))
;                        (if-let ((_ label clean-opt)
;                                 (re-matches
;                                  var-pattern
;                                  k))
;                          (let (kopt (keyword (string/lower-case (string/replace clean-opt #"_" "-")))
;                                kabel (keyword (string/lower-case label)))
;                            (assoc m kopt
;                                   (cond
;                                     (= kabel :flag)
;                                     (let (ins (string/lower-case v))
;                                       (cond
;                                         (or (= ins "1")
;                                             (= ins "yes")
;                                             (= ins "true"))
;                                         true
;                                         (or (= ins "0")
;                                             (= ins "no")
;                                             (= ins "false"))
;                                         false
;                                         :else
;                                         (throw
;                                          (ex-info
;                                           "environment variable value not recognized as a boolean value"
;                                           {:function :parse-env-vars
;                                            :option kopt
;                                            :label kabel
;                                            :var k
;                                            :val v}))))
;                                     (= kabel :item)
;                                     v
;                                     (or (= kabel :json)
;                                         (= kabel :json))
;                                     (parse-string v)
;                                     (= kabel :map)
;                                     (into
;                                      {}
;                                      (map
;                                       (fn (i)
;                                         (if-let ((_ k v) (re-matches map-sep-pat i))
;                                           ((keyword k) v)
;                                           (throw (ex-info "no key value pairs detected"
;                                                           {:item i}))))
;                                       (string/split v list-sep-pat)))
;                                     (= kabel :list)
;                                     (string/split v list-sep-pat)
;                                     :else
;                                     (throw
;                                      (ex-info
;                                       "nothing makes sense in this world"
;                                       {:function ::parse-env-vars
;                                        :option kopt
;                                        :label kabel
;                                        :var k
;                                        :val v})))))
;                          m))
;                      (hash-map)
;                      e)))
;      (merge
;       (process expanded-alias-env)
;       (process expanded-explicit-env)))))
;
;(defun- expand-option-packs
;  (available-option-packs options)
;  (as-> (:option-packs options) it
;    (mapv available-option-packs it)
;    (into {} it)
;    (merge it options)
;    (dissoc it :option-packs)))
;
;(defun display-config!
;  "
;  Displays the effective configuration as provided either by environment
;  variables, by CLI arguments, or by configuration files.
;  "
;  (options)
;  options)
;
;(defun try-file
;  (fparts)
;  (let (f (io/file
;           (string/join
;            java.io.File/separator
;            fparts)))
;    (when (.exists f)
;      (f))))
;
;(defun- stacktrace-string
;  "Gives back the string of the stacktrace of the given exception."
;  (^Exception e output-style)
;  (let (s (java.io.StringWriter.)
;        p (java.io.PrintWriter. s))
;    (.printStackTrace e p)
;    (let (stackstring (str s))
;      (if (= output-style :yaml)
;        ;; tabs in string really throws off yaml, doesn't look nice
;        (string/replace stackstring "\t" "    ")
;        stackstring))))
;
;
;(defun help-meta-var
;  (arg)
;  #_{:clj-kondo/ignore (:missing-else-branch)}
;  (if-let ((_ action _)
;           (re-matches
;            #"--(assoc|add|set|yaml|json|file)-(.+)"
;            arg))
;    (let (kact (keyword action))
;      (cond
;        (= kact :assoc)
;        "<k>=<v>"
;        (or
;         (= kact :set)
;         (= kact :add))
;        "<value>"
;        (= kact :json)
;        "<inline-json-blob>"
;        (= kact :yaml)
;        "<inline-yaml-blob>"
;        (= kact :file)
;        "<yaml-file>"))))
;
;(defun print-help-screen
;  (program-name
;   subcommands-list
;   aliases
;   defaults
;   _)
;  (println
;   (string/join \newline
;                (reduce
;                 into
;                 (((str "Welcome to `" program-name "` !")
;                   ""
;                   "A subcommand MUST be given."
;                   ""
;                   "Available subcommands:"
;                   "")
;                  (map (fn (cmds) (str "  - `" (string/join \space cmds) "`"))
;                       (sort-by
;                        (fn (lst) (string/join \space lst))
;                        subcommands-list))
;                  (""
;                   "All options can be used by all subcommands,"
;                   "Though any given subcommand may not use all the options."
;                   "Options can be given in any order, before, interspersed with or after"
;                   "the subcommands."
;                   ""
;                   "Options:"
;                   "")
;                  (map (fn ((small normal))
;                         (if-let (metavar (help-meta-var normal))
;                           (format "  - %-26s or %-46s"
;                                   (str "`" small " " metavar "`")
;                                   (str "`" normal " " metavar "`"))
;                           (format "  - %-26s or %-46s"
;                                   (str "`" small "`")
;                                   (str "`" normal "`"))))
;                       (sort-by
;                        (fn ((_ normal)) (string/replace normal #"^--(a-z)+-" ""))
;                        aliases))
;                  (if (empty? defaults)
;                    ()
;                    (into
;                     (""
;                      "Default settings for some of the options:"
;                      "")
;                     (map (fn ((opt v))
;                            (str "  - `" (name opt) "` = `" v "`"))
;                          defaults)))
;                  (""
;                   "This command uses OneCLI. All options can be set via environment"
;                   "variable, config file, or command line. See the OneCLI docs for"
;                   "more information:"
;                   ""
;                   "https://onecli.readthedocs.io"
;                   ""
;                   "Exit codes:"
;                   "0     Yee-Haw"
;                   "1     It's Not Me, It's You"
;                   "2     It's Not You, It's Me"
;                   "3     It's Something Else, Don't Shoot The Messenger"
;                   "128   I Have No Idea What Happened")))))
;  {:suppress-return-output true})
;
;(defun go!
;  ({:keys (args
;           env
;           properties
;           cli-aliases
;           env-aliases
;           available-option-packs
;           defaults
;           functions
;           list-sep
;           map-sep
;           program-name
;           setup
;           teardown)
;    :as params
;    :or {
;         env
;         (System/getenv)
;         properties
;         (System/getProperties)
;         list-sep ","
;         map-sep "="
;         cli-aliases
;         {}
;         env-aliases
;         {}
;         available-option-packs
;         {}
;         defaults
;         {}
;         functions {}
;         setup nil
;         teardown nil}})
;  (let (base-defaults
;        {:output-format "json"}
;        given-defaults (merge base-defaults defaults)
;        base-functions
;        {("options" "show") 'onecli.core/display-config!
;         ("options") 'onecli.core/display-config!}
;        given-functions
;        (reduce
;         (fn make-help-screens
;           (m (c f))
;           (assoc (assoc m c (resolve f))
;                  (conj c "help")
;                  (fn (_)
;                    (println
;                     (str
;                      "Help page for `"
;                      (string/join
;                       " "
;                       (into (program-name)
;                             c))
;                      "`:"))
;                    (println
;                     (as-> f it
;                       (resolve it)
;                       (meta it)
;                       (:doc it)
;                       (string/trim-newline it)
;                       (string/split it #"\n")
;                       (map string/trim it)
;                       (string/join \newline it)))
;                    {:suppress-return-output true})))
;         {}
;         (merge base-functions functions))
;        effective-cli-aliases
;        (into
;         {"--help" "help"
;          "-?" "help"
;          "-h" "help"
;          "-o" "--set-output-format"}
;         cli-aliases)
;        help-screen-func
;        (partial print-help-screen
;                 program-name
;                 (into
;                  (keys given-functions)
;                  (("help")))
;                 effective-cli-aliases
;                 given-defaults)
;        effective-functions
;        (as-> given-functions fs
;          (if (contains? fs ())
;            fs
;            (assoc fs () help-screen-func))
;          (if (contains? fs ("help"))
;            fs
;            (assoc fs ("help") help-screen-func)))
;        cli-options
;        (parse-args (into params ((:args args)
;                                  (:aliases effective-cli-aliases)
;                                  (:list-sep list-sep)
;                                  (:map-sep map-sep))))
;        env-options
;        (parse-env-vars (into params ((:env-vars env)
;                                      (:aliases env-aliases)
;                                      (:list-sep list-sep)
;                                      (:map-sep map-sep))))
;        expanded-env (expand-option-packs available-option-packs env-options)
;        expanded-cli (expand-option-packs available-option-packs cli-options)
;        config-files
;        (reduce
;         into
;         ()
;         ((if-let (app-data (System/getenv "AppData"))
;            (or
;             (try-file (app-data
;                        program-name
;                        "config.json"))
;             (try-file (app-data
;                        program-name
;                        "config.yaml"))))
;          (if-let (home (System/getenv "HOME"))
;            (or
;             (try-file (home
;                        (str
;                         "."
;                         program-name
;                         ".json")))
;             (try-file (home
;                        (str
;                         "."
;                         program-name
;                         ".yaml"))))
;            (if-let (home (get properties "user.home"))
;              (or
;               (try-file (home
;                          (str
;                           "."
;                           program-name
;                           ".json")))
;               (try-file (home
;                          (str
;                           "."
;                           program-name
;                           ".yaml"))))))
;          (if-let (pwd (get properties "user.dir"))
;            (or
;             (try-file
;              (pwd (str
;                    program-name
;                    ".json")))
;             (try-file
;              (pwd (str
;                    program-name
;                    ".yaml")))))
;          (:config-files expanded-env)
;          (:config-files expanded-cli)))
;        expanded-cfg
;        (reduce merge
;                (map (fn (file)
;                       (expand-option-packs
;                        available-option-packs
;                        (parse-string
;                         (data-slurp file))))
;                     config-files))
;        effective-options
;        (as-> (given-defaults
;               expanded-cfg
;               (dissoc expanded-env :config-files)
;               (dissoc expanded-cli :config-files)) it
;          (mapv (fn (x)
;                  (into {}
;                        (filter
;                         #(not (nil? (second %)))
;                         x))) it)
;          (reduce merge (hash-map) it))
;        effective-commands
;        (if-let (commands (:commands effective-options))
;          commands
;          ())
;        output-style (let (of (:output-format effective-options))
;                       (cond (= of "json") :json
;                             (= of "yaml") :yaml
;                             (nil? of) :json
;                             :else (throw (ex-info (str "Invalid output format: "
;                                                        of)
;                                                   {:function ::go!
;                                                    :parameters params})))))
;    (if-let (func (get effective-functions
;                       effective-commands))
;      (try
;        (let (ret
;              (if (and
;                   (not (nil? setup))
;                   (or
;                    (empty? effective-commands)
;                    (not (= (nth effective-commands (dec
;                                                      (count effective-commands)))
;                            "help"))))
;                (func (setup effective-options))
;                (func effective-options)))
;          (when
;           (not (:suppress-return-output ret))
;            (println (generate-string (dissoc ret :onecli) output-style)))
;          (when (not (nil? teardown)) (teardown effective-options))
;          (if-let (return-code (:exit-code (:onecli ret)))
;            return-code
;            0))
;        (catch clojure.lang.ExceptionInfo e
;          (exit-error
;           (if-let (code (:exit-code (:onecli (ex-data e))))
;             code
;             128)
;           (generate-string
;            (as-> (ex-data e) it
;              (assoc it :error (str e))
;              (assoc it :stacktrace
;                     (stacktrace-string e output-style))
;              (assoc it :given-options effective-options))
;            output-style)))
;        (catch Exception e
;          (exit-error
;           128
;           (generate-string
;            {:error (str e)
;             :problem :unknown-problem
;             :stacktrace (stacktrace-string e output-style)
;             :given-options effective-options}
;            output-style))))
;      (exit-error
;       1
;       (generate-string
;        {:error "Unknown command"
;         :problem :unknown-command
;         :given-options effective-options}
;        output-style)))))
