#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)

(defpackage #:cl-i/tests
  (:use #:cl
        #:rove)
  (:import-from
    #:cl-i)
  (:import-from
    #:nrdl)
  (:import-from
    #:cl-ppcre))

(in-package :cl-i/tests)

(defparameter *test-config-file*
  (merge-pathnames
    (make-pathname
      :directory
      (list
        :relative
        "Code"
        "lisp"
        "cl-i"
        "tests")
      :name
      ".cl-i"
      :type
      "nrdl")
    (cl-i:os-specific-home #'uiop/os:getenv)))

(defparameter *tests-dir*
  (merge-pathnames
    #P"tests/"
    (slot-value
      (asdf:find-system "cl-i")
      'asdf/component:absolute-pathname)))

;(def-suite cl-i-main
;           :description "Main functions in test suite."
;           :in cl-i/tests:cl-i)
;(in-suite cl-i-main)

; For the REPL:
;(setf fiveam:*run-test-when-defined* t)

(defun positive-dec (n) (declare (number n)) (if (> n 0) (- n 1) 0))

(defun broken-dec (n) (declare (number n)) (if (>= n 0) (- n 1) 0))

(deftest
  repeatedly-eq
  (testing "repeatedly-eq"
  (signals (cl-i::repeatedly-eq #'broken-dec 3))
  (ok (equal (cl-i::repeatedly-eq #'positive-dec 3) '(3 2 1 0)))))

(deftest
  repeatedly
  (testing "repeatedly"
  (signals
    (cl-i::repeatedly
                          #'positive-dec
                          3
                          (lambda (thing) (< thing 0))))
  (ok (equal (cl-i::repeatedly
               #'positive-dec
               3
               (lambda (item)
                 (<= item 0)))
             '(3 2 1)))))


(deftest
  basic-find-file
  (testing "basic-find-file"
           (ok
             (equal
               (slot-value
                 (asdf:find-system "cl-i")
                 'asdf/component:absolute-pathname)
               (cl-i:find-file
                 *tests-dir*
                 "cl-i")))
           (ok
             (equal
               (cl-i:find-file
                 (merge-pathnames
                   #P"/leaves-of-grass"
                   (slot-value
                     (asdf:find-system "cl-i")
                     'asdf/component:absolute-pathname))
                 "600dc0d36077a10ada600dd3a10fda7a")
               nil))))

(deftest
  slurp-stream
  (testing "basic slurp-stream"
           (ok
             (equal
               (with-open-file
                 (f
                   (merge-pathnames
                     #P".cl-i.nrdl"
                     *tests-dir*)
                   :direction :input
                   :external-format :utf-8)
                 (cl-i:slurp-stream f))
               "{ \"hoo\" \"haa\" }"))))


(deftest
  base-slurp
  (testing "base-slurp"
           (ok
             (equal
               (cl-i::base-slurp
                 *test-config-file*)
               "{ \"hoo\" \"haa\" }"))))

(deftest
  slurp
  (testing
    "paths"
    (ok
      (equal
        (cl-i:data-slurp
          *test-config-file*)
        "{ \"hoo\" \"haa\" }")))
  (testing
    "file URL"
    (ok
      (equal
        (cl-i:data-slurp
          (concatenate 'string
                       "file://"
                       (namestring *test-config-file*)))
        "{ \"hoo\" \"haa\" }")))
  (testing
    "noauth"
    (ok
      (equal
        (cl-i:data-slurp
          "https://localhost:8443/noauth/complete.txt"
          :insecure t)
        (format nil "noauth complete"))))

  (testing
    "basic"
    (ok
      (equal
        (cl-i:data-slurp
          "https://mode:code@localhost:8443/basic/complete.txt"
          :insecure t)
        (format nil "basic complete"))))
  (testing
    "header"
    (ok
      (equal
        (cl-i:data-slurp
          "https://Authorization=Bearer%20600dc0de6077a10ada600ddea10fda7a@localhost:8443/token/complete.txt"
          :insecure t)
        (format nil "token complete"))))
  (testing
    "token"
    (ok
      (equal
        (cl-i:data-slurp
          "https://600dc0de6077a10ada600ddea10fda7a@localhost:8443/token/complete.txt"
          :insecure t)
        (format nil "token complete")))))


(deftest
  consume-arguments
  (testing
    "other-args"
    (signals
       (cl-i:consume-arguments
        '("--enable-dark-mode"
          "--reset-dark-mode"
          "--add-dark-mode"
          "crying"
          "--add-dark-mode"
          "firm"
          "well-done"
          "medium-well"
          "--join-my"
          "pride=hurt"
          "--join-my"
          "start=great"
          "--nrdl-fight"
          "15.0"
          "--nrdl-stride"
          "tests/.cl-i.nrdl")))
    (multiple-value-bind (opts other-args)
        (cl-i:consume-arguments
          '("--enable-dark-mode"
            "--reset-dark-mode"
            "--add-dark-mode"
            "crying"
            "--add-dark-mode"
            "firm"
            "well-done"
            "medium-well"
            "--join-my"
            "pride=hurt"
            "--join-my"
            "start=great"
            "--nrdl-fight"
            "15.0"
            "--file-stride"
            "tests/.cl-i.nrdl"))
      (ok (equal
            (nrdl:nested-to-alist opts)
            '((:DARK-MODE "firm" "crying")
             (:FIGHT . 15.0)
             (:MY (:PRIDE . "hurt")
                  (:START . "great"))
             (:STRIDE ("hoo" . "haa")))
            ))
      (ok (equal
            '("medium-well" "well-done")
            other-args))))
  (testing
    "empty"
    (ok
      (equal (cl-i:generate-string
               (cl-i:consume-arguments
                 '()))
             (format nil "{~%}"))))
  (testing
    "basic"
    (ok
      (equal (nrdl:nested-to-alist
               (cl-i:consume-arguments
                 '("--enable-foo" "--disable-bar" "baz" "--nrdl-force" "15" "--set-quux" "farquad")))
             '((:BAR) (:FOO . T) (:FORCE . 15) (:QUUX . "farquad"))))))

(deftest
  consume-environment
  (testing
    "Basic"
    (ok
      (equal
        '((:FINES ("key" . 155.5))
          (:FORESIGHT . T)
          (:FORKS . "whenceandwhither")
          (:MAPLE "1" "2" "3" "4" "5"))
        (nrdl:nested-to-alist
          (cl-i:consume-environment
            "hello"
            (alexandria:alist-hash-table
              '(("HELLO_LIST_MAPLE" . "1,2,3,4,5")
                ("HELLO_FANGLE_DOG" . "12345")
                ("VARS" . "xtreem")
                ("HELLO_NRDL_FINES" . "{ \"key\": 155.5 }")
                ("HELLO_FLAG_FORESIGHT" . "0")
                ("HELLO_ITEM_FORKS" . "whenceandwhither")))))))))

(defun blank-command
  (options)
  (format t "Options:~&  ~A~&" (nrdl:nested-to-alist options))
  (alexandria:alist-hash-table
    '((:status . :successful))))

(defun error-command
  (options)
  (format t "Options:~&  ~A~&" (nrdl:nested-to-alist options))
  (alexandria:alist-hash-table
    '((:status . :general-error))))

(defun io-error
  (options)
  (setf (gethash :status options) :input-output-error)
  options)

(deftest config-file-options
         (testing
           "typical invocation"
           (ok
             (equal
               (nrdl:nested-to-alist
                 (cl-i:config-file-options
                   "hi"
                   (alexandria:alist-hash-table
                     #+windows
                     '(
                       ("USERPROFILE" . "C:\\Users\\djh")
                       )
                     #-windows
                     '(
                       ("HOME" . "/home/djha-skin")
                       )
                     :test #'equal
                     )
                   (make-hash-table)))
               '((:CHIVES
                  (:LOVE . 15)
                  (:SORE_LOSERS
                   (:COOL . :BEANS)
                   (:STATE . "virginia"))
                  (:SPICES . T))
                 (:DOT . nil)
                 (:GARY . 3)
                 (:HAIRY . 4)
                 (:SLASH))))))

(deftest
  execute-program
  (testing
    "empty cases"
    (ok
      (signals
        (cl-i:execute-program
          "Halo"
          (make-hash-table)
          nil)
        'cl-i:necessary-env-var-absent)
      "Necessary environment variables absent")
    (ok
      (signals
        (cl-i:execute-program
          "Halo"
          (alexandria:alist-hash-table
            #+windows
            '(
              ("USERPROFILE" . "C:\\Users\\djh")
              )
            #-windows
            '(("HOME" . "/home/djha-skin")
              )
            :test #'equal)
          nil)
        'cl-i:invalid-subcommand)
      "No test provided in hash table args"))
  (testing
    "typical invocation"
    (multiple-value-bind (code outcome)
      (cl-i:execute-program
        "hi"
        (alexandria:alist-hash-table
          #+windows
          '(
            ("USERPROFILE" . "C:\\Users\\djh")
            ("HI_ITEM_FOUR" . "square")
            ("HI_LIST_LOVERS" . "so,many,lovers")
            ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
            )
          #-windows
          '(
          ("HOME" . "/home/djha-skin")
          ("HI_ITEM_FOUR" . "square")
          ("HI_LIST_LOVERS" . "so,many,lovers")
          ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
          )
        :test #'equal
        )
      `((nil . ,#'blank-command)
        (("error") . ,#'error-command)
        (("io-error") . ,#'io-error))
      :cli-arguments
      '(
        "--join-deals" "a=jhyghyjub"
        "--join-deals" "c=d"
        "--add-barf" "1"
        "--add-barf" "2"
        "--add-barf" "3"
        "--enable-gary"
        "--reset-gary"
        "--set-gary" "four"
        "--disable-all-the-things"
        "io-error"
        ))
    (ok (equal (nrdl:nested-to-alist outcome)

               '((:ALL-THE-THINGS) (:BARF "3" "2" "1")
                                   (:CHIVES (:LOVE . 15) (:SORE_LOSERS (:COOL . :BEANS) (:STATE . "virginia"))
                                            (:SPICES . T))
                                   (:DEALS (:A . "jhyghyjub") (:C . "d")) (:DOT . nil) (:FOUR . "square")
                                   (:GARY . "four") (:HAIRY . 4) (:LOVERS "so" "many" "lovers")
                                   (:OF (:CONTENT-MAKERS . "too-many") (:CONTENT-PEOPLE . "few")
                                        (:CONTENTS . "lots"))
                                   (:SLASH)
                                   (:STATUS . :INPUT-OUTPUT-ERROR)
                                   )))
    (ok (equal code 74)))))


(defmacro write-or-check-output (thing strm file expected actual)
  (let ((file-single-eval (gensym "write-or-check-output-file-"))
        ;; TODO lots of single evals here
    `(let* ((,file-single-eval ,file)
            (,expected
              (with-output-to-string (,strm)
                ,thing))
            (,actual (when (probe-file ,file-single-eval)
                      (uiop:read-file-string ,file-single-eval))))
       (if (not (null ,actual))
           (ok (equal ,expected ,actual))
           (with-open-file (,file-single-eval :direction :output :if-exists :supersede :external-format :utf-8)
             (write-string expected out))

         output))))

(defmacro write-or-check-nrdl (thing strm file expected actual)










(deftest
  default-help
  (testing
    "empty cases"
    (let ((strstrm (string-stream
    (with-output-to-string (out)
      (default-help
        out 
        "halo"
        nil
        nil
        nil
        (uiop/os:getcwd)
        nil 
        ","
        "="))
  (testing
    "typical case"
    (alexandria:hash-table-alist
      (cl-i::default-help
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


    (ok
      (signals
        (cl-i:execute-program
          "Halo"
          (make-hash-table)
          nil)
        'cl-i:necessary-env-var-absent)
      "Necessary environment variables absent")
    (ok
      (signals
        (cl-i:execute-program
          "Halo"
          (alexandria:alist-hash-table
            #+windows
            '(
              ("USERPROFILE" . "C:\\Users\\djh")
              )
            #-windows
            '(("HOME" . "/home/djha-skin")
              )
            :test #'equal)
          nil)
        'cl-i:invalid-subcommand)
      "No test provided in hash table args"))
  (testing
    "typical invocation"
    (multiple-value-bind (code outcome)
      (cl-i:execute-program
        "hi"
        (alexandria:alist-hash-table
          #+windows
          '(
            ("USERPROFILE" . "C:\\Users\\djh")
            ("HI_ITEM_FOUR" . "square")
            ("HI_LIST_LOVERS" . "so,many,lovers")
            ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
            )
          #-windows
          '(
          ("HOME" . "/home/djha-skin")
          ("HI_ITEM_FOUR" . "square")
          ("HI_LIST_LOVERS" . "so,many,lovers")
          ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
          )
        :test #'equal
        )
      `((nil . ,#'blank-command)
        (("error") . ,#'error-command)
        (("io-error") . ,#'io-error))
      :cli-arguments
      '(
        "--join-deals" "a=jhyghyjub"
        "--join-deals" "c=d"
        "--add-barf" "1"
        "--add-barf" "2"
        "--add-barf" "3"
        "--enable-gary"
        "--reset-gary"
        "--set-gary" "four"
        "--disable-all-the-things"
        "io-error"
        ))
    (ok (equal (nrdl:nested-to-alist outcome)

               '((:ALL-THE-THINGS) (:BARF "3" "2" "1")
                                   (:CHIVES (:LOVE . 15) (:SORE_LOSERS (:COOL . :BEANS) (:STATE . "virginia"))
                                            (:SPICES . T))
                                   (:DEALS (:A . "jhyghyjub") (:C . "d")) (:DOT . nil) (:FOUR . "square")
                                   (:GARY . "four") (:HAIRY . 4) (:LOVERS "so" "many" "lovers")
                                   (:OF (:CONTENT-MAKERS . "too-many") (:CONTENT-PEOPLE . "few")
                                        (:CONTENTS . "lots"))
                                   (:SLASH)
                                   (:STATUS . :INPUT-OUTPUT-ERROR)
                                   )))
    (ok (equal code 74)))))
