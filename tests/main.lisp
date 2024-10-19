#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))

(in-package #:cl-user)

(defpackage #:com.djhaskin.cliff/tests
  (:use #:cl
        #:rove)
  (:import-from
    #:com.djhaskin.nrdl)
  (:import-from
    #:com.djhaskin.cliff)
  (:import-from
    #:cl-ppcre)
  (:local-nicknames
    (#:cliff #:com.djhaskin.cliff)
    (#:nrdl #:com.djhaskin.nrdl)))

(in-package #:com.djhaskin.cliff/tests)

(defparameter *tests-dir*
  (merge-pathnames
    #P"tests/"
    (slot-value
      (asdf:find-system "com.djhaskin.cliff")
      'asdf/component:absolute-pathname)))

(defparameter *test-config-file*
  (merge-pathnames
    *tests-dir*
    #P"./.cliff.nrdl"))

;(def-suite cliff-main
;           :description "Main functions in test suite."
;           :in cliff/tests:cliff)
;(in-suite cliff-main)

; For the REPL:
;(setf fiveam:*run-test-when-defined* t)

(defun positive-dec (n) (declare (number n)) (if (> n 0) (- n 1) 0))

(defun broken-dec (n) (declare (number n)) (if (>= n 0) (- n 1) 0))

(deftest
  repeatedly-eq
  (testing "repeatedly-eq"
  (signals (cliff::repeatedly-eq #'broken-dec 3))
  (ok (equal (cliff::repeatedly-eq #'positive-dec 3) '(3 2 1 0)))))

(deftest
  repeatedly
  (testing "repeatedly"
  (signals
    (cliff::repeatedly
                          #'positive-dec
                          3
                          (lambda (thing) (< thing 0))))
  (ok (equal (cliff::repeatedly
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
                 (asdf:find-system "com.djhaskin.cliff")
                 'asdf/component:absolute-pathname)
               (cliff:find-file
                 *tests-dir*
                 "cliff")))
           (ok
             (equal
               (cliff:find-file
                 (merge-pathnames
                   #P"/leaves-of-grass"
                   (slot-value
                     (asdf:find-system "com.djhaskin.cliff")
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
                     #P".cliff.nrdl"
                     *tests-dir*)
                   :direction :input
                   :external-format :utf-8)
                 (cliff:slurp-stream f))
               "{ \"hoo\" \"haa\" }"))))


(deftest
  base-slurp
  (testing "base-slurp"
           (ok
             (equal
               (cliff::base-slurp
                 *test-config-file*)
               "{ \"hoo\" \"haa\" }"))))

(deftest
  slurp
  (testing
    "paths"
    (ok
      (equal
        (cliff:data-slurp
          *test-config-file*)
        "{ \"hoo\" \"haa\" }")))
  (testing
    "file URL"
    (ok
      (equal
        (cliff:data-slurp
          (concatenate 'string
                       "file://"
                       (namestring *test-config-file*)))
        "{ \"hoo\" \"haa\" }")))
  (testing
    "noauth"
    (ok
      (equal
        (cliff:data-slurp
          "https://localhost:8443/noauth/complete.txt"
          :insecure t)
        (format nil "noauth complete"))))

  (testing
    "basic"
    (ok
      (equal
        (cliff:data-slurp
          "https://mode:code@localhost:8443/basic/complete.txt"
          :insecure t)
        (format nil "basic complete"))))
  (testing
    "header"
    (ok
      (equal
        (cliff:data-slurp
          "https://Authorization=Bearer%20600dc0de6077a10ada600ddea10fda7a@localhost:8443/token/complete.txt"
          :insecure t)
        (format nil "token complete"))))
  (testing
    "token"
    (ok
      (equal
        (cliff:data-slurp
          "https://600dc0de6077a10ada600ddea10fda7a@localhost:8443/token/complete.txt"
          :insecure t)
        (format nil "token complete")))))


(deftest
  consume-arguments
  (testing
    "other-args"
    (multiple-value-bind (opts other-args)
        (cliff:consume-arguments
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
            "tests/.cliff.nrdl"))
      (ok (equal
            (nrdl:nested-to-alist opts)
            '((:DARK-MODE "firm" "crying")
             (:FIGHT . 15.0)
             (:MY (:PRIDE . "hurt")
                  (:START . "great"))
             (:STRIDE ("hoo" . "haa")))
            ))
      (ok (equal
            '("well-done" "medium-well")
            other-args)
          "Other-args handling of consume-arguments")))
  (testing
    "empty"
    (ok
      (equal (cliff:generate-string
               (cliff:consume-arguments
                 '()))
             (format nil "{~%}"))
      "Empty argument parsing"))
  (testing
    "basic"
    (ok
      (equal (nrdl:nested-to-alist
               (cliff:consume-arguments
                 '("--enable-foo" "--disable-bar" "baz" "--nrdl-force" "15" "--set-quux" "farquad")))
             '((:BAR) (:FOO . T) (:FORCE . 15) (:QUUX . "farquad")))
      "Basic argument parsing")))

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
          (cliff:consume-environment
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
                 (cliff:config-file-options
                   "hi"
                   (alexandria:alist-hash-table
                     #+windows
                     '(
                       ("USERPROFILE" . "C:\\Users\\djh")
                       )
                     #-windows
                     '(
                       ("HOME" . "/home/skin")
                       )
                     :test #'equal
                     )
                   (make-hash-table)
                   nil
                   (slot-value
                     (asdf:find-system "com.djhaskin.cliff")
                     'asdf/component:absolute-pathname)))
               '((:CHIVES
                  (:LOVE . 15)
                  (:SORE_LOSERS
                   (:COOL . :BEANS)
                   (:STATE . "virginia"))
                  (:SPICES . T))
                 (:DOT . nil)
                 (:GARY . 3)
                 (:HAIRY . 4)
                 (:SLASH . cl:null))))))

(deftest
  execute-program
  (testing
    "typical invocation"
    (multiple-value-bind (code outcome)
      (cliff:execute-program
        "hi"
          :environment-variables
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
          :subcommand-functions
          `(
            (("error") . ,#'error-command)
            (("io-error") . ,#'io-error))
          :default-function #'blank-command
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
      (list
        code
        (nrdl:nested-to-alist outcome))
    (ok (equal (nrdl:nested-to-alist outcome)

               '((:ALL-THE-THINGS) (:BARF "3" "2" "1")
                                   (:CHIVES (:LOVE . 15) (:SORE_LOSERS (:COOL . :BEANS) (:STATE . "virginia"))
                                            (:SPICES . T))
                                   (:DEALS (:A . "jhyghyjub") (:C . "d")) (:DOT . nil) (:FOUR . "square")
                                   (:GARY . "four") (:HAIRY . 4) (:LOVERS "so" "many" "lovers")
                                   (:OF (:CONTENT-MAKERS . "too-many") (:CONTENT-PEOPLE . "few")
                                        (:CONTENTS . "lots"))
                                   (:SLASH . cl:null)
                                   (:STATUS . :INPUT-OUTPUT-ERROR)
                                   ))
        "Typical invocation hash table check")
    (ok (equal code 74)
        "Typical invocation exit code check"))))

;;#(defmacro write-or-check-nrdl (thing strm file expected actual)
;;#
;;#
;;#
;;#
;;#
;;#
;;#
;;#
;;#
;;#
;;#(deftest
;;#  default-help
;;#  (testing
;;#    "empty cases"
;;#    (let ((strstrm (string-stream
;;#    (with-output-to-string (out)
;;#      (default-help
;;#        out 
;;#        "halo"
;;#        nil
;;#        nil
;;#        nil
;;#        (uiop/os:getcwd)
;;#        nil 
;;#        ","
;;#        "="))
;;#  (testing
;;#    "typical case"
;;#    (alexandria:hash-table-alist
;;#      (cliff::default-help
;;#        t
;;#        "halo"
;;#        (alexandria:alist-hash-table
;;#          '((:a . 1)
;;#            (:b . 2)
;;#            (:c . 3)))
;;#        '("hello" "world")
;;#        nil
;;#        (uiop/os:getcwd)
;;#        '(
;;#          (("hello" "world") . "
;;#                             This is nonsense.
;;#                             "))
;;#                             ","
;;#                             "="))
;;#
;;#
;;#    (ok
;;#      (signals
;;#        (cliff:execute-program
;;#          "Halo"
;;#          (make-hash-table)
;;#          nil)
;;#        'cliff:necessary-env-var-absent)
;;#      "Necessary environment variables absent")
;;#    (ok
;;#      (signals
;;#        (cliff:execute-program
;;#          "Halo"
;;#          (alexandria:alist-hash-table
;;#            #+windows
;;#            '(
;;#              ("USERPROFILE" . "C:\\Users\\djh")
;;#              )
;;#            #-windows
;;#            '(("HOME" . "/home/djha-skin")
;;#              )
;;#            :test #'equal)
;;#          nil)
;;#        'cliff:invalid-subcommand)
;;#      "No test provided in hash table args"))
;;#  (testing
;;#    "typical invocation"
;;#    (multiple-value-bind (code outcome)
;;#      (cliff:execute-program
;;#        "hi"
;;#        (alexandria:alist-hash-table
;;#          #+windows
;;#          '(
;;#            ("USERPROFILE" . "C:\\Users\\djh")
;;#            ("HI_ITEM_FOUR" . "square")
;;#            ("HI_LIST_LOVERS" . "so,many,lovers")
;;#            ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
;;#            )
;;#          #-windows
;;#          '(
;;#          ("HOME" . "/home/djha-skin")
;;#          ("HI_ITEM_FOUR" . "square")
;;#          ("HI_LIST_LOVERS" . "so,many,lovers")
;;#          ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
;;#          )
;;#        :test #'equal
;;#        )
;;#      `((nil . ,#'blank-command)
;;#        (("error") . ,#'error-command)
;;#        (("io-error") . ,#'io-error))
;;#      :cli-arguments
;;#      '(
;;#        "--join-deals" "a=jhyghyjub"
;;#        "--join-deals" "c=d"
;;#        "--add-barf" "1"
;;#        "--add-barf" "2"
;;#        "--add-barf" "3"
;;#        "--enable-gary"
;;#        "--reset-gary"
;;#        "--set-gary" "four"
;;#        "--disable-all-the-things"
;;#        "io-error"
;;#        ))
;;#    (ok (equal (nrdl:nested-to-alist outcome)
;;#
;;#               '((:ALL-THE-THINGS) (:BARF "3" "2" "1")
;;#                                   (:CHIVES (:LOVE . 15) (:SORE_LOSERS (:COOL . :BEANS) (:STATE . "virginia"))
;;#                                            (:SPICES . T))
;;#                                   (:DEALS (:A . "jhyghyjub") (:C . "d")) (:DOT . nil) (:FOUR . "square")
;;#                                   (:GARY . "four") (:HAIRY . 4) (:LOVERS "so" "many" "lovers")
;;#                                   (:OF (:CONTENT-MAKERS . "too-many") (:CONTENT-PEOPLE . "few")
;;#                                        (:CONTENTS . "lots"))
;;#                                   (:SLASH)
;;#                                   (:STATUS . :INPUT-OUTPUT-ERROR)
;;#                                   )))
;;#    (ok (equal code 74)))))
