#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)

(defpackage #:cl-i/tests
  (:use #:cl
        #:rove)
  (:import-from
    #:cl-i)
  (:import-from
    #:cl-ppcre))
(in-package :cl-i/tests)

(defvar *test-config-file*
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
      "yaml")
    (cl-i:os-specific-home #'uiop/os:getenv)))

(defvar *tests-dir*
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
  (signals (cl-i:repeatedly-eq #'broken-dec 3))
  (ok (equal (cl-i:repeatedly-eq #'positive-dec 3) '(3 2 1 0)))))

(deftest
  repeatedly
  (testing "repeatedly"
  (signals 
    (cl-i:repeatedly
                          #'positive-dec
                          3
                          (lambda (thing) (< thing 0))))
  (ok (equal (cl-i:repeatedly
               #'positive-dec
               3
               (lambda (item)
                 (<= item 0)))
             '(3 2 1)))))

(deftest
  nested-to-alist
  (testing "empty"
           (ok (equal nil (cl-i:nested-to-alist nil)))
           (ok (equal "" (cl-i:nested-to-alist ""))))
  (testing "atomic values"
           (ok (equal "hi" (cl-i:nested-to-alist "hi")))
           (ok (equal 15 (cl-i:nested-to-alist 15)))
           (ok (equal t (cl-i:nested-to-alist t)))
           (ok (equal 'a (cl-i:nested-to-alist 'a)))
           (ok (equal :b (cl-i:nested-to-alist :b))))
  (testing "typical invocations"
           (ok
             (equal
               (let
                 ((a (make-hash-table)))
                 (setf (gethash 'a a) 1)
                 (setf (gethash 'b a) 2)
                 (setf (gethash 'c a) 3)
                 (cl-i:nested-to-alist
                   `(1 2 3 (4 5) 6 (7 (8 ,a)))))
               '(1 2 3 (4 5) 6 (7 (8 ((A . 1) (B . 2) (C . 3)))))))
           (ok (equal
                 (let ((a (make-hash-table))
                       (b (make-hash-table)))
                   (setf (gethash :origin b) "thither")
                   (setf (gethash :destination b) "yon")
                   (setf (gethash 'a a) nil)
                   (setf (gethash 'b a) b)
                   (setf (gethash 'c a) '(1 2 3 4 5))
                   (cl-i:nested-to-alist a))
                 '((A)
                   (B 
                     (:DESTINATION . "yon")
                     (:ORIGIN . "thither")) (C 1 2 3 4 5))))))

(deftest
  hash-to-kw-hash
  (testing "basic"
           (equal (cl-i:nested-to-alist
                    (cl-i::hash-to-kw-hash
                      (alexandria:alist-hash-table
                        '(("c" . 3)
                          ("a" . 1)
                          ("b" . 2))
                        :test #'equal)))
                  '((:A . 1)
                    (:B . 2)
                    (:C . 3)))))


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
  dbg
  (testing
    "basic dbg"
    (let* ((tested-string
            (with-output-to-string (str)
              (cl-i:dbg (+ 3 3) str)))
          (tested-lines (cl-ppcre:split (format nil "~%") tested-string)))
      (ok (equal (length tested-lines) 2))
      (ok
        (cl-ppcre:scan
          "^debug: type of `\\(\\+ 3 3\\)` = `\\(INTEGER \\d+ \\d+\\)`$"
          (elt tested-lines 0)))
      (ok
        (equal
          "debug: eval of `(+ 3 3)` = `6`"
          (elt tested-lines 1))))))


(deftest
  slurp-stream
  (testing "basic slurp-stream"
           (ok
             (equal
               (with-open-file
                 (f
                   (merge-pathnames
                     #P".cl-i.yaml"
                     *tests-dir*)
                   :direction :input
                   :external-format :utf-8)
                 (cl-i:slurp-stream f))
               "hoo: haa"))))


(deftest
  base-slurp
  (testing "base-slurp"
  (ok
    (equal
      (cl-i::base-slurp
        *test-config-file*)
      "hoo: haa"))))

(deftest
  slurp
    (testing
      "paths"
      (ok
        (equal
          (cl-i:data-slurp
            *test-config-file*)
      "hoo: haa")))
    (testing
      "file URL"
      (ok
        (equal
          (cl-i:data-slurp
            (concatenate 'string
                         "file://"
                         (namestring *test-config-file*)))
          "hoo: haa")))
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
          "--yaml-fight"
          "15.0"
          "--yaml-stride"
          "tests/.cl-i.yaml")))
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
            "--yaml-fight"
            "15.0"
            "--file-stride"
            "tests/.cl-i.yaml"))
      (ok (equal
            (cl-i:nested-to-alist opts)
            '((:DARK-MODE "firm" "crying")
             (:FIGHT . 15.0)
             (:MY (:PRIDE . "hurt")
                  (:START . "great"))
             (:STRIDE (:HOO . "haa")))
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
             (cl-i:join-strings
               "---"
               ""))))
  (testing
    "basic"
    (ok
      (equal (cl-i:nested-to-alist
               (cl-i:consume-arguments
                 '("--enable-foo" "--disable-bar" "baz" "--yaml-force" "15" "--set-quux" "farquad")))
             '((:BAR) (:FOO . T) (:FORCE . 15) (:QUUX . "farquad"))))))

(deftest
  consume-environment
  (testing
    "Basic"
    (ok
      (equal
        '((:FINES (:KEY . 155.5d0))
          (:FORESIGHT . T)
          (:FORKS . "whenceandwhither")
          (:MAPLE "1" "2" "3" "4" "5"))
        (cl-i:nested-to-alist
          (cl-i:consume-environment
            "hello"
            (alexandria:alist-hash-table
              '(("HELLO_LIST_MAPLE" . "1,2,3,4,5")
                ("HELLO_FANGLE_DOG" . "12345")
                ("VARS" . "xtreem")
                ("HELLO_YAML_FINES" . "{ \"key\": 155.5 }")
                ("HELLO_FLAG_FORESIGHT" . "0")
                ("HELLO_ITEM_FORKS" . "whenceandwhither")))))))))

(defun blank-command
  (options)
  (format t "Options:~&  ~A~&" (cl-i:nested-to-alist options))
  (alexandria:alist-hash-table
    '((:status . :successful))))

(defun error-command
  (options)
  (format t "Options:~&  ~A~&" (cl-i:nested-to-alist options))
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
               (cl-i:nested-to-alist
                 (cl-i:config-file-options
                   "hi"
                   (alexandria:alist-hash-table
                     '(
                       ("HOME" . "/home/djha-skin")
                       )
                     :test #'equal)
                   (make-hash-table)))
               '((:CHIVES
                  (:LOVE . 15)
                  (:SORE_LOSERS
                   (:COOL . "beans")
                   (:STATE . "virginia"))
                  (:SPICES . T))
                 (:DOT)
                 (:GARY . 3)
                 (:HAIRY . 4))))))

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
            '(("HOME" . "/home/djha-skin"))
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
          '(
            ("HOME" . "/home/djha-skin")
            ("HI_ITEM_FOUR" . "square")
            ("HI_LIST_LOVERS" . "so,many,lovers")
            ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
            )
          :test #'equal)
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
      (ok (equal (cl-i:nested-to-alist outcome) 

                 '((:ALL-THE-THINGS) (:BARF "3" "2" "1")
                                    (:CHIVES (:LOVE . 15) (:SORE_LOSERS (:COOL . "beans") (:STATE . "virginia"))
                                             (:SPICES . T))
                                    (:DEALS (:A . "jhyghyjub") (:C . "d")) (:DOT) (:FOUR . "square")
                                    (:GARY . "four") (:HAIRY . 4) (:LOVERS "so" "many" "lovers")
                                    (:OF (:CONTENT-MAKERS . "too-many") (:CONTENT-PEOPLE . "few")
                                         (:CONTENTS . "lots"))
                                    (:STATUS . :INPUT-OUTPUT-ERROR))))
      (ok (equal code 74)))))
