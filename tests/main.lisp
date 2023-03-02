(in-package #:cl-user)
(defpackage #:cl-i/tests
  (:use #:cl
        #:rove)
  (:import-from
    #:cl-i))
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
  (signals (cl-i:repeatedly
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
                  (B (:ORIGIN . "thither")
                     (:DESTINATION . "yon")) (C 1 2 3 4 5))))))

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
  (testing "basic dbg"
  (ok
    (equal
      (with-output-to-string (str)
        (cl-i:dbg (+ 3 3) str))
      (format
        nil
        (concatenate
          'string
          "debug: type of `(+ 3 3)` = `(INTEGER 0 4611686018427387903)`~%"
          "debug: eval of `(+ 3 3)` = `6`~%"))))))

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
          "tests/.cl-i.yaml"
          )
       )
      (ok (equal
            (cl-i:join-lines
              "---"
              "DARK-MODE:"
              "- firm"
              "- crying"
              "MY:"
              "  pride: hurt"
              "  start: great"
              "FIGHT: 15.0"
              "STRIDE:"
              "  hoo: haa"
              "...")
            (cl-i:generate-string
              opts)))
      (ok (equal
            '("medium-well" "well-done")
            other-args))))
  (testing
    "empty"
    (ok
      (equal (cl-i:generate-string
               (cl-i:consume-arguments
                 '()))
             (cl-i:join-lines
               "--- {}"
               "..."))))
  (testing
    "basic"
    (ok
      (equal (cl-i:generate-string
               (cl-i:consume-arguments
                 '("--enable-foo" "--disable-bar" "baz" "--yaml-force" "15" "--set-quux" "farquad")))
             (cl-i:join-lines
               "---"
               "FOO: true"
               "BAR: false"
               "QUUX: farquad"
               "FORCE: 15"
               "...")))))
(deftest
  consume-environment
  (testing
    "Basic"
    (ok
      (equal
        (cl-i:join-lines
          "---"
          "MAPLE:"
          "- 5"
          "- 4"
          "- 3"
          "- 2"
          "- 1"
          "FINES:"
          "  key: 155.2"
          "FORESIGHT: true"
          "FORKS: whenceandwhither"
          "..."
          )
  (cl-i:generate-string (cl-i:consume-environment
    "hello"
    (alexandria:alist-hash-table
      '(("HELLO_LIST_MAPLE" . "1,2,3,4,5")
        ("HELLO_FANGLE_DOG" . "12345")
        ("VARS" . "xtreem")
        ("HELLO_YAML_FINES" . "{ 'key': 155.2 }")
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
    '((:status :general-error))))

(defun io-error
  (options)
  (format t "Options:~&  ~A~&" (cl-i:nested-to-alist options))
  (alexandria:alist-hash-table
    '((status :input-output-error))))

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
                       ("HI_ITEM_FOUR" . "square")
                       )
                     :test #'equal)
                   (make-hash-table)))
               '(("gary" . 3)
                ("hairy" . 4)
                ("dot")
                ("chives"
                 ("spices" . "yes")
                 ("love" . 15)
                 ("sore_losers"
                  ("cool" . "beans")
                  ("state" . "virginia"))))))))


(cl-i:execute-program
  "hi"
  (alexandria:alist-hash-table
    '(
      ("HOME" . "/home/djha-skin")
      ("HI_ITEM_FOUR" . "square")
      )
    :test #'equal)
  `((nil . ,#'blank-command)
    ("error" . ,#'error-command)
    ("io-error" . ,#'io-error))
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
    ))




(deftest
  execute-program
  (testing
    "empty cases"
    (ok
      (signals
        (cl-i:execute-program
          "Halo"
          nil
          nil)
        'cl-i:necessary-env-var-absent)
      "Necessary environment variables absent")
    (ok
      (signals
        (cl-i:execute-program
          "Halo"
          nil
          (make-hash-table)
          (alexandria:alist-hash-table
            '(("HOME" . "/home/djha-skin"))
            :test #'equal)
          (make-hash-table)
          (make-hash-table))
        'cl-i:invalid-subcommand)
      "No test provided in hash table args")))
;;  (testing
;;    "typical case"
;;    (ok (eql 0
;;             (cl-i:execute-program
;;               "hi"
;;               '("-i" "fish.txt" "-o" "{\"foo\": 3}" "--" "ugly" )
;;               (alexandria:alist-hash-table
;;                 '(("-i" . "--set-i")
;;                   ("-o" . "--json-o")
;;
;;
;;
;;(defparameter *exit-codes*
;;  ;; taken from /usr/include/sysexit.h
;;  (alexandria:alist-hash-table
;;    '((:successful . 0)
;;      (:general-error . 1)
;;      (:cl-usage-error . 64)
;;      (:data-format-error . 65)
;;      (:cannot-open-input . 66)
;;      (:addressee-unknown . 67)
;;      (:hostname-unknown . 68)
;;      (:service-unavailabe . 69)
;;      (:internal-software-error . 70)
;;      (:system-error . 71)
;;      (:os-file-missing . 72)
;;      (:cant-create-uof . 73)
;;      (:input-output-error . 74)
;;      (:temporary-failure . 75)
;;      (:remote-error-in-protocol . 76)
;;      (:permission-denied . 77)
;;      (:configuration-error . 78))))
