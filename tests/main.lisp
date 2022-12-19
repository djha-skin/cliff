(in-package #:cl-user)

(defpackage #:cl-i/tests
  (:use #:cl
        #:rove)
  (:import-from
    #:cl-i))
(in-package :cl-i/tests)


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


(defmacro check-both-values (expr left right)
  "
  Takes an expression that returns two values. Checks that both
  of them are equal to the given `left` and `right values.
  "
  (let ((myl (gensym "left"))
        (myr (gensym "right")))
        `(multiple-value-bind
           (,myl ,myr)
           ,expr
           (ok (equal ,left (cl-i:dbg ,myl)))
           (ok (equal ,right (cl-i:dbg ,myr))))))

(defmacro check-partition (args left right)
  `(check-both-values (apply #'cl-i:partition ,args) ,left ,right))

(deftest
  partition
  (testing "basic-partition"
           (check-both-values (cl-i:partition nil 15) nil nil)
           (check-both-values (cl-i:partition nil 0) nil nil)
           (check-both-values (cl-i:partition '(1 2 3) 0) nil '(1 2 3))
           (check-both-values (cl-i:partition '(1 2 3) 3) '(1 2 3) nil)
           (check-both-values (cl-i:partition '(1 2 3) 15) '(1 2 3) nil))
  (testing "error-partition"
           (signals (cl-i:partition nil -1))))

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
  basic-find-file
  (testing "basic-find-file"
           (ok
             (equal
               #P"/home/djha-skin/Code/lisp/cl-i/tests/.cl-i.yaml"
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
          "Debug: Type of `(+ 3 3)` = `(INTEGER 0 4611686018427387903)`~%"
          "Debug: Eval of `(+ 3 3)` = `6`~%"))))))

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
      (merge-pathnames
        #P".cl-i.yaml"
        *tests-dir*))
  "hoo: haa"))))

(deftest
  slurp
  (let ((test-config-file
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
            (cl-i:os-specific-home #'uiop/os:getenv))))
    (testing
      "paths"
      (ok
        (equal
          (cl-i:data-slurp
            test-config-file)
          "hoo: haa")))
    (testing
      "file URL"
      (ok
        (equal
          (cl-i:data-slurp
            (concatenate 'string
                         "file://"
                         (namestring test-config-file)))
          "hoo: haa"))))
  (testing
    "noauth"
    (ok
      (equal
        (cl-i:data-slurp
          "https://djha-skin.me:8443/noauth/complete.txt"
          :insecure t)
        (format nil "noauth complete~%"))))

  (testing
    "basic"
    (ok
      (equal
        (cl-i:data-slurp
          "https://mode:code@djha-skin.me:8443/basic/complete.txt"
          :insecure t)
        (format nil "basic complete~%"))))
  (testing
    "header"
    (ok
      (equal
        (cl-i:data-slurp
          "https://Authorization=Bearer%20600dc0de6077a10ada600ddea10fda7a@djha-skin.me:8443/token/complete.txt"
          :insecure t)
        (format nil "token complete~%"))))
  (testing
    "token"
    (ok
      (equal
        (cl-i:data-slurp
          "https://600dc0de6077a10ada600ddea10fda7a@djha-skin.me:8443/token/complete.txt"
          :insecure t)
        (format nil "token complete~%")))))

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
