(in-package #:cl-user)

(defpackage #:cl-i/tests
  (:use #:cl
        #:rove)
  (:import-from
    #:cl-i))
(in-package :cl-i/tests)

(defun join
  (strs &key (fmt "~%"))
  (format nil
          (apply
            #'concatenate
            (cons 'string
          (mapcar (lambda (str) (concatenate 'string str fmt))
          strs)))))

(defun join-lines
  (&rest lines)
  (funcall #'join lines))

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
  (testing "repeatedly-eq"
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
               #P"/home/djha-skin/Development/lisp/cl-i/tests/.cl-i.yaml"
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
  (testing
    "paths"
    (ok
      (equal
        (cl-i:data-slurp
          #P"tests/.cl-i.yaml")
        "hoo: haa")))
  (testing
    "file"
    (ok
      (equal
        (cl-i:data-slurp
          "file:///home/djha-skin/Development/lisp/cl-i/tests/.cl-i.yaml")
        "hoo: haa")))
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
     (cl-i:generate-string
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
          ))
       )
      (ok (equal
            (join-lines
              "---"
              "DARK-MODE:"
              "- firm"
              "- crying"
              "MY:"
              "  pride: hurt"
              "  start: false"
              "FIGHT: 15.0"
              "SRIDE:"
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
             (join-lines
               "--- {}"
               "..."))))
  (testing
    "basic"
    (ok
      (equal (cl-i:generate-string
               (cl-i:consume-arguments
                 '("--enable-foo" "--disable-bar" "baz" "--set-quux" "farquad")))
             (join-lines
               "---"
               "FOO: true"
               "BAR: false"
               "QUUX: farquad"
               "...")))))
