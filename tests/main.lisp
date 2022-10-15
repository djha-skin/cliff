(in-package :cl-user)
(defpackage :cl-i/tests/main
  (:use :cl
        :fiveam)
  (:import-from
    :cl-i)
  (:import-from
    :cl-i/tests))
(in-package :cl-i/tests/main)


(defvar *tests-dir*
  (merge-pathnames
    #P"tests/"
    (slot-value
      (asdf:find-system "cl-i")
      'asdf/component:absolute-pathname)))

(def-suite cl-i-main
           :description "Main functions in test suite."
           :in cl-i/tests:cl-i)
(in-suite cl-i-main)

; For the REPL:
;(setf fiveam:*run-test-when-defined* t)

(defun positive-dec (n) (declare (number n)) (if (> n 0) (- n 1) 0))

(defun broken-dec (n) (declare (number n)) (if (>= n 0) (- n 1) 0))

(test
  repeatedly-eq
  (signals simple-error (cl-i:repeatedly-eq #'broken-dec 3))
  (is (equal (cl-i:repeatedly-eq #'positive-dec 3) '(3 2 1 0))))


(test
  repeatedly
  (signals simple-error (cl-i:repeatedly
                          #'positive-dec
                          3
                          (lambda (thing) (< thing 0))))
  (is (equal (cl-i:repeatedly
               #'positive-dec
               3
               (lambda (item)
                 (<= item 0)))
             '(3 2 1))))

(test
  basic-find-file
  (is
    (equal
      #P"/home/djha-skin/Development/lisp/cl-i/tests/.cl-i.yaml"
      (cl-i:find-file
        *tests-dir*
        "cl-i")))
  (is
    (equal
      (cl-i:find-file
        (merge-pathnames
          #P"/leaves-of-grass"
          (slot-value
            (asdf:find-system "cl-i")
            'asdf/component:absolute-pathname))
        "600dc0d36077a10ada600dd3a10fda7a")
      nil)))

(test
  dbg
  (is
    (equal
      (with-output-to-string (str)
        (cl-i:dbg (+ 3 3) str))
      (format
        nil
        (concatenate
          'string
          "Debug: Type of `(+ 3 3)` = `(INTEGER 0 4611686018427387903)`~%"
          "Debug: Eval of `(+ 3 3)` = `6`~%")))))

(test
  slurp-stream
  (is
    (equal
      (with-open-file
        (f
          (merge-pathnames
            #P".cl-i.yaml"
            *tests-dir*)
          :direction :input
          :external-format :utf-8)
        (cl-i:slurp-stream f))
      "hoo: haa")))



