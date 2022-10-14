(in-package :cl-user)
(defpackage :cl-i/tests/main
  (:use :cl
        :fiveam)
  (:import-from
    :cl-i)
  (:import-from
    :cl-i/tests))
(in-package :cl-i/tests/main)

(def-suite cl-i-main
           :description "Main functions in test suite."
           :in cl-i/tests:cl-i)
(in-suite cl-i-main)

(defun positive-dec (n) (declare (number n)) (if (>= n 0) (- n 1) 0))

(test
  another
  (is (= 1 0)))

;(test
;  repeatedly-eq
;           (is (equal (cl-i::repeatedly-eq #'positive-dec 3) '(3 2 1 0))))
;
;(test
;  repeatedly
;  (testing "error case"
;  (is (equal (cl-i::repeatedly #'positive-dec 3 (lambda (n) (<= n -3)))
;             '(3 2 1 0 -1 -2 -3))))
