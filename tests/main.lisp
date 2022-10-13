(in-package #:cl-user)
(defpackage #:cl-i/tests
  (:use #:cl
        #:rove)
  (:import-from
    :cl-i))
(in-package #:cl-i/tests)

(defun positive-dec (n) (declare (number n)) (if (>= n 0) (- n 1) 0))

(deftest
  repeatedly-eq 
  (testing "basic test"
           (ok (equal (cl-i::repeatedly-eq #'positive-dec 3) '(3 2 1 0)))))
(deftest
  another
  (testing "Please work") 
  (ok (= 1 0)))

(deftest
  repeatedly
  (testing "error case"
  (ok (equal (cl-i::repeatedly #'positive-dec 3 (lambda (n) (<= n -3)))
             '(3 2 1 0 -1 -2 -3)))))
