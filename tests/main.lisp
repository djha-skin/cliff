(defpackage cl-i/tests/main
  (:use :cl
        :cl-i
        :rove))
(in-package :cl-i/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-i)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
