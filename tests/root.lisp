(in-package #:cl-user)
(defpackage :cl-i/tests
  (:use :cl
        :fiveam)
  (:import-from
    :cl-i)
  (:export cl-i))
(in-package :cl-i/tests)

(def-suite cl-i)