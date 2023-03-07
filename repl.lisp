(cl-i:repeatedly
  #'positive-dec
  3
  (lambda (arg) (<= arg 0)))

(in-package #:cl-i)
(in-package #:cl-i/tests)

(defun positive-dec (n) (declare (number n)) (if (> n 0) (- n 1) 0))
(stable-sort '((b . 2) (a . 1) (c . 3)) #'string< :key (lambda (thing) (symbol-name (car thing))))

(defvar please `((nil . ,#'blank-command)
                            (("error") . ,#'error-command)
                            (("io-error") . ,#'io-error)))
