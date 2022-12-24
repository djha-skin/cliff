(asdf:load-system "qlot")
(defvar cl-i-quicklisp
  (uiop:native-namestring
    (merge-pathnames #P".qlot/" (uiop/os:getcwd))))

(setf (uiop/os:getenv "QUICKLISP_HOME") cl-i-quicklisp)

;(setf ql:*quicklisp-home* cl-i-quicklisp)
(let* ((name "cl-i"))
  (asdf:load-system name)
  (asdf:load-system (format nil "~A/tests" name)))