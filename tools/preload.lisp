;(asdf:load-system "qlot")
;(defvar cl-i-quicklisp
;  (uiop:native-namestring
;    (merge-pathnames #P".qlot/" (uiop/os:getcwd))))
;
;(setf (uiop/os:getenv "QUICKLISP_HOME") cl-i-quicklisp)
;(setf (uiop/os:getenv "CL_SOURCE_REGISTRY") (uiop:native-namestring (uiop/os:getcwd)))
;
;(setf ql:*quicklisp-home* cl-i-quicklisp)
(let* ((name "cl-i"))
  (handler-bind ((uiop/lisp-build:compile-file-error (lambda (condt) (declare (ignore condt)) (invoke-restart (find-restart 'accept)))))
    (handler-bind ((asdf/find-component:missing-component (lambda (condt)
                                                            (declare (ignore condt))
                                                            (invoke-restart (find-restart 'continue)))))
      (handler-bind ((error (lambda (condt)
                              (declare (ignore condt))
                              (invoke-restart (find-restart 'continue)))))
  (asdf:load-system name)
  (asdf:load-system (format nil "~A/tests" name))
  ))))