(let* ((name "cl-i")
       (asds (list (concatenate 'string name ".asd"))))
  (asdf:load-system name)
  (asdf:load-system (format nil "~A/tests" name)))