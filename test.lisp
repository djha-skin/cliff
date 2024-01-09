(defclass interface ()
  ((a :allocation :class :initarg :a)))

(defclass implementation (interface)
  ((a :allocation :class :initarg :a :initform
     (lambda (x y) (* x y)
       )
     )
   )
  )

(defparameter *i* (make-instance 'implementation))

(funcall (slot-value *i* 'a) 1 2)