;;;; errors.lisp -- Deal with errors
;;; Daniel Haskin

#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:com.djhaskin.cl-i/errors
  (:use #:cl)
  (:documentation
    "
    Package that defines errors that CL-I knows the exit codes for.
    Also defines exit codes for well-known CL errors.

    These errors are intended to be used to define other conditions
    in programs calling CL-I.  The exit codes are defined in the
    `*exit-codes*` hash table.  The keys are the keywords that
    represent the errors and the values are the exit codes.

    The exit codes are taken from the /usr/include/sysexits.h file.
    ")
  (:import-from #:com.djhaskin.nrdl)
  (:local-nicknames
    (#:nrdl #:com.djhaskin.nrdl))
    (:export
      *exit-codes*
      exit-status
      exit-map-members
      exit-error))

(in-package #:com.djhaskin.cl-i/errors)

(defparameter *exit-codes*
  ;; taken from /usr/include/sysexit.h
  (alexandria:alist-hash-table
    '(
      (:unknown-error . 128)
      (:general-error . 1)
      (:successful . 0)
      (:cl-usage-error . 64)
      (:data-format-error . 65)
      (:no-input-error . 66)
      (:no-user-error . 67)
      (:no-host-error . 68)
      (:service-unavailable . 69)
      (:internal-software-error . 70)
      (:system-error . 71)
      (:os-file-error . 72)
      (:cant-create-file . 73)
      (:input-output-error . 74)
      (:temporary-failure . 75)
      (:protocol-error . 76)
      (:permission-denied . 77)
      (:configuration-error . 78))))

(defgeneric exit-status (condition)
  (:documentation
    "Return the exit code for the given condition."))

(defgeneric exit-map-members (condition)
  (:documentation
    "Return an alist of items to be added to the exit map of CL-I."))

(define-condition exit-error (error)
  ((exit-error-status :initarg :status
                      :reader exit-error-status :initform :unknown-error)
   (exit-error-map-members :initarg :map-members
                           :reader exit-error-map-members :initform nil))
  (:documentation
    "An ad-hoc error with which the user can specify exit code and any output
    map members.")
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream "Abnormal exit error~%"))))

(defmethod exit-status ((condition exit-error))
  (exit-error-status condition))

(defmethod exit-map-members ((condition exit-error))
  (exit-error-map-members condition))

;;; By default, we don't add anything to the exit map.
(defmethod exit-map-members ((condition condition))
    `(
      (:error-type . ,(prin1-to-string (type-of condition)))))

;;; We define exit codes for the standard CL conditions.
;;; Condition Type SERIOUS-CONDITION
(defmethod exit-status ((condition serious-condition))
  :general-error)

;;; Condition Type ARITHMETIC-ERROR
;;; Condition Type DIVISION-BY-ZERO
;;; Condition Type FLOATING-POINT-INVALID-OPERATION
;;; Condition Type FLOATING-POINT-OVERFLOW
;;; Condition Type FLOATING-POINT-UNDERFLOW
(defmethod exit-status ((condition arithmetic-error))
  :internal-software-error)

(defmethod exit-map-members ((condition arithmetic-error))
  (let ((operands (arithmetic-error-operands condition))
        (operation (arithmetic-error-operation condition)))
      `((:operands . ,(mapcar #'prin1-to-string operands))
        (:operation .
         ,(etypecase operation
            (list
              (mapcar #'prin1-to-string operation))
            (t
              (prin1-to-string operation)))))))

;;; Condition Type CELL-ERROR
;;; Condition Type UNBOUND-VARIABLE
;;; Condition Type UNDEFINED-FUNCTION
(defmethod exit-status ((condition cell-error))
  :internal-software-error)

(defmethod exit-map-members ((condition cell-error))
  (let ((name (cell-error-name condition)))
      `((:error-cell-name . ,(prin1-to-string name)))))

;;; Condition Type FILE-ERROR
(defmethod exit-status ((condition file-error))
  :input-output-error)

(defmethod exit-map-members ((condition file-error))
  (let ((pathname (file-error-pathname condition)))
      `((:error-pathname . ,(namestring pathname)))))

;;; Condition Type PACKAGE-ERROR
(defmethod exit-status ((condition package-error))
  :internal-software-error)

(defmethod exit-map-members ((condition package-error))
  (let ((package (package-error-package condition)))
      `((:error-package . ,(package-name package)))))

;;; Condition Type PARSE-ERROR
;;; Condition Type READER-ERROR
(defmethod exit-status ((condition parse-error))
  :data-format-error)

;;; Condition Type PRINT-NOT-READABLE
(defmethod exit-status ((condition print-not-readable))
  ;; This one is ambiguous. Can I not print because of a bad return value?
  ;; Or because the object is not printable?
  :internal-software-error)

(defmethod exit-map-members ((condition print-not-readable))
  (let ((object (print-not-readable-object condition)))
      `((:error-object . ,(prin1-to-string object)))))

;;; Condition Type PROGRAM-ERROR
(defmethod exit-status ((condition program-error))
  :internal-software-error)



;;; Condition Type TYPE-ERROR
(defmethod exit-status ((condition type-error))
  :data-format-error)

(defmethod exit-map-members ((condition type-error))
  (let ((datum (type-error-datum condition))
        (expected-type (type-error-expected-type condition)))
      `((:error-datum . ,(prin1-to-string datum))
        (:error-expected-type . ,(prin1-to-string expected-type)))))

;;; Condition Type STORAGE-CONDITION
(defmethod exit-status ((condition storage-condition))
  :system-error)

;;; Condition Type STREAM-ERROR
;;; Condition Type END-OF-FILE
(defmethod exit-status ((condition stream-error))
  :input-output-error)

(defmethod exit-map-members ((condition stream-error))
  (let ((stream (stream-error-stream condition)))
      `((:error-stream . ,(prin1-to-string stream)))))
