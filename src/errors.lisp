#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:cl-i/errors
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
    (:export
      *exit-codes*
      exit-code
      cl-i-error
      general-error
      error-message
      cl-usage-error
      invalid-arguments
      data-format-error
      option
      incorrect-data
      reason-incorrect
      no-input-error
      input-resource
      no-user-error
      absent-user
      no-host-error
      absent-host
      service-unavailable
      unavailable-service
      internal-software-error
      software-component
      internal-error
      system-error
      operating-system-error
      os-file-error
      system-file
      cant-create-file
      output-file
      input-output-error
      io-file
      io-problem
      temporary-failure
      transient-error
      protocol-error
      messaging-server
      messaging-response
      messaging-problem
      permission-denied
      denied-user
      denied-destination
      denied-operation
      configuration-error
      config-location
      config-error))
(in-package #:cl-i/errors)

(defparameter *exit-codes*
  ;; taken from /usr/include/sysexit.h
  (alexandria:alist-hash-table
    '(
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

(defgeneric exit-code (condition)
  (:documentation
    "Return the exit code for the given condition."))

(define-condition cl-i-error (error)
  (:documentation "Condition defined to describe CLI program errors.")
  (:report (lambda (this strm)
             (format strm
                     "An error occurred the CLI application.~&"))))

;; There is no /usr/include/sysexits.h entry for general errors.
;; The error is just more or less a standard at this point.
;; Fields needed:
;; - an arbitrary message.
(define-condition general-error (cl-i-error)
  ((error-message :initarg :error-message
            :type string
            :initform (error "Please specify a message.")
            :reader error-message))
  (:documentation "General CLI program error.")
  (:report (lambda (this strm)
             (format strm
                     "Error: ~A~&"
                     (error-message this)))))

(defmethod exit-code ((condition general-error))
  (gethash :general-error *exit-codes*))

;; /usr/include/sysexits.h entry:
;;
;;    EX_USAGE -- The command was used incorrectly, e.g., with
;;        the wrong number of arguments, a bad flag, a bad
;;        syntax in a parameter, or whatever.
;;
;; How CL-I uses it: subcommand was incorrect.
;; Fields needed:
;; - a list of invalid arguments.
(define-condition cl-usage-error (cl-i-error)
  ((invalid-arguments :initarg :invalid-arguments
                      :type list
                      :initform (error "Please specify the invalid arguments.")
            :reader invalid-arguments))
  (:documentation "The command was used incorrectly.")
  (:report (lambda (this strm)
             (format strm
                     "Invalid command line arguments: ~{~A~^ ~}~&"
                     (invalid-arguments this)))))

(defmethod exit-code ((condition cl-usage-error))
  (gethash :cl-usage-error *exit-codes*))

;; /usr/include/sysexits.h entry:
;;
;;    EX_DATAERR -- The input data was incorrect in some way.
;;        This should only be used for user's data & not
;;        system files.
;; How CL-I uses it: supplied option was incorrect.
;; Fields needed:
;; - The option that was in the wrong format.
;; - The incorrectly given data.
;; - A reason for why the data didn't work
(define-condition data-format-error (cl-i-error)
  ((option :initarg :option
            :type string
            :initform (error "Please specify the option.")
            :reader option)
   (incorrect-data :initarg :incorrect-data
                   :type string
                   :initform (error "Please specify the incorrect data.")
            :reader incorrect-data)
   (reason-incorrect :initarg :reason-incorrect
            :type string
            :initform (error "Please specify a reason.")
            :reader reason-incorrect))
  (:documentation "The input data was incorrect in some way.")
  (:report (lambda (this strm)
             (format strm "~@{~@?~}"
                     "Option `~A` was given in the wrong format.~&"
                     (option this)
                     "Given value: ~A~&"
                     (incorrect-data this)
                     "Reason: ~A~&"
                     (reason-incorrect this)))))

(defmethod exit-code ((condition data-format-error))
  (gethash :data-format-error *exit-codes*))

;; /usr/include/sysexits.h entry:
;;
;;    EX_NOINPUT -- An input file (not a system file) did not
;;        exist or was not readable.  This could also include
;;        errors like "No message" to a mailer (if it cared
;;        to catch it).
;; How CL-I uses it: a file or URI Resource was not found.
;; Fields needed:
;; - The file or URI Resource that was not found.
(define-condition no-input-error (cl-i-error)
  ((input-resource :initarg :input-resource
             :type string
             :initform (error "Please specify the resource.")
            :reader input-resource))
  (:documentation "An input file or URI did not exist or was not readable.")
  (:report (lambda (this strm)
             (format strm
                     "Resource `~A` not found.~&"
                     (input-resource this)))))

(defmethod exit-code ((condition no-input-error))
  (gethash :no-input-error *exit-codes*))

;; /usr/include/sysexits.h entry:
;;    EX_NOUSER -- The user specified did not exist.  This might
;;        be used for mail addresses or remote logins.
;; How CL-I uses it: the user specified for some login operation or other did
;; not exist.
;; Fields needed:
;; - The user that was not found.
(define-condition no-user-error (cl-i-error)
  ((absent-user :initarg :absent-user
         :type string
         :initform (error "Please specify the user.")
            :reader absent-user))
  (:documentation "The user specified did not exist.")
  (:report (lambda (this strm)
             (format strm
                     "User `~A` not found.~&"
                     (absent-user this)))))

(defmethod exit-code ((condition no-user-error))
  (gethash :no-user-error *exit-codes*))


;; /usr/include/sysexits.h entry:
;;    EX_NOHOST -- The host specified did not exist.  This is used
;;        in mail addresses or network requests.
;; How CL-I uses it: the host specified for some network operation or other did
;; not exist.
;; Fields needed:
;; - The host that was not found.
(define-condition no-host-error (cl-i-error)
  ((absent-host :initarg :absent-host
         :type string
         :initform (error "Please specify the host.")
            :reader absent-host))
  (:documentation "The host specified did not exist.")
  (:report (lambda (this strm)
             (format strm
                     "Host `~A` not found.~&"
                     (absent-host this)))))

(defmethod exit-code ((condition no-host-error))
    (gethash :no-host-error *exit-codes*))


;; /usr/include/sysexits.h entry:
;;    EX_UNAVAILABLE -- A service is unavailable.  This can occur
;;        if a support program or file does not exist.  This
;;        can also be used as a catchall message when something
;;        you wanted to do doesn't work, but you don't know
;;        why.
;; How CL-I uses it: a service is unavailable.
;; Fields needed:
;; - The service that is unavailable.
(define-condition service-unavailable (cl-i-error)
  ((unavailable-service :initarg :unavailable-service
            :type string
            :initform (error "Please specify the service.")
            :reader unavailable-service))
  (:documentation "A service is unavailable.")
  (:report (lambda (this strm)
             (format strm
                     "Service `~A` unavailable.~&"
                     (unavailable-service this)))))

(defmethod exit-code ((condition service-unavailable))
  (gethash :service-unavailable *exit-codes*))

;; /usr/include/sysexits.h entry:
;;    EX_SOFTWARE -- An internal software error has been detected.
;;        This should be limited to non-operating system related
;;        errors as much as possible.
;; How CL-I uses it: an internal software error has been detected.
;; Fields needed:
;; - The software component or program that had the error.
;; - The error that was detected.
(define-condition internal-software-error (cl-i-error)
  ((software-component :initarg :software-component
              :type string
              :initform (error "Please specify the component.")
            :reader software-component)
   (internal-error :initarg :internal-error
          :type string
          :initform (error "Please specify the error.")
            :reader internal-error))
  (:documentation "An internal software error has been detected.")
  (:report (lambda (this strm)
             (format strm
                     "Internal software error in `~A`: ~A~&"
                     (software-component this)
                     (internal-error this)))))

(defmethod exit-code ((condition internal-software-error))
  (gethash :internal-software-error *exit-codes*))

;; /usr/include/sysexits.h entry:
;;    EX_OSERR -- An operating system error has been detected.
;;        This is intended to be used for such things as "cannot
;;        fork", "cannot create pipe", or the like.  It includes
;;        things like getuid returning a user that does not
;;        exist in the passwd file.
;; How CL-I uses it: an operating system error has been detected.
;; Fields needed:
;; - The operating system error that was detected.
(define-condition system-error (cl-i-error)
  ((operating-system-error :initarg :operating-system-error
                 :type string
                 :initform (error "Please specify the system error.")
            :reader operating-system-error))
  (:documentation "An operating system error has been detected.")
  (:report (lambda (this strm)
             (format strm
                     "Operating system error: ~A~&"
                     (operating-system-error this)))))

(defmethod exit-code ((condition system-error))
  (gethash :system-error *exit-codes*))

;; /usr/include/sysexits.h entry:
;;    EX_OSFILE -- Some system file (e.g., /etc/passwd, /etc/utmp,
;;        etc.) does not exist, cannot be opened, or has some
;;        sort of error (e.g., syntax error).
;; How CL-I uses it: a system file does not exist, cannot be opened, or has
;; some sort of error (e.g., syntax error).
;; Fields needed:
;; - The system file in question.
(define-condition os-file-error (cl-i-error)
  ((system-file :initarg :system-file
                :type string
                :initform (error "Please specify the system file.")
            :reader system-file))
  (:documentation "Problem opening or reading a system file.")
  (:report (lambda (this strm)
             (format strm
                     "Problem opening or reading system file `~A`.~&"
                     (system-file this)))))

(defmethod exit-code ((condition os-file-error))
  (gethash :os-file-error *exit-codes*))


;; /usr/include/sysexits.h entry:
;;    EX_CANTCREAT -- A (user specified) output file cannot be
;;        created.
;; How CL-I uses it: a (user specified) output file cannot be created.
;; Fields needed:
;; - The output file that cannot be created.
(define-condition cant-create-file (cl-i-error)
  ((output-file :initarg :output-file
                :type string
                :initform (error "Please specify the output file.")
            :reader output-file))
  (:documentation "A (user specified) output file cannot be created.")
  (:report (lambda (this strm)
             (format strm
                     "Output file `~A` cannot be created.~&"
                     (output-file this)))))

(defmethod exit-code ((condition cant-create-file))
  (gethash :cant-create-file *exit-codes*))

;; /usr/include/sysexits.h entry:
;;    EX_IOERR -- An error occurred while doing I/O on some file.
;; How CL-I uses it: an error occurred while doing I/O on some file.
;; Fields needed:
;; - The file that had the error.
;; - The problem that occurred.
(define-condition input-output-error (cl-i-error)
  ((io-file :initarg :io-file
         :type string
         :initform (error "Please specify the file.")
            :reader io-file)
   (io-problem :initarg :io-problem
            :type string
            :initform (error "Please specify the problem.")
            :reader io-problem))
  (:documentation "An error occurred while doing I/O on some file.")
  (:report (lambda (this strm)
             (format strm
                     "I/O error on file `~A`: ~A~&"
                     (io-file this)
                     (io-problem this)))))

(defmethod exit-code ((condition input-output-error))
  (gethash :input-output-error *exit-codes*))

;; /usr/include/sysexits.h entry:
;;    EX_TEMPFAIL -- temporary failure, indicating something that
;;        is not really an error.  In sendmail, this means
;;        that a mailer (e.g.) could not create a connection,
;;        and the request should be reattempted later.
;; How CL-I uses it: to describe transient errors.
;; Fields needed:
;; - The transient error.
(define-condition temporary-failure (cl-i-error)
  ((transient-error :initarg :transient-error
                    :type string
                    :initform (error "Please specify the transient error.")
            :reader transient-error))
  (:documentation "Temporary failure, indicating a transient error.")
  (:report (lambda (this strm)
             (format strm
                     "Temporary failure: ~A~&"
                     (transient-error this)))))

(defmethod exit-code ((condition temporary-failure))
  (gethash :temporary-failure *exit-codes*))


;; /usr/include/sysexits.h entry:
;;    EX_PROTOCOL -- the remote system returned something that
;;        was "not possible" during a protocol exchange.
;; How CL-I uses it: to describe invalid messages coming from other servers.
;; Fields needed:
;; - The server in question.
;; - The invalid response.
;; - The problem with the message.
(define-condition protocol-error (cl-i-error)
  ((messaging-server :initarg :messaging-server
           :type string
           :initform (error "Please specify the server.")
            :reader messaging-server)
   (messaging-response :initarg :messaging-response
            :type string
            :initform (error "Please specify the message.")
            :reader messaging-response)
   (messaging-problem :initarg :messaging-problem
            :type string
            :initform (error "Please specify the problem.")
            :reader messaging-problem))
  (:documentation "The remote system returned an invalid response.")
  (:report (lambda (this strm)
             (format strm
                     "Server `~A` returned:~&~&~A~&~&But: ~A"
                     (messaging-server this)
                     (messaging-response this)
                     (messaging-problem this)))))

;; /usr/include/sysexits.h entry:
;; EX_NOPERM -- You did not have sufficient permission to
;;     perform the operation.  This is not intended for
;;     file system problems, which should use NOINPUT or
;;     CANTCREAT, but rather for higher level permissions.
;; How CL-I uses it: to describe insufficient permissions.
;; Fields needed:
;; - The user that attempted the operation.
;; - The destination of the operation.
;; - The operation that was attempted.
(define-condition permission-denied (cl-i-error)
  ((denied-user :initarg :user
         :type string
         :initform (error "Please specify the user.")
            :reader user)
   (denied-destination :initarg :destination
                :type string
                :initform (error "Please specify the destination.")
            :reader destination)
   (denied-operation :initarg :operation
              :type string
              :initform (error "Please specify the operation.")
            :reader operation))
  (:documentation "Insufficient permissions error")
  (:report (lambda (this strm)
             (format strm
                     "User `~A` cannot perform `~A` at or on `~A`.~&"
                     (denied-user this)
                     (denied-operation this)
                     (denied-destination this)))))

(defmethod exit-code ((condition permission-denied))
  (gethash :permission-denied *exit-codes*))

;; /usr/include/sysexits.h entry:
;; EX_CONFIG -- Something was found in an unconfigured or
;;     miscon\-figured state.
;; How CL-I uses it: to describe a configuration error.
;; Fields needed:
;; - The location of the configuration file or resource.
;; - The configuration error.
(define-condition configuration-error (cl-i-error)
  ((config-location :initarg :config-location
                    :type string
                    :initform (error "Please specify the config location.")
            :reader config-location)
   (config-error :initarg :config-error
                  :type string
                  :initform (error "Please specify the config error.")
            :reader config-error))
  (:documentation "Configuration error.")
  (:report (lambda (this strm)
             (format strm
                     "Configuration error at `~A`: ~A~&"
                     (config-location this)
                     (config-error this)))))

(defmethod exit-code ((condition configuration-error))
  (gethash :configuration-error *exit-codes*))

;; OK, now we've made all the superclass conditions, we can define the
;; exit codes for the standard CL conditions.

; Condition Type SERIOUS-CONDITION
(defmethod exit-code ((condition serious-condition))
  (gethash :general-error *exit-codes*))

; Condition Type ARITHMETIC-ERROR
(defmethod exit-code ((condition arithmetic-error))
  (gethash :internal-software-error
           *exit-codes*))

; Condition Type CELL-ERROR
(defmethod exit-code ((condition cell-error))
  (gethash :internal-software-error *exit-codes*))

; Condition Type CONTROL-ERROR
(defmethod exit-code ((condition cell-error))
  (gethash :internal-software-error *exit-codes*))

; Condition Type DIVISION-BY-ZERO
(defmethod exit-code ((condition division-by-zero))
  (gethash :internal-software-error *exit-codes*))

; Condition Type END-OF-FILE
(defmethod exit-code ((condition end-of-file))
  (gethash :data-format-error *exit-codes*))

; Condition Type ERROR
(defmethod exit-code ((condition error))
  (gethash :general-error *exit-codes*))

; Condition Type FILE-ERROR
(defmethod exit-code ((condition file-error))
  (gethash :input-output-error
           *exit-codes*))

; Condition Type FLOATING-POINT-INEXACT
(defmethod exit-code ((condition floating-point-inexact))
  (gethash :internal-software-error *exit-codes*))

; Condition Type FLOATING-POINT-INVALID-OPERATION
(defmethod exit-code ((condition floating-point-invalid-operation))
  (gethash :internal-software-error *exit-codes*))

; Condition Type FLOATING-POINT-OVERFLOW
(defmethod exit-code ((condition floating-point-overflow))
  (gethash :internal-software-error *exit-codes*))

; Condition Type FLOATING-POINT-UNDERFLOW
(defmethod exit-code ((condition floating-point-underflow))
  (gethash :internal-software-error *exit-codes*))

; Condition Type PACKAGE-ERROR
(defmethod exit-code ((condition package-error))
  (gethash :internal-software-error *exit-codes*))

; Condition Type PARSE-ERROR
(defmethod exit-code ((condition parse-error))
  (gethash :data-format-error *exit-codes*))

; Condition Type PRINT-NOT-READABLE
(defmethod exit-code ((condition print-not-readable))
  ;; This one is ambiguous. Can I not print because of a bad return value?
  ;; Or because the object is not printable?
  (gethash :internal-software-error
           *exit-codes*))

; Condition Type PROGRAM-ERROR
(defmethod exit-code ((condition program-error))
  (gethash :internal-software-error *exit-codes*))

; Condition Type READER-ERROR
(defmethod exit-code ((condition reader-error))
  (gethash :data-format-error *exit-codes*))

; Condition Type TYPE-ERROR
(defmethod exit-code ((condition type-error))
  (gethash :data-format-error *exit-codes*))

; Condition Type STORAGE-CONDITION
(defmethod exit-code ((condition storage-condition))
  (gethash :internal-software-error *exit-codes*))

; Condition Type STREAM-ERROR
(defmethod exit-code ((condition stream-error))
  (gethash :input-output-error *exit-codes*))

; Condition Type UNBOUND-SLOT
(defmethod exit-code ((condition unbound-slot))
  (gethash :internal-software-error *exit-codes*))

; Condition Type UNBOUND-VARIABLE
(defmethod exit-code ((condition unbound-variable))
  (gethash :internal-software-error *exit-codes*))

; Condition Type UNDEFINED-FUNCTION
(defmethod exit-code ((condition undefined-function))
  (gethash :internal-software-error *exit-codes*))
