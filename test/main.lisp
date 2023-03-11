#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)

(defpackage #:cl-i/tests
  (:use #:cl
        #:fiveam)
  (:import-from
    #:cl-i))
(in-package :cl-i/tests)

(def-suite :cl-i
           :description "Main functions in test suite.")
(in-suite :cl-i)

(defvar *test-config-file*
  (merge-pathnames
    (make-pathname
      :directory
      (list
        :relative
        "Code"
        "lisp"
        "cl-i"
        "tests")
      :name
      ".cl-i"
      :type
      "json")
    (cl-i:os-specific-home #'uiop/os:getenv)))

(defvar *tests-dir*
  (merge-pathnames
    #P"tests/"
    (slot-value
      (asdf:find-system "cl-i")
      'asdf/component:absolute-pathname)))

; For the REPL:
;(setf fiveam:*run-test-when-defined* t)

(defun positive-dec (n) (declare (number n)) (if (> n 0) (- n 1) 0))

(defun broken-dec (n) (declare (number n)) (if (>= n 0) (- n 1) 0))

(test "repeatedly-eq"
      (signals (cl-i:repeatedly-eq #'broken-dec 3))
      (is (equal (cl-i:repeatedly-eq #'positive-dec 3) '(3 2 1 0))))

(test "repeatedly"
      (signals 
        (cl-i:repeatedly
          #'positive-dec
          3
          (lambda (thing) (< thing 0))))
      (is (equal (cl-i:repeatedly
                   #'positive-dec
                   3
                   (lambda (item)
                     (<= item 0)))
                 '(3 2 1))))

(test nested-to-alist-empty
      (is (equal nil (cl-i:nested-to-alist nil)))
      (is (equal "" (cl-i:nested-to-alist ""))))
(test nested-to-alist-atomic-values
      (is (equal "hi" (cl-i:nested-to-alist "hi")))
      (is (equal 15 (cl-i:nested-to-alist 15)))
      (is (equal t (cl-i:nested-to-alist t)))
      (is (equal 'a (cl-i:nested-to-alist 'a)))
      (is (equal :b (cl-i:nested-to-alist :b))))
(test nested-to-alist-typical-invocations
      (is
        (equal
          (let
            ((a (make-hash-table)))
            (setf (gethash 'a a) 1)
            (setf (gethash 'b a) 2)
            (setf (gethash 'c a) 3)
            (cl-i:nested-to-alist
              `(1 2 3 (4 5) 6 (7 (8 ,a)))))
          '(1 2 3 (4 5) 6 (7 (8 ((A . 1) (B . 2) (C . 3)))))))
      (is (equal
            (let ((a (make-hash-table))
                  (b (make-hash-table)))
              (setf (gethash :origin b) "thither")
              (setf (gethash :destination b) "yon")
              (setf (gethash 'a a) nil)
              (setf (gethash 'b a) b)
              (setf (gethash 'c a) '(1 2 3 4 5))
              (cl-i:nested-to-alist a))
            '((A)
              (B 
                (:DESTINATION . "yon")
                (:ORIGIN . "thither")) (C 1 2 3 4 5)))))

(test basic-find-file
      (is
        (equal
          (slot-value
            (asdf:find-system "cl-i")
            'asdf/component:absolute-pathname)
          (cl-i:find-file
            *tests-dir*
            "cl-i")))
      (is
        (equal
          (cl-i:find-file
            (merge-pathnames
              #P"/leaves-of-grass"
              (slot-value
                (asdf:find-system "cl-i")
                'asdf/component:absolute-pathname))
            "600dc0d36077a10ada600dd3a10fda7a")
          nil)))

(test
  dbg-basic
  (is
    (let ((tested-string
            (with-output-to-string (str)
              (cl-i:dbg (+ 3 3) str))))
      (or
        ;; ecl
        (equal
          tested-string  
          (format
            nil
            (concatenate
              'string
              "debug: type of `(+ 3 3)` = `(INTEGER 6 6)`~%"
              "debug: eval of `(+ 3 3)` = `6`~%")))
        ;; sbcl
        (equal
          tested-string
          (format
            nil
            (concatenate
              'string
              "debug: type of `(+ 3 3)` = `(INTEGER 6 6) 4611686018427387903)`~%"
              "debug: eval of `(+ 3 3)` = `6`~%")))))))

(test slurp-stream-basic
      (is
        (equal
          (with-open-file
            (f
              (merge-pathnames
                #P".cl-i.json"
                *tests-dir*)
              :direction :input
              :external-format :utf-8)
            (cl-i:slurp-stream f))
          "{ \"hoo\": \"haa\"}")))


(test base-slurp
      (is
        (equal
          (cl-i::base-slurp
            *test-config-file*)
          "{ \"hoo\": \"haa\"}")))

(test
  data-slurp-paths
  (is
    (equal
      (cl-i:data-slurp
        *test-config-file*)
      "{ \"hoo\": \"haa\"}")))
(test
  data-slurp-file-url
  (is
    (equal
      (cl-i:data-slurp
        (concatenate 'string
                     "file://"
                     (namestring *test-config-file*)))
      "{ \"hoo\": \"haa\"}")))
(test
  data-slurp-noauth
  (is
    (equal
      (cl-i:data-slurp
        "https://localhost:8443/noauth/complete.txt"
        :insecure t)
      (format nil "noauth complete"))))

(test
  data-slurp-basic
  (is
    (equal
      (cl-i:data-slurp
        "https://mode:code@localhost:8443/basic/complete.txt"
        :insecure t)
      (format nil "basic complete"))))
(test
  data-slurp-header
  (is
    (equal
      (cl-i:data-slurp
        "https://Authorization=Bearer%20600dc0de6077a10ada600ddea10fda7a@localhost:8443/token/complete.txt"
        :insecure t)
      (format nil "token complete"))))
(test
  data-slurp-token
  (is
    (equal
      (cl-i:data-slurp
        "https://600dc0de6077a10ada600ddea10fda7a@localhost:8443/token/complete.txt"
        :insecure t)
      (format nil "token complete"))))


(test consume-arguments-other-args
      (signals 
        (cl-i:consume-arguments
          '("--enable-dark-mode"
            "--reset-dark-mode"
            "--add-dark-mode"
            "crying"
            "--add-dark-mode"
            "firm"
            "well-done"
            "medium-well"
            "--join-my"
            "pride=hurt"
            "--join-my"
            "start=great"
            "--yaml-fight"
            "15.0"
            "--yaml-stride"
            "tests/.cl-i.yaml")))
      (multiple-value-bind (opts other-args)
        (cl-i:consume-arguments
          '("--enable-dark-mode"
            "--reset-dark-mode"
            "--add-dark-mode"
            "crying"
            "--add-dark-mode"
            "firm"
            "well-done"
            "medium-well"
            "--join-my"
            "pride=hurt"
            "--join-my"
            "start=great"
            "--json-fight"
            "15.0"
            "--file-stride"
            "tests/.cl-i.json"))
        (is (equal
              (cl-i:nested-to-alist opts)
              '((:DARK-MODE "firm" "crying")
                (:FIGHT . 15.0d0)
                (:MY (:PRIDE . "hurt")
                     (:START . "great"))
                (:STRIDE (:HOO . "haa")))
              ))
        (is (equal
              '("medium-well" "well-done")
              other-args))))
(test consume-arguments-empty
      (is
        (equal (cl-i:generate-string
                 (cl-i:consume-arguments
                   '()))
               "{}")))
(test
  consume-arguments-basic
  (is
    (equal (cl-i:nested-to-alist
             (cl-i:consume-arguments
               '("--enable-foo" "--disable-bar" "baz" "--json-force" "15" "--set-quux" "farquad")))
           '((:BAR) (:FOO . T) (:FORCE . 15) (:QUUX . "farquad")))))
(test
  consume-environment-basic
  (is
    (equal
      '((:FINES (:KEY . 155.5d0))
        (:FORESIGHT . T)
        (:FORKS . "whenceandwhither")
        (:MAPLE "1" "2" "3" "4" "5"))
      (cl-i:nested-to-alist
        (cl-i:consume-environment
          "hello"
          (alexandria:alist-hash-table
            '(("HELLO_LIST_MAPLE" . "1,2,3,4,5")
              ("HELLO_FANGLE_DOG" . "12345")
              ("VARS" . "xtreem")
              ("HELLO_JSON_FINES" . "{ \"key\": 155.5 }")
              ("HELLO_FLAG_FORESIGHT" . "0")
              ("HELLO_ITEM_FORKS" . "whenceandwhither"))))))))

(defun blank-command
  (options)
  (format t "Options:~&  ~A~&" (cl-i:nested-to-alist options))
  (alexandria:alist-hash-table
    '((:status . :successful))))

(defun error-command
  (options)
  (format t "Options:~&  ~A~&" (cl-i:nested-to-alist options))
  (alexandria:alist-hash-table
    '((:status . :general-error))))

(defun io-error
  (options)
  (format t "Options:~&  ~A~&" (cl-i:nested-to-alist options))
  (alexandria:alist-hash-table
    '((status . :input-output-error))))

(test
  nested-to-alist-typical-invocation
  (is
    (equal
      (cl-i:nested-to-alist
        (cl-i:config-file-options
          "hi"
          (alexandria:alist-hash-table
            '(
              ("HOME" . "/home/djha-skin")
              )
            :test #'equal)
          (make-hash-table)))
      '((:CHIVES
          (:LOVE . 15)
          (:SORE_LOSERS
            (:COOL . "beans")
            (:STATE . "virginia"))
          (:SPICES . T))
        (:DOT)
        (:GARY . 3)
        (:HAIRY . 4)))))

(test
  execute-program-empty-cases
  (is
    (signals
      (cl-i:execute-program
        "Halo"
        (make-hash-table)
        nil)
      'cl-i:necessary-env-var-absent))
  (is
    (signals
      (cl-i:execute-program
        "Halo"
        (alexandria:alist-hash-table
          '(("HOME" . "/home/djha-skin"))
          :test #'equal)
        nil)
      'cl-i:invalid-subcommand)))

(test execute-program-typical-invocation
      (let* ((teststr (make-string-output-stream))
             (code (cl-i:execute-program
                     "hi"
                     (alexandria:alist-hash-table
                       '(
                         ("HOME" . "/home/djha-skin")
                         ("HI_ITEM_FOUR" . "square")
                         ("HI_LIST_LOVERS" . "so,many,lovers")
                         ("HI_TABLE_OF" . "contents=lots,content-people=few,content-makers=too-many")
                         )
                       :test #'equal)
                     `((nil . ,#'blank-command)
                       (("error") . ,#'error-command)
                       (("io-error") . ,#'io-error))
                     :cli-arguments
                     '(
                       "--join-deals" "a=jhyghyjub"
                       "--join-deals" "c=d"
                       "--add-barf" "1"
                       "--add-barf" "2"
                       "--add-barf" "3"
                       "--enable-gary"
                       "--reset-gary"
                       "--set-gary" "four"
                       "--disable-all-the-things"
                       "io-error"
                       ))
                   :out-stream
                   teststr)))
      (is
        (equal (get-output-stream-string teststr)
               (cl-i:join-strings
                 '("{"
                   "  \"barf\": ["
                   "    \"3\","
                   "    \"2\","
                   "    \"1\""
                   "  ],"
                   "  \"lovers\": ["
                   "    \"so\","
                   "    \"many\","
                   "    \"lovers\""
                   "  ],"
                   "  \"all-the-things\": false,"
                   "  \"of\": {"
                   "    \"contents\": \"lots\","
                   "    \"content-people\": \"few\","
                   "    \"content-makers\": \"too-many\""
                   "  },"
                   "  \"gary\": \"four\","
                   "  \"four\": \"square\","
                   "  \"hairy\": 4,"
                   "  \"dot\": false,"
                   "  \"chives\": {"
                   "    \"spices\": true,"
                   "    \"sore_losers\": {"
                   "      \"cool\": \"beans\","
                   "      \"state\": \"virginia\""
                   "    },"
                   "    \"love\": 15"
                   "  },"
                   "  \"deals\": {"
                   "    \"c\": \"d\","
                   "    \"a\": \"jhyghyjub\""
                   "  }"
                   "}"
                   ""))))
      (is (equal (code 74))))
