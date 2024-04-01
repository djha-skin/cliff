(defsystem "com.djhaskin.cl-i"
  :version "0.7.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "quri"
               "alexandria"
               "com.djhaskin.nrdl"
               "cl-ppcre"
               "arrows"
               "dexador"
               "trivial-features"
               "trivial-package-local-nicknames"
               )
  :components ((:module "src"
                :components
                (
                 (:file "errors")
                 (:file "main")
                 )))
  :description "CLI library for Common Lisp. Handles args, env vars, and conf"
  :in-order-to (
                (test-op (test-op "com.djhaskin.cl-i/tests"))))

(defsystem "com.djhaskin.cl-i/tests"
  :version "0.7.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
      "com.djhaskin.cl-i"
      "alexandria"
      "parachute"
      "cl-ppcre")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-i"
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call
                           :parachute
                           :test :com.djhaskin.cl-i/tests)))
