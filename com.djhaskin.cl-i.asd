(defsystem "com.djhaskin.cl-i"
  :version "0.8.0"
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
  :in-order-to ((test-op (test-op "com.djhaskin.cl-i/tests"))))

(defsystem "com.djhaskin.cl-i/tests"
  :version "0.8.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
      "com.djhaskin.cl-i"
      "alexandria"
      "rove"
      "cl-ppcre")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-i"
  :perform (test-op (op c) (symbol-call :rove :run c)))
