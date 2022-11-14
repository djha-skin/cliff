(defsystem "cl-i"
  :version "0.3.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "arrows"
               "cl-yaml"
               "dexador"
               "quri"
               "trivial-features"
               "trivial-package-local-nicknames"
               )
  :components ((:module "src"
          :components
          ((:file "main"))))
  :description "CLI library for Common Lisp. Handles args, envvars, and conf"
  :in-order-to ((test-op (test-op "cl-i/tests"))))

(defsystem "cl-i/tests"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
      "cl-i"
      "alexandria"
      "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-i"
  :perform (test-op (op c) (symbol-call :rove :run c)))
