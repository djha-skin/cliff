(defsystem "cl-i"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on ("cl-yaml"
               "drakma"
               "alexandria"
               "trivial-package-local-nicknames"
               "arrows")
  :components ((:module "src"
          :components
          ((:file "main"))))
  :description "CLI library for Common Lisp. Handles args, envvars, and conf"
  :in-order-to ((test-op (test-op "cl-i/tests"))))

(defsystem "cl-i/tests"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
      "cl-i"
      "fiveam")
  :components ((:module "tests"
                :components
                ((:file "root") (:file "main"))))
  :description "Test system for cl-i"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                (find-symbol* :cl-i :cl-i/tests))))
