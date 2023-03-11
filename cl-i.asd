(defsystem
  "cl-i"
  :version "0.4.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :description "CLI Framework for Common Lisp"
  :depends-on (
               #:arrows
               #:com.inuoe.jzon
               #:alexandria
               #:dexador
               #:quri
               #:trivial-features
               #:trivial-package-local-nicknames
               )
  :in-order-to ((test-op (load-op "cl-i-test")))
  :perform (test-op (op c) (unless (symbol-call :fiveam :run! '#:cl-i)
                             (error "CL-I tests failed to run.")))
  :components
  ((:module "src"
            :components ((:file "main")))))
