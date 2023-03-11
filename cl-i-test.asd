(defsystem "cl-i-tests"
  :version "0.4.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
      #:cl-i
      #:alexandria
      #:fiveam)
  :components ((:module "test"
                :components
                ((:file "main"))))
  :description "Test system for cl-i")
