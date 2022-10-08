(defsystem "cl-i"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("cl-yaml"
               "drakma"
               "alexandria"
               "trivial-package-local-nicknames"
               "arrows")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-i/tests"))))

(defsystem "cl-i/tests"
  :author ""
  :license ""
  :depends-on ("cl-i"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-i"
  :perform (test-op (op c) (symbol-call :rove :run c)))
