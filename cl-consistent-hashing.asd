(defsystem "cl-consistent-hashing"
  :version "0.0.1"
  :author "Alan Gomes"
  :license ""
  :depends-on ("trees")
  :components ((:module "src"
                :components
                ((:file "ring"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-consistent-hashing/tests"))))

(defsystem "cl-consistent-hashing/tests"
  :author "Alan Gomes"
  :license ""
  :depends-on ("cl-consistent-hashing"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "ring"))))
  :description "Test system for cl-consistent-hashing"
  :perform (test-op (op c) (symbol-call :rove :run c)))
