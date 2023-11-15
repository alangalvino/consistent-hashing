(defsystem "consistent-hashing"
  :version "0.0.1"
  :author "Alan Gomes"
  :license ""
  :depends-on ("trees")
  :components ((:module "src"
                :components
                ((:file "consistent-hashing")
                 (:file "virtual-node")
                 (:file "finder")
                 (:file "ring")
                 (:file "finder-tree"))))
  :description ""
  :in-order-to ((test-op (test-op "consistent-hashing/tests"))))

(defsystem "consistent-hashing/tests"
  :author "Alan Gomes"
  :license ""
  :depends-on ("consistent-hashing"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "ring"))))
  :description "Test system for consistent-hashing"
  :perform (test-op (op c) (symbol-call :rove :run c)))
