(defpackage consistent-hashing/tests/main
  (:use :cl
   :consistent-hashing
        :rove))
(in-package :cl-consistent-hashing/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-consistent-hashing)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
