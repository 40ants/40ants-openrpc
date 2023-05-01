(uiop:define-package #:40ants-openrpc-tests/server
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:40ants-openrpc-tests/server)


(deftest test-example ()
  (ok t "Replace this test with something useful."))
