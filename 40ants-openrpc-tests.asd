(defsystem "40ants-openrpc-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/40ants-openrpc/"
  :class :package-inferred-system
  :description "Provides tests for 40ants-openrpc."
  :source-control (:git "https://github.com/40ants/40ants-openrpc")
  :bug-tracker "https://github.com/40ants/40ants-openrpc/issues"
  :pathname "t"
  :depends-on ("40ants-openrpc-tests/server")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
