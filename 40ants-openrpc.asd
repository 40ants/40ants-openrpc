#-asdf3.1 (error "40ants-openrpc requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "40ants-openrpc"
  :description "A set of helpers to start JSON-RPC server based on https://40ants.com/openrpc/ library."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/40ants-openrpc/"
  :source-control (:git "https://github.com/40ants/40ants-openrpc")
  :bug-tracker "https://github.com/40ants/40ants-openrpc/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("40ants-openrpc/server")
  :in-order-to ((test-op (test-op "40ants-openrpc-tests"))))
