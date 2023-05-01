(defsystem "40ants-openrpc-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/40ants-openrpc/"
  :class :package-inferred-system
  :description "Provides documentation for 40ants-openrpc."
  :source-control (:git "https://github.com/40ants/40ants-openrpc")
  :bug-tracker "https://github.com/40ants/40ants-openrpc/issues"
  :pathname "docs"
  :depends-on ("40ants-openrpc"
               "40ants-openrpc-docs/index"))
