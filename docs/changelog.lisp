(uiop:define-package #:40ants-openrpc-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-openrpc-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.2.0 2023-05-27
         "
## Changes

* 40ANTS-OPENRPC/SERVER:START function now uses OPENRPC-SERVER/CLACK:APP-MIDDLEWARES generic-function.

## Additions

* Added 40ANTS-OPENRPC/CLIENT:GENERATE-CLIENT macro to generate client while storing remote spec in a local cache file.
* Added 40ANTS-OPENRPC/JWT:WITH-SESSION macro to decode JWT token and extract user-id and other params out of it. Also this package provides 40ANTS-OPENRPC/JWT:ISSUE-TOKEN function which is useful for logging user in.
          ")
  (0.1.0 2023-05-01
         "* Initial version."))
