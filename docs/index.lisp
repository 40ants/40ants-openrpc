(uiop:define-package #:40ants-openrpc-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:40ants-openrpc-docs/changelog
                #:@changelog)
  (:import-from #:40ants-openrpc/server
                #:start
                #:stop
                #:start-in-production)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:40ants-openrpc-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "40ants-openrpc-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "40ants-openrpc - A set of helpers to start JSON-RPC server based on https://40ants.com/openrpc/ library."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "JWT"
                                   "RPC"
                                   "ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"
                                   "OpenRPC"
                                   "API"
                                   "CORS"
                                   "SLYNK"
                                   "JSON-RPC"
                                   "Unlicense"
                                   "SLYNK_PORT"
                                   "REPL"
                                   "GIT")
                    :external-docs ("https://40ants.com/slynk/"
                                    "https://40ants.com/openrpc/"
                                    "https://40ants.com/clack-cors/")
                    :external-links (("OpenRPC" . "https://40ants.com/openrpc/")))
  (40ants-openrpc system)
  "
[![](https://github-actions.40ants.com/40ants/40ants-openrpc/matrix.svg?only=ci.run-tests)](https://github.com/40ants/40ants-openrpc/actions)

![Quicklisp](http://quickdocs.org/badge/40ants-openrpc.svg)
"
  (@installation section)
  (@usage section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :40ants-openrpc)
```
""")


(defsection @usage (:title "Usage")
  "
This is a set of opionated helpers for starting Common Lisp [OpenRPC][OpenRPC] servers which uses:

- https://40ants.com/openrpc/
- https://40ants.com/logging/
- https://40ants.com/slynk/

Also, it can manage a multiple OpenRPC servers in the one Lisp image.

The easiest way to start a server is to define one or more api methods using [OpenRPC][OpenRPC] library
and then call START. This will bring API on http://localhost:8000/ and it's spec will be available
as http://localhost:8000/openrpc.json

This system uses following environment variables to configure the server:

- `APP_PORT` and `APP_INTERFACE` are used in START-IN-PRODUCTION function to control on which port and interface
  API should be started on.
- `DEBUG` also used in START-IN-PRODUCTION function to control how verbose logging should be. If it is given
  then logging will be with `DEBUG` level.
- `CORS_ALLOWED_ORIGIN` and `CORS_ALLOWED_HEADERS` are control how API will respond with CORS related headers.
  Learn more about used middleware in CLACK-CORS system documentation.
"
  (@api section))


(defautodoc @api (:system "40ants-openrpc"))
