(uiop:define-package #:40ants-openrpc/server
  (:use #:cl)
  (:import-from #:40ants-logging)
  (:import-from #:40ants-slynk)
  (:import-from #:local-time)
  (:import-from #:cl+ssl)
  (:import-from #:clack
                #:clackup)
  (:import-from #:openrpc-server
                #:make-clack-app)
  (:import-from #:clack-cors
                #:make-cors-middleware)
  (:export #:start
           #:stop
           #:start-in-production))
(in-package #:40ants-openrpc/server)


(defvar *default-port* "8000")

(defvar *default-interface* "localhost")

(defvar *servers* (make-hash-table :test 'equal))


(defun find-server (port interface)
  (gethash (cons port interface)
           *servers*))

(defun register-server (port interface server)
  (setf (gethash (cons port interface)
                 *servers*)
        server))

(defun delete-server (port interface)
  (remhash (cons port interface)
           *servers*))


(defun start (&key (port *default-port*)
                (api openrpc-server/api::default-api)
                (interface *default-interface*)
                (debug nil))
  (when (find-server port interface)
    (error "Server already running"))

  (when (probe-file ".local-config.lisp")
    (load (probe-file ".local-config.lisp")))

  (40ants-logging:setup-for-backend :level (if debug
                                               :debug
                                               :warn))

  (40ants-slynk:start-slynk-if-needed)
  (local-time:reread-timezone-repository)

  (let ((postgres-certs-file
          (probe-file "~/.postgresql/root.crt")))
    (when postgres-certs-file
      (cl+ssl:ssl-load-global-verify-locations postgres-certs-file)))

  (register-server port interface
                   (clackup
                    (make-cors-middleware
                     (make-clack-app :api api :indent-json t)
                     :allowed-origin (or (uiop:getenv "CORS_ALLOWED_ORIGIN")
                                         clack-cors:*default-allowed-origin*)
                     :allowed-headers (or (uiop:getenv "CORS_ALLOWED_HEADERS")
                                          clack-cors:*default-allowed-headers*))
                    :address interface
                    :port port))
  (log:info "Server started" api)
  (values))


(defun stop (&key (port *default-port*)
               (interface *default-interface*))
  (let ((server (find-server port interface)))
    (when server
      (clack:stop server)
      (delete-server port interface)))
  (values))


(defun start-in-production ()
  ;; Entry point for webapp, started in the Docker
  (start :port (parse-integer (or (uiop:getenv "APP_PORT")
                                  *default-port*))
         :interface (or (uiop:getenv "APP_INTERFACE")
                        *default-interface*))
  (loop do (sleep 5)))
