(uiop:define-package #:40ants-openrpc/server
  (:use #:cl)
  (:import-from #:40ants-logging)
  (:import-from #:40ants-slynk)
  (:import-from #:local-time)
  (:import-from #:cl+ssl)
  (:import-from #:with-user-abort)
  (:import-from #:clack
                #:clackup)
  (:import-from #:openrpc-server
                #:make-clack-app)
  (:import-from #:clack-cors
                #:make-cors-middleware)
  (:import-from #:clack-prometheus
                #:with-prometheus-stats)
  (:import-from #:openrpc-server/clack
                #:app-middlewares)
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
  "Starts Open RPC API server on given PORT and INTERFACE.
   Also it configures logging and Slynk.

   Slynk is started only if SLYNK_PORT env variable is set to some value.
   You will find more details in the 40ANTS-SLYNK system documentation.
"
  (when (find-server port interface)
    (error "Server already running"))

  (when (probe-file ".local-config.lisp")
    (load (probe-file ".local-config.lisp")))

  (40ants-logging:setup-for-backend :level (if debug
                                               :debug
                                               :warn))

  (40ants-slynk:start-slynk-if-needed)
  (local-time:reread-timezone-repository)

  ;; TODO: probably is should move this code to 40ants-pg somehow?
  (let ((postgres-certs-file
          (probe-file "~/.postgresql/root.crt")))
    (when postgres-certs-file
      (cl+ssl:ssl-load-global-verify-locations postgres-certs-file)))

  (let* ((main-app (make-clack-app api :indent-json t))
         (middlewares-plist (app-middlewares api))
         (app (loop with app = main-app
                    for (middleware-name middleware) on middlewares-plist by #'cddr
                    do (log:debug "Applying middleware" middleware-name)
                       (setf app
                             (funcall middleware app))
                    finally (return app))))
    (register-server port interface
                     (clackup app
                              :address interface
                              :port port)))
  (log:info "Server started" api)
  (values))


(defun stop (&key (port *default-port*)
               (interface *default-interface*))
  "Stops API server running on given PORT and INTERFACE."
  (let ((server (find-server port interface)))
    (when server
      (clack:stop server)
      (delete-server port interface)))
  (values))


(defun start-in-production (&key (api openrpc-server/api::default-api))
  "Entry point for API webserver, started in the Docker or Kubernetes.
   It works like a START but blocks forever."
  (start :api api
         :port (parse-integer (or (uiop:getenv "APP_PORT")
                                  *default-port*))
         :interface (or (uiop:getenv "APP_INTERFACE")
                        *default-interface*)
         :debug (when (uiop:getenv "DEBUG")
                  t))

  (handler-case
      (with-user-abort:with-user-abort
        (loop do (sleep 5)))
    (with-user-abort:user-abort ()
      (log:info "Quitting gracefully")
      (stop)
      ;; Wait 5 seconds till server will stop
      (loop repeat 5
            do (sleep 1))
      (uiop:quit 0))))
