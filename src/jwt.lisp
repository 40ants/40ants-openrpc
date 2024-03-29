(uiop:define-package #:40ants-openrpc/jwt
  (:use #:cl)
  (:import-from #:cl-json-web-tokens)
  (:import-from #:lack.request
                #:request-headers)
  (:import-from #:openrpc-server
                #:return-error)
  (:import-from #:openrpc-server/vars
                #:*current-request*)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:alexandria
                #:assoc-value
                #:ensure-list
                #:make-keyword
                #:with-gensyms)
  (:import-from #:serapeum
                #:dict)
  (:export #:decode
           #:issue-token
           #:with-session
           #:with-test-token))
(in-package #:40ants-openrpc/jwt)


(defun get-jwt-secret ()
  (or (uiop:getenv "JWT_SECRET")
      "test-secret"))


(defun decode (token)
  (cl-json-web-tokens:decode token :secret (get-jwt-secret)))


(defun issue-token (payload &key ttl)
  "Encodes payload into a JWT token.

   If TTL argument is given, it should be specified in seconds. After this number of seconds, token will become invalid."
  (check-type ttl (or null
                      integer))
  (cl-json-web-tokens:issue payload
                            :algorithm :hs256
                            :secret (get-jwt-secret)
                            :issued-at (get-universal-time)
                            :expiration (when ttl
                                          (+ (get-universal-time)
                                             ttl))))


(defun call-with-test-token (token thunk)
  (let* ((*current-request* (lack.request:make-request (list :headers
                                                             (dict))))
         (headers (lack.request:request-headers *current-request*)))
    (setf (gethash "authorization" headers)
          token)
    (funcall thunk)))


(defmacro with-test-token ((token) &body body)
  `(call-with-test-token ,token (lambda () ,@body)))


(defun decode-current-jwt-token ()
  (let* ((headers (request-headers *current-request*))
         (token (gethash "authorization" headers)))
    (when token
      (handler-case
          (with-log-unhandled ()
            (decode token))
        (error (c)
          (openrpc-server:return-error (format nil "Unable to parse Authorization token: ~A"
                                               c)))))))


(defun ensure-list-of-keywords (items)
  (loop for item in items
        collect (make-keyword (string-upcase item))))


(defmacro with-session (((&rest bindings)
                         &key (require t)
                              (processors
                               '(("roles" . ensure-list-of-keywords))))
                        &body body)
  (with-gensyms (session-var)
    (let ((bindings
            (loop for var in (ensure-list bindings)
                  for key = (string-downcase var)
                  for processor = (assoc-value processors key
                                               :test #'string-equal)
                  collect (cond
                            (processor
                             `(,var (when ,session-var
                                      (,processor (gethash ,key ,session-var)))))
                            (t
                             `(,var (when ,session-var
                                      (gethash ,key ,session-var))))))))
      `(let ((,session-var (decode-current-jwt-token)))
         (when (and ,require
                    (not ,session-var))
           (return-error "This method requires authentication."
                         :code 3))
         (let (,@bindings)
           ,@body)))))

