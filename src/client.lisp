(uiop:define-package #:40ants-openrpc/client
  (:use #:cl)
  (:import-from #:dexador)
  (:import-from #:log)
  (:import-from #:openrpc-client)
  (:import-from #:jsonrpc)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:alexandria
                #:write-string-into-file
                #:symbolicate)
  (:export #:generate-client))
(in-package #:40ants-openrpc/client)


(defun cached-url-as (url path)
  "Скачивает URL, если получится, кеширует результат по указанному пути и возвращает этот путь.

   Это нужно, чтобы у нас все спеки микросервисов лежали в репозитории и были доступны
   в момент, когда сами сервисы не доступны, а только происходит сборка."
  (let ((content
          (handler-case (dex:get url :read-timeout 3)
            (error ()
              (log:warn "Unable to retrieve URL with OpenRPC spec from" url "will try to load it from" path)))))
    (cond
      (content
       (write-string-into-file content path
                               :if-exists :supersede
                               :if-does-not-exist :create))
      ((not (probe-file path))
       (error "Не удалось скачать спеку с URL ~A и на диске её тоже нет."
              url)))
    path))


(defmacro generate-client (name url)
  (let* ((spec-url (fmt "~A/openrpc.json"
                        (str:trim-right url :char-bag "/")))
         (package (symbol-package name))
         (connect-func-name (intern "CONNECT" package))
         (package-name (package-name package))
         (system-name (asdf:primary-system-name
                       (string-downcase package-name)))
         (spec-filename "openrpc-spec.json"))
    `(progn
       (openrpc-client:generate-client ,name
                                       (cached-url-as ,spec-url
                                                      (asdf:system-relative-pathname
                                                       ,system-name
                                                       ,spec-filename)))

       (export ',connect-func-name)
       (defun ,connect-func-name (&optional token)
         (jsonrpc:client-connect (make-instance ',name)
                                 :mode :http
                                 :url ,url
                                 :headers (when token
                                            (list (cons :authorization
                                                        token))))))))
