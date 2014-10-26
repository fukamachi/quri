(in-package :cl-user)
(defpackage quri.uri.http
  (:use :cl)
  (:import-from :quri.uri
                :uri
                :scheme
                :port
                :uri-query)
  (:import-from :quri.encode
                :url-encode-form)
  (:import-from :quri.decode
                :url-decode-form)
  (:import-from :alexandria
                :when-let)
  (:export :uri-http
           :make-uri-http
           :uri-http-p

           :uri-query-form))
(in-package :quri.uri.http)

(defstruct (uri-http (:include uri (scheme :http) (port 80))))

(defun uri-query-form (http)
  (when-let (query (uri-query http))
    (url-decode-form query)))

(defun (setf uri-query-form) (new http)
  (setf (uri-query http) (url-encode-form new)))
