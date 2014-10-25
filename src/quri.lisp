(in-package :cl-user)
(defpackage quri
  (:use :cl)
  (:import-from :quri.parser
                :parse-uri)
  (:import-from :quri.decode
                :url-decode)
  (:import-from :quri.encode
                :url-encode)
  (:export :parse-uri
           :uri
           :uri-scheme
           :uri-userinfo
           :uri-host
           :uri-port
           :uri-path
           :uri-query
           :uri-fragment
           :url-decode
           :url-encode))
(in-package :quri)

(defstruct uri
  scheme
  userinfo
  host
  port
  path
  query
  fragment)

(defstruct (http (:include uri (scheme :http) (port 80))))
(defstruct (https (:include uri (scheme :https) (port 443))))

(defun uri (uri-string)
  (multiple-value-bind (scheme userinfo host port path query fragment)
      (parse-uri uri-string)
    (let ((uri (funcall (cond
                          ((eq scheme :http)  #'make-http)
                          ((eq scheme :https) #'make-https)
                          (T #'make-uri))
                        :scheme scheme
                        :userinfo userinfo
                        :host host
                        :path path
                        :query query
                        :fragment fragment)))
      (when port
        (setf (uri-port uri) port))
      uri)))
