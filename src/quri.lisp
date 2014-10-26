(in-package :cl-user)
(defpackage quri
  (:use :cl
        :quri.uri
        :quri.uri.ftp
        :quri.uri.http
        :quri.uri.ldap)
  (:import-from :quri.parser
                :parse-uri)
  (:import-from :quri.decode
                :url-decode
                :url-decode-form)
  (:import-from :quri.encode
                :url-encode
                :url-encode-form)
  (:import-from :quri.port
                :scheme-default-port)
  (:export :parse-uri

           :uri
           :uri-p
           :uri-scheme
           :uri-userinfo
           :uri-host
           :uri-port
           :uri-path
           :uri-query
           :uri-fragment
           :uri-authority

           :urn
           :urn-p
           :urn-nid
           :urn-nss

           :uri-ftp
           :uri-ftp-p
           :uri-typecode

           :uri-http
           :uri-http-p
           :uri-https
           :uri-https-p
           :uri-query-form

           :uri-ldap
           :uri-ldap-p
           :uri-ldap-dn
           :uri-ldap-attributes
           :uri-ldap-scope
           :uri-ldap-filter
           :uri-ldap-extensions

           :render-uri

           :url-decode
           :url-decode-form
           :url-encode
           :url-encode-form))
(in-package :quri)

(defun uri (uri-string)
  (multiple-value-bind (scheme userinfo host port path query fragment)
      (parse-uri uri-string)
    (funcall (cond
               ((eq scheme :http)  #'make-uri-http)
               ((eq scheme :https) #'make-uri-https)
               ((or (eq scheme :ldap)
                    (eq scheme :ldaps)) #'make-uri-ldap)
               ((eq scheme :ftp)   #'make-uri-ftp)
               ((eq scheme :urn)   #'make-urn)
               (T #'make-uri))
             :port (or port
                       (scheme-default-port scheme))
             :scheme scheme
             :userinfo userinfo
             :host host
             :path path
             :query query
             :fragment fragment)))

(defun render-uri (uri &optional stream)
  (if (uri-ftp-p uri)
      (format stream
              "~:[~;~:*~(~A~):~]~:[~;~:*//~A~]~:[~;~:*~A~]~:[~;~:*;type=~A~]~:[~;~:*?~A~]~:[~;~:*#~A~]"
              (uri-scheme uri)
              (uri-authority uri)
              (uri-path uri)
              (uri-typecode uri)
              (uri-query uri)
              (uri-fragment uri))
      (format stream
              "~:[~;~:*~(~A~):~]~:[~;~:*//~A~]~:[~;~:*~A~]~:[~;~:*?~A~]~:[~;~:*#~A~]"
              (uri-scheme uri)
              (uri-authority uri)
              (uri-path uri)
              (uri-query uri)
              (uri-fragment uri))))

(defmethod print-object ((uri uri) stream)
  (format stream "#<~S ~A>"
          (type-of uri)
          (render-uri uri)))
