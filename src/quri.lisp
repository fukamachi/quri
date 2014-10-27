(in-package :cl-user)
(defpackage quri
  (:use :cl
        :quri.uri
        :quri.uri.ftp
        :quri.uri.http
        :quri.uri.ldap)
  (:import-from :quri.parser
                :parse-uri-string)
  (:import-from :quri.decode
                :url-decode
                :url-decode-params)
  (:import-from :quri.encode
                :url-encode
                :url-encode-params)
  (:import-from :quri.port
                :scheme-default-port)
  (:export :parse-uri-string

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
           :uri-ftp-typecode

           :uri-http
           :uri-http-p
           :uri-query-params

           :uri-ldap
           :uri-ldap-p
           :uri-ldap-dn
           :uri-ldap-attributes
           :uri-ldap-scope
           :uri-ldap-filter
           :uri-ldap-extensions

           :render-uri

           :url-decode
           :url-decode-params
           :url-encode
           :url-encode-params))
(in-package :quri)

(defun uri (uri-string &key (start 0) end)
  (multiple-value-bind (scheme userinfo host port path query fragment)
      (parse-uri-string uri-string :start start :end end)
    (funcall (cond
               ((or (eq scheme :http)
                    (eq scheme :https)) #'make-uri-http)
               ((or (eq scheme :ldap)
                    (eq scheme :ldaps)) #'make-uri-ldap)
               ((eq scheme :ftp)        #'make-uri-ftp)
               ((eq scheme :urn)        #'make-urn)
               (T                       #'make-uri))
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
