(in-package :cl-user)
(defpackage quri
  (:use :cl)
  (:import-from :quri.parser
                :parse-uri)
  (:export :parse-uri
           :uri
           :uri-scheme
           :uri-userinfo
           :uri-host
           :uri-port
           :uri-path
           :uri-query
           :uri-fragment))
(in-package :quri)

(defstruct uri
  scheme
  userinfo
  host
  port
  path
  query
  fragment)

(defun uri (uri-string)
  (multiple-value-bind (scheme userinfo host port path query fragment)
      (parse-uri uri-string)
    (make-uri :scheme scheme
              :userinfo userinfo
              :host host
              :port port
              :path path
              :query query
              :fragment fragment)))
