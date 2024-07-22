(in-package :cl-user)
(defpackage quri.uri.http
  (:use :cl)
  (:import-from :quri.uri
                :uri
                :scheme
                :port
                :uri-query)
  (:import-from :quri.port
                :scheme-default-port)
  (:import-from :quri.encode
                :url-encode-params)
  (:import-from :quri.decode
                :url-decode-params)
  (:import-from :alexandria
                #:when-let
		#:positive-integer)
  (:import-from #:closer-mop
		#:defclass)
  (:export :uri-http
           :make-uri-http
           :uri-http-p

           :uri-https
           :make-uri-https
           :uri-https-p

           :uri-query-params))
(in-package :quri.uri.http)

(defclass uri-http (uri)
  ((scheme :initform "http"
	   :initarg :scheme
	   :accessor uri-http-scheme
	   :type string)
   (port :initform #.(scheme-default-port "http")
	 :initarg :port
	 :accessor uri-http-port
	 :type positive-integer)))

(declaim (ftype (function (&rest list &key (:scheme string) (:userinfo string)
				 (:host string) (:port positive-integer) (:path (or string pathname))
				 (:query string) (:fragment string)))
		make-uri-http make-uri-https))
(defun make-uri-http (&rest initargs &key scheme userinfo host port path query fragment)
  (declare (ignore scheme userinfo host port path query fragment))
  (apply #'make-instance (cons 'uri-http initargs)))

(declaim (ftype (function (t) boolean) uri-http-p uri-https-p))
(defun uri-http-p (uri)
  (typep uri (find-class 'uri-http)))

(defclass uri-https (uri)
  ((scheme :initform "https"
	   :initarg :scheme
	   :accessor uri-https-scheme
	   :type string)
   (port :initform #.(scheme-default-port "https")
	 :initarg :port
	 :accessor uri-https-port
	 :type positive-integer)))

(defun make-uri-https (&rest args &key scheme userinfo host port path query fragment)
  (declare (ignore scheme userinfo host port path query fragment))
  (apply #'make-instance (cons 'uri-https args)))

(defun uri-https-p (uri)
  (typep uri (find-class 'uri-https)))

(defun uri-query-params (http &key (lenient t) (percent-decode t))
  (when-let (query (uri-query http))
    (url-decode-params query
                       :lenient lenient
                       :percent-decode percent-decode)))

(defun (setf uri-query-params) (new http &key lenient (percent-encode t))
  (declare (ignore lenient))
  (setf (uri-query http)
        (if new
            (url-encode-params
             new :percent-encode percent-encode)
            nil)))
