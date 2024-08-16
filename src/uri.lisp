(in-package :cl-user)
(defpackage quri.uri
  (:use :cl)
  (:import-from :quri.port
                :scheme-default-port)
  (:import-from #:closer-mop
		#:defclass)
  (:import-from #:alexandria
		#:positive-integer)
  (:export :uri
           :make-basic-uri
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
           :make-urn
           :urn-p
           :urn-nid
           :urn-nss))
(in-package :quri.uri)

(defclass uri ()
  ((scheme :initarg :scheme
	   :accessor uri-scheme
	   :initform nil
	   :type (or null string))
   (userinfo :initarg :userinfo
	     :accessor uri-userinfo
	     :initform nil
	     :type (or null string))
   (host :initarg :host
	 :accessor uri-host
	 :initform nil
	 :type (or null string))
   (port :initarg :port
	 :accessor uri-port
	 :initform nil
	 :type (or null positive-integer))
   (path :initarg :path
	 :accessor uri-path
	 :initform nil
	 :type (or null string pathname))
   (query :initarg :query
	  :accessor uri-query
	  :initform nil
	  :type (or null string))
   (fragment :initarg :fragment
	     :accessor uri-fragment
	     :initform nil
	     :type (or null string))))

(declaim (ftype (function (t) boolean) uri-p urn-p))
(defun uri-p (uri)
  (typep uri (find-class 'uri)))

(defmethod make-load-form ((object uri) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(declaim (ftype (function (&rest t &key (:scheme string) (:userinfo string)
				 (:host string) (:port positive-integer)
				 (:path (or string pathname))
				 (:query string) (:fragment string)))
		make-basic-uri))
(defun make-basic-uri (&rest initargs &key scheme userinfo host port path query fragment)
  (declare (ignore scheme userinfo host port path query fragment))
  (let ((uri (apply #'make-instance (cons 'uri initargs))))
    (unless (uri-port uri)
      (setf (uri-port uri) (scheme-default-port (uri-scheme uri))))
    (when (pathnamep (uri-path uri))
      (setf (uri-path uri)
            (uiop:native-namestring (uri-path uri))))
    uri))

(defun uri-authority (uri)
  (when (uri-host uri)
    (let ((default-port (scheme-default-port (uri-scheme uri))))
      (with-standard-io-syntax
        (format nil "~:[~;~:*~A@~]~A~:[:~A~;~*~]"
                (uri-userinfo uri)
                (uri-host uri)
                (eql (uri-port uri) default-port)
                (uri-port uri))))))

(defclass urn (uri)
  ((nid :initarg nid
	:accessor urn-nid
	:initform :urn)
   (nss :initarg nss
	:accessor urn-nss
	:initform nil)))

(defun urn-p (uri)
  (typep uri (find-class 'urn)))

(declaim (ftype (function (&rest t &key (:scheme string) (:userinfo string)
				 (:host string) (:port positive-integer) (:path (or string pathname))
				 (:query string) (:fragment string) (:nid t) (:nss t)))
		make-urn))
(defun make-urn (&rest initargs &key scheme userinfo host port path query fragment nid nss)
  (declare (ignore scheme userinfo host port path query fragment nid nss))
  (let ((urn (apply #'make-instance (cons 'urn initargs))))
    (when (uri-path urn)
      (let ((colon-pos (position #\: (uri-path urn))))
        (if colon-pos
            (setf (urn-nid urn) (subseq (uri-path urn) 0 colon-pos)
                  (urn-nss urn) (subseq (uri-path urn) (1+ colon-pos)))
            (setf (urn-nid urn) (uri-path urn)))))
    urn))
