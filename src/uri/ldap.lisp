(in-package :cl-user)
(defpackage quri.uri.ldap
  (:use :cl)
  (:import-from :quri.uri
                :uri
                :scheme
                :port
                :uri-path
                :uri-query)
  (:import-from :quri.port
                :scheme-default-port)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from #:closer-mop
		#:defclass)
  (:import-from :alexandria
                #:when-let
		#:positive-integer)
  (:export :uri-ldap
           :make-uri-ldap
           :uri-ldap-p

           :uri-ldaps
           :make-uri-ldaps
           :uri-ldaps-p

           :uri-ldap-dn
           :uri-ldap-attributes
           :uri-ldap-scope
           :uri-ldap-filter
           :uri-ldap-extensions))
(in-package :quri.uri.ldap)

(defclass uri-ldap (uri)
  ((scheme :initform "ldap"
	   :initarg :scheme
	   :accessor uri-ldap-scheme
	   :type string)
   (port :initform #.(scheme-default-port "ldap")
	 :initarg :port
	 :accessor uri-ldap-port
	 :type positive-integer)))

(declaim (ftype (function (&rest list &key (:scheme string) (:userinfo string)
				 (:host string) (:port positive-integer) (:path (or string pathname))
				 (:query string) (:fragment string)))
		make-uri-ldap make-uri-ldaps))
(defun make-uri-ldap (&rest initargs &key scheme userinfo host port path query fragment)
  (declare (ignore scheme userinfo host port path query fragment))
  (apply #'make-instance (cons 'uri-ldap initargs)))

(declaim (ftype (function (t) boolean) uri-ldaps-p uri-ldap-p))
(defun uri-ldap-p (uri)
  (typep uri (find-class 'uri-ldap)))

(defclass uri-ldaps (uri)
  ((scheme :initform "ldaps"
	   :initarg :scheme
	   :accessor uri-ldaps-scheme
	   :type string)
   (port :initform #.(scheme-default-port "ldaps")
	 :initarg :port
	 :accessor uri-ldaps-port
	 :type positive-integer)))

(defun make-uri-ldaps (&rest initargs &key scheme userinfo host port path query fragment)
  (declare (ignore scheme userinfo host port path query fragment))
  (apply #'make-instance (cons 'uri-ldaps initargs)))

(defun uri-ldaps-p (uri)
  (typep uri (find-class 'uri-ldaps)))

(defun uri-ldap-dn (ldap)
  (let ((path (uri-path ldap)))
    (when (and path
               (/= 0 (length path)))
      (if (char= (aref path 0) #\/)
          (subseq path 1)
          path))))

(defun (setf uri-ldap-dn) (new ldap)
  (setf (uri-path ldap)
        (concatenate 'string "/" new))
  new)

(defun nth-uri-ldap-lists (ldap n)
  (check-type ldap uri-ldap)
  (when-let (query (uri-query ldap))
    (car (last (split-sequence #\? query :count n)))))

(defun (setf nth-uri-ldap-lists) (new ldap n)
  (check-type ldap uri-ldap)
  (check-type new string)
  (let ((query (uri-query ldap)))
    (setf (uri-query ldap)
          (if query
              (let ((parts (split-sequence #\? query)))
                (with-output-to-string (s)
                  (dotimes (i n)
                    (princ (or (pop parts) "") s)
                    (write-char #\? s))
                  (princ new s)
                  (pop parts) ;; ignore
                  (dolist (part parts)
                    (write-char #\? s)
                    (princ part s))))
              new))))

(defun uri-ldap-attributes (ldap)
  (nth-uri-ldap-lists ldap 1))
(defun (setf uri-ldap-attributes) (new ldap)
  (setf (nth-uri-ldap-lists ldap 0) new))

(defun uri-ldap-scope (ldap)
  (nth-uri-ldap-lists ldap 2))
(defun (setf uri-ldap-scope) (new ldap)
  (setf (nth-uri-ldap-lists ldap 1) new))

(defun uri-ldap-filter (ldap)
  (nth-uri-ldap-lists ldap 3))
(defun (setf uri-ldap-filter) (new ldap)
  (setf (nth-uri-ldap-lists ldap 2) new))

(defun uri-ldap-extensions (ldap)
  (nth-uri-ldap-lists ldap 4))
(defun (setf uri-ldap-extensions) (new ldap)
  (setf (nth-uri-ldap-lists ldap 3) new))
