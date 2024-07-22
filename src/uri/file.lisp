(in-package :cl-user)
(defpackage quri.uri.file
  (:use :cl)
  (:import-from :quri.uri
                :uri
                :scheme
                :port
                :uri-path)
  (:import-from #:alexandria
		#:positive-integer)
  (:import-from #:closer-mop
		#:defclass)
  (:export :uri-file
           :uri-file-p
           :make-uri-file
           :uri-file-pathname))
(in-package :quri.uri.file)

(defclass uri-file (uri)
  ((scheme :initform "file"
	   :initarg :scheme
	   :accessor uri-file-scheme
	   :type string)))

(declaim (ftype (function (&rest t &key (:scheme string) (:userinfo string)
				 (:host string) (:port positive-integer) (:path (or string pathname))
				 (:query string) (:fragment string)))
		make-uri-file))
(defun make-uri-file (&rest initargs &key scheme userinfo host port path query fragment)
  (declare (ignore scheme userinfo host port query fragment))
  (let ((file-uri (apply #'make-instance (cons 'uri-file initargs))))
    (when (pathnamep path)
      (setf (uri-path file-uri)
	    (uiop:native-namestring path)))
    file-uri))

(declaim (ftype (function (t) boolean) uri-file-p))
(defun uri-file-p (uri)
  (typep uri (find-class 'uri-file)))

(declaim (ftype (function (uri-file) pathname) uri-file-pathname))
(defun uri-file-pathname (file)
  "Get a lisp pathname object from a file URI.
Assumes that the path of the file URI is correct path syntax for the environment."
  (parse-namestring (uri-path file)))
