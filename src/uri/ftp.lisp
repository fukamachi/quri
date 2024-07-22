(in-package :cl-user)
(defpackage quri.uri.ftp
  (:use :cl)
  (:import-from :quri.uri
                :uri
                :scheme
                :port
                :uri-path)
  (:import-from :quri.port
                :scheme-default-port)
  (:import-from #:alexandria
		#:positive-integer)
  (:import-from #:closer-mop
		#:defclass)
  (:export :uri-ftp
           :uri-ftp-p
           :uri-ftp-typecode
           :make-uri-ftp))
(in-package :quri.uri.ftp)

(defclass uri-ftp (uri)
  ((scheme :initform "ftp"
	   :initarg :scheme
	   :accessor uri-ftp-scheme
	   :type string)
   (port :initform #.(scheme-default-port "ftp")
	 :initarg :port
	 :accessor uri-ftp-port
	 :type (or null positive-integer))
   (typecode :initform nil
	     :initarg :typecode
	     :accessor uri-ftp-typecode
	     :type (or null string))))

(declaim (ftype (function (&rest t &key (:scheme string) (:userinfo string)
				 (:host string) (:port positive-integer) (:path (or string pathname))
				 (:query string) (:fragment string) (:typecode string)))
		make-uri-ftp))
(defun make-uri-ftp (&rest initargs &key scheme userinfo host port path query fragment typecode)
  (declare (ignore scheme userinfo host port path query fragment typecode))
  (let ((ftp (apply #'make-instance (cons 'uri-ftp initargs))))
    (multiple-value-bind (path typecode)
        (parse-ftp-typecode (uri-path ftp))
      (when path
        (setf (uri-path ftp) path
              (uri-ftp-typecode ftp) typecode)))
    ftp))

(declaim (ftype (function (t) boolean) uri-ftp-p))
(defun uri-ftp-p (uri)
  (typep uri (find-class 'uri-ftp)))

(defun parse-ftp-typecode (path)
  (let ((len (length path)))
    (when (and (< #.(length ";type=") len)
               (string= path ";type="
                        :start1 (- len 1 #.(length ";type="))
                        :end1 (1- len)))
      (let ((typecode (aref path (1- len))))
        (when (or (char= typecode #\a)
                  (char= typecode #\i)
                  (char= typecode #\d))
          (values (subseq path 0 (- len #.(1+ (length ";type="))))
                  typecode))))))
