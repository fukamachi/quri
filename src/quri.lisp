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
           :ftp-typecode

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
(defstruct (ftp (:include uri (scheme :ftp) (port 21))
                (:constructor %make-ftp))
  typecode)

(defun make-ftp (&rest initargs)
  (let ((ftp (apply #'%make-ftp initargs)))
    (multiple-value-bind (path typecode)
        (parse-ftp-typecode (uri-path ftp))
      (when path
        (setf (uri-path ftp) path
              (ftp-typecode ftp) typecode)))
    ftp))

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

(defun uri (uri-string)
  (multiple-value-bind (scheme userinfo host port path query fragment)
      (parse-uri uri-string)
    (let ((uri (funcall (cond
                          ((eq scheme :ftp)   #'make-ftp)
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
