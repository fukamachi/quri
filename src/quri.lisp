(in-package :cl-user)
(defpackage quri
  (:use :cl)
  (:import-from :quri.parser
                :parse-uri)
  (:import-from :quri.decode
                :url-decode)
  (:import-from :quri.encode
                :url-encode)
  (:import-from :quri.port
                :scheme-default-port)
  (:export :parse-uri
           :uri
           :uri-scheme
           :uri-userinfo
           :uri-host
           :uri-port
           :uri-path
           :uri-query
           :uri-fragment
           :uri-authority
           :ftp-typecode
           :urn-nid
           :urn-nss

           :render-uri

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

(defun uri-authority (uri)
  (when (uri-host uri)
    (let ((default-port (scheme-default-port (uri-scheme uri))))
      (format nil "~:[~;~:*~A@~]~A~:[:~A~;~*~]"
              (uri-userinfo uri)
              (uri-host uri)
              (eql (uri-port uri) default-port)
              (uri-port uri)))))

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

(defstruct (urn (:include uri (scheme :urn))
                (:constructor %make-urn))
  nid
  nss)

(defun make-urn (&rest initargs)
  (let ((urn (apply #'%make-urn initargs)))
    (when (uri-path urn)
      (let ((colon-pos (position #\: (uri-path urn))))
        (if colon-pos
            (setf (urn-nid urn) (subseq (uri-path urn) 0 colon-pos)
                  (urn-nss urn) (subseq (uri-path urn) (1+ colon-pos)))
            (setf (urn-nid urn) (uri-path urn)))))
    urn))

(defun uri (uri-string)
  (multiple-value-bind (scheme userinfo host port path query fragment)
      (parse-uri uri-string)
    (funcall (cond
               ((eq scheme :ftp)   #'make-ftp)
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
  (if (ftp-p uri)
      (format stream
              "~:[~;~:*~(~A~):~]~:[~;~:*//~A~]~:[~;~:*~A~]~:[~;~:*;type=~A~]~:[~;~:*?~A~]~:[~;~:*#~A~]"
              (uri-scheme uri)
              (uri-authority uri)
              (uri-path uri)
              (ftp-typecode uri)
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
