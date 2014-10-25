(in-package :cl-user)
(defpackage quri.encode
  (:use :cl
        :quri.util)
  (:import-from :babel-encodings
                :*default-character-encoding*)
  (:export :url-encode))
(in-package :quri.encode)

(declaim (type (simple-array character (16)) +hexdigit-char+))
(defvar +hexdigit-char+
  (let ((ary (make-array 16 :element-type 'character)))
    (loop for char across "0123456789ABCDEF"
          for i from 0
          do (setf (aref ary i) char))
    ary))

(defun integer-to-hexdigit (byte)
  (declare (type (unsigned-byte 8) byte)
           (optimize (speed 3) (safety 0)))
  (let ((res (make-string 2)))
    (multiple-value-bind (quotient remainder)
        (floor byte 16)
      (setf (aref res 0) (aref +hexdigit-char+ quotient)
            (aref res 1) (aref +hexdigit-char+ remainder)))
    res))

(declaim (type (simple-array string (94)) +reserved+))
(defvar +reserved+
  (let ((ary (make-array 94 :element-type 'string
                         :initial-element "")))
    (map nil
         (lambda (char)
           (setf (aref ary (char-code char))
                 (integer-to-hexdigit (char-code char))))
         "!*'();:@&=+$,/?#[]")
    ary))

(defun url-encode (string &key
                            (encoding babel-encodings:*default-character-encoding*)
                            (start 0)
                            end)
  (declare (optimize (speed 3)))
  (check-type string string)
  (let ((octets (babel:string-to-octets string :encoding encoding :start start :end end)))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets))
    (with-output-to-string (s)
      (loop for byte of-type (unsigned-byte 8) across octets do
        (cond
          ((< byte 97)
           (let ((reserved (aref +reserved+ byte)))
             (if (zerop (length reserved))
                 (write-char (code-char byte) s)
                 (progn
                   (write-char #\% s)
                   (write-sequence reserved s)))))
          ((< byte 128)
           (write-char (code-char byte) s))
          (T
           (write-char #\% s)
           (write-sequence (integer-to-hexdigit byte) s)))))))
