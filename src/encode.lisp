(in-package :cl-user)
(defpackage quri.encode
  (:use :cl)
  (:import-from :babel-encodings
                :*default-character-encoding*)
  (:export :url-encode
           :url-encode-form))
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
         "!*'();:@&=+$,/?#[] ")
    ary))

(defun url-encode (string &key
                            (encoding babel-encodings:*default-character-encoding*)
                            (start 0)
                            end
                            space-to-plus)
  (declare (optimize (speed 3)))
  (check-type string string)
  (let* ((octets (babel:string-to-octets string :encoding encoding :start start :end end))
         (res (make-array (* (length octets) 3) :element-type 'character :fill-pointer t))
         (i 0))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets)
             (type string res)
             (type integer i))
    (loop for byte of-type (unsigned-byte 8) across octets do
      (cond
        ((and space-to-plus
              (= byte #.(char-code #\Space)))
         (setf (aref res i) #\+)
         (incf i))
        ((< byte #.(char-code #\a))
         (let ((reserved (aref +reserved+ byte)))
           (if (zerop (length reserved))
               (progn
                 (setf (aref res i) (code-char byte))
                 (incf i))
               (progn
                 (setf (aref res i) #\%)
                 (incf i)
                 (replace res reserved :start1 i)
                 (incf i 2)))))
        ((< byte 128)
         (setf (aref res i) (code-char byte))
         (incf i))
        (T
         (setf (aref res i) #\%)
         (incf i)
         (replace res (integer-to-hexdigit byte) :start1 i)
         (incf i 2))))
    (setf (fill-pointer res) i)
    res))

(defun url-encode-form (form-alist &key (encoding babel-encodings:*default-character-encoding*)
                                     space-to-plus)
  (declare (optimize (speed 3)))
  (check-type form-alist list)
  (with-output-to-string (s)
    (loop for ((field . value) . rest) on form-alist do
      (write-string (url-encode field :encoding encoding :space-to-plus space-to-plus) s)
      (when value
        (write-char #\= s)
        (write-string (url-encode value :encoding encoding :space-to-plus space-to-plus) s))
      (when rest
        (write-char #\& s)))))
