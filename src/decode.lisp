(in-package :cl-user)
(defpackage quri.decode
  (:use :cl
        :quri.util
        :quri.error)
  (:import-from :babel
                :octets-to-string)
  (:import-from :babel-encodings
                :*default-character-encoding*)
  (:export :url-decode))
(in-package :quri.decode)

(declaim (ftype (function (character) (unsigned-byte 4)) hexdigit-to-integer))
(defun hexdigit-to-integer (char)
  (declare (type character char)
           (optimize (speed 3) (safety 0)))
  (let ((code (char-code char)))
    (declare (type fixnum code))
    (cond
      ((<= #.(char-code #\0) code #.(char-code #\9))
       (- code #.(char-code #\0)))
      ((<= #.(char-code #\A) code #.(char-code #\F))
       (- code #.(- (char-code #\A) 10)))
      ((<= #.(char-code #\a) code #.(char-code #\f))
       (- code #.(- (char-code #\a) 10)))
      (T (error 'uri-malformed-urlencoded-string)))))

(defun url-decode (string &key
                            (encoding babel-encodings:*default-character-encoding*)
                            (start 0)
                            end)
  (declare (optimize (speed 3)))
  (check-type string string)
  (let* ((end (or end (length string)))
         (buffer (make-array (- end start)
                             :element-type '(unsigned-byte 8)))
         (i 0)
         parsing-encoded-part)
    (declare (type (simple-array (unsigned-byte 8)) buffer))
    (flet ((write-to-buffer (byte)
             (declare (optimize (speed 3) (safety 0))
                      #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
             (setf (aref buffer i) byte)
             (incf i)))
      (with-array-parsing (char p string start end)
        (parsing
         (cond
           ((char= char #\%)
            (gonext))
           (T
            (write-to-buffer (char-code char))
            (redo))))

        (parsing-encoded-part
         (setq parsing-encoded-part
               (* 16 (hexdigit-to-integer char)))
         (gonext))

        (parsing-encoded-part-second
         (write-to-buffer
          (+ parsing-encoded-part
             (hexdigit-to-integer char)))
         (goto parsing))))
    (babel:octets-to-string buffer :end i :encoding encoding)))
