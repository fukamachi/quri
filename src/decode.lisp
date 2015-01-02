(in-package :cl-user)
(defpackage quri.decode
  (:use :cl
        :quri.util
        :quri.error)
  (:import-from :babel
                :octets-to-string)
  (:import-from :babel-encodings
                :*default-character-encoding*)
  (:import-from :cl-utilities
                :collecting
                :collect)
  (:export :url-decode
           :url-decode-params))
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
      (T (error 'url-decoding-error)))))

(defun url-decode (data &key
                          (encoding babel-encodings:*default-character-encoding*)
                          (start 0)
                          end)
  (declare (type (or string (simple-array (unsigned-byte 8) (*))) data)
           (type integer start)
           (optimize (speed 3) (safety 2)))
  (let* ((end (or end (length data)))
         (buffer (make-array (- end start)
                             :element-type '(unsigned-byte 8)))
         (i 0)
         parsing-encoded-part)
    (declare (type integer end i)
             (type (simple-array (unsigned-byte 8)) buffer))
    (flet ((write-to-buffer (byte)
             (declare (optimize (speed 3) (safety 0)))
             (setf (aref buffer i) byte)
             (incf i)))
      (with-array-parsing (char p data start end (and (not (stringp data))
                                                      #'code-char))
        (parsing
         (cond
           ((char= char #\%)
            (gonext))
           ((char= char #\+)
            (write-to-buffer #.(char-code #\Space))
            (redo))
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
         (setq parsing-encoded-part nil)
         (goto parsing))

        (:eof
         (when parsing-encoded-part
           (error 'url-decoding-error)))))
    (babel:octets-to-string buffer :end i :encoding encoding)))

;; If malformed data comes in (format a==b) this dies, and that causes the web server to
;; run out of threads - wouldn't a more sensible default be to attempt a clean up vs error?
;; especially as it seems like clack/caveman2 has no way for the implentor to touch the GET/POST
;; data before it collides with this error to pre-emptively clean it up
(defun clean-up-malformed-data (data &key (delimiter #\&))
  "Some data sent in like a==b or a&&b will wreck this, so clean it up"
  (let ((last-c nil))
    (format nil "~{~a~}"
            (remove nil (loop for c across data
                           collect (unless (and (equal last-c c)
                                                (member c (list #\= delimiter)))
                                     (setf last-c c)
                                     c))))))

(defun url-decode-params (data &key
                                 (delimiter #\&)
                                 (encoding babel-encodings:*default-character-encoding*)
                                 (start 0)
                                 end)
  (declare (type (or string (simple-array (unsigned-byte 8) (*))) data)
           (type integer start)
           (type character delimiter)
           (optimize (speed 3) (safety 2)))
  (let ((end (or end (length data)))
        (start-mark nil)
        (=-mark nil)
        (data (clean-up-malformed-data data :delimiter delimiter))) ;; Clean up malformed data to avoid blocking errors
    (declare (type integer end))
    (collecting
      (flet ((collect-pair (p)
               (collect (cons (url-decode data :encoding encoding :start start-mark :end =-mark)
                              (url-decode data :encoding encoding :start (1+ =-mark) :end p)))
               (setq start-mark nil
                     =-mark nil))
             (collect-field (p)
               (collect (cons (url-decode data :encoding encoding :start start-mark :end p) nil))
               (setq start-mark nil)))
        (with-array-parsing (char p data start end (and (not (stringp data))
                                                        #'code-char))
          (start
           (when (or (char= char #\=)
                     (char= char delimiter))
             (error 'uri-malformed-urlencoded-string))
           (setq start-mark p)
           (gonext))

          (parsing-field
           (cond
             ((char= char #\=)
              (setq =-mark p)
              (gonext))
             ((char= char delimiter)
              ;; field only
              (collect-field p)
              (goto start)))
           (redo))

          (parsing-value
           (cond
             ((char= char #\=)
              (error 'uri-malformed-urlencoded-string))
             ((char= char delimiter)
              (collect-pair p)
              (goto start)))
           (redo))

          (:eof
           (cond
             (=-mark (collect-pair p))
             (start-mark (collect-field p)))))))))
