(in-package :cl-user)
(defpackage quri.parser
  (:use :cl
        :quri.error
        :quri.util)
  (:import-from :alexandria
                :with-gensyms)
  (:export :parse-uri))
(in-package :quri.parser)

(defun parse-uri (string)
  (check-type string string)
  (let (scheme authority path query fragment
        (len (length string)))
    (block nil
      (flet ((parse-from-path (string start)
               (multiple-value-bind (string start end)
                   (parse-path string :start start)
                 (setq path (subseq string start end))
                 (multiple-value-bind (string start end)
                     (parse-query string :start end :end len)
                   (when string
                     (setq query (subseq string start end))
                     (multiple-value-bind (string start end)
                         (parse-fragment string :start end :end len)
                       (when string
                         (setq fragment (subseq string start end)))))))))
        (multiple-value-bind (string start end)
            (handler-case (parse-scheme string)
              (uri-malformed-string ()
                ;; assume this is a relative uri.
                (return (parse-from-path string 0))))
          (setq scheme (subseq string start end))
          (multiple-value-bind (string start end)
              (parse-authority string :start (1+ end) :end len)
            (setq authority (subseq string start end))
            (parse-from-path string end)))))
    (values scheme authority path query fragment)))

(defun parse-scheme (string &key (start 0) (end (length string)))
  (declare (type string string)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (flet ((scheme-char-p (char)
           (or (alphanumericp char)
               (char= char #\+)
               (char= char #\-)
               (char= char #\.))))
    (with-array-parsing (char p string start end)
      (declare (type character char))
      (parsing-scheme-start
       (unless (alpha-char-p char)
         (error 'uri-invalid-scheme))
       (gonext))

      (parsing-scheme
       (cond
         ((char= char #\:)
          (return-from parse-scheme
            (values string 0 p)))
         ((scheme-char-p char)
          (redo))
         (T
          (error 'uri-invalid-scheme))))
      (:eof (error 'uri-malformed-string)))))

(defun parse-authority (string &key (start 0) (end (length string)))
  (declare (type string string)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((authority-mark nil))
    (with-array-parsing (char p string start end)
      (declare (type character char))
      (parsing-first
       (cond
         ((char= char #\/)
          (gonext))
         (T
          (return-from parse-authority
            (values string start p)))))

      (parsing-authority-starting
       (unless (char= char #\/)
         (error 'uri-malformed-string))
       (gonext))

      (parsing-authority-start
       (setq authority-mark p)
       (gonext 0))

      (parsing-authority
       (cond
         ((or (char= char #\/)
              (char= char #\?)
              (char= char #\#))
          (return-from parse-authority
            (values string authority-mark p)))
         (T (redo))))

      (:eof
       (return-from parse-authority
         (values string (or authority-mark p) end))))))

(defmacro parse-until (delimiters string &key (start 0) (end (length string)))
  (with-gensyms (p char)
    `(block nil
       (progn
         (do ((,p ,start (1+ ,p)))
             ((= ,p ,end)
              (values ,string ,start ,end))
           (let ((,char (aref ,string ,p)))
             (declare (type character ,char))
             (when (or ,@(loop for delim in delimiters
                               collect `(char= ,delim ,char)))
               (return (values ,string ,start ,p)))))))))

(defun parse-path (string &key (start 0) (end (length string)))
  (declare (type string string)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (parse-until (#\? #\#) string :start start :end end))

(defun parse-query (string &key (start 0) (end (length string)))
  (declare (type string string)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((?-pos (position #\? string :start start :end end)))
    (when ?-pos
      (parse-until (#\#) string :start (1+ ?-pos) :end end))))

(defun parse-fragment (string &key (start 0) (end (length string)))
  (let ((|#-pos| (position #\# string :start start :end end)))
    (when |#-pos|
      (values string (1+ |#-pos|) end))))
