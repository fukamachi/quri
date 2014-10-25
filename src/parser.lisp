(in-package :cl-user)
(defpackage quri.parser
  (:use :cl
        :quri.error
        :quri.util)
  (:import-from :alexandria
                :with-gensyms
                :when-let)
  (:export :parse-uri))
(in-package :quri.parser)

(defun parse-uri (string)
  (check-type string string)
  (let (scheme userinfo host port path query fragment
        (len (length string)))
    (block nil
      (flet ((parse-from-path (string start)
               (multiple-value-bind (string start end)
                   (parse-path string :start start)
                 (unless (= start end)
                   (setq path (subseq string start end)))
                 (multiple-value-bind (parsed-string path-start path-end)
                     (parse-query string :start end :end len)
                   (when parsed-string
                     (setq query (subseq parsed-string path-start path-end)))
                   (multiple-value-bind (string start end)
                       (parse-fragment string :start (or path-end end) :end len)
                     (when string
                       (setq fragment (subseq string start end))))))))
        (multiple-value-bind (string start end)
            (handler-case (parse-scheme string)
              (uri-malformed-string ()
                ;; assume this is a relative uri.
                (return (parse-from-path string 0))))
          (setq scheme (make-string (- end start)))
          (do ((p start (1+ p))
               (i 0 (1+ i)))
              ((= p end))
            (setf (aref scheme i)
                  (char-upcase (aref string p))))
          (setq scheme (intern scheme :keyword))
          (multiple-value-bind (string userinfo-start userinfo-end
                                host-start host-end port-start port-end)
              (parse-authority string :start (1+ end) :end len)
            (when userinfo-start
              (setq userinfo (subseq string userinfo-start userinfo-end)))
            (unless (= host-start host-end)
              (setq host (subseq string host-start host-end)))
            (when port-start
              (handler-case
                  (setq port
                        (parse-integer string :start port-start :end port-end))
                (error ()
                  (error 'uri-invalid-port))))
            (parse-from-path string (or port-end host-end))))))
    (values scheme userinfo host port path query fragment)))

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
  (let ((authority-mark nil)
        (colon-mark nil)
        userinfo-start
        userinfo-end
        host-start
        host-end
        port-start
        port-end)
    (with-array-parsing (char p string start end)
      (declare (type character char))
      (parsing-first
       (cond
         ((char= char #\/)
          (gonext))
         (T
          (error 'uri-malformed-string))))

      (parsing-authority-starting
       (unless (char= char #\/)
         (error 'uri-malformed-string))
       (gonext))

      (parsing-authority-start
       (setq authority-mark p)
       (gonext 0))

      ;; parsing host or userinfo
      (parsing-authority
       (cond
         ((char= char #\:)
          (setq colon-mark p)
          (redo))
         ((char= char #\@)
          (when userinfo-start
            (error 'uri-malformed-string))
          (setq userinfo-start authority-mark
                userinfo-end p)
          (setq authority-mark (1+ p)
                colon-mark nil)
          (redo))
         ((or (char= char #\/)
              (char= char #\?)
              (char= char #\#))
          (go :eof))
         (T (redo))))

      (:eof
       (if colon-mark
           (setq host-start authority-mark
                 host-end colon-mark
                 port-start (1+ colon-mark)
                 port-end p)
           (setq host-start authority-mark
                 host-end p))
       (return-from parse-authority
         (values string
                 userinfo-start userinfo-end
                 host-start host-end
                 port-start port-end))))))

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
