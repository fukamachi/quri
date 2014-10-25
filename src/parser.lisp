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
  (declare (type string string)
           (optimize (speed 3) (safety 2)))
  (let (scheme userinfo host port path query fragment
        (len (length string)))
    (declare (type integer len))
    (block nil
      (flet ((parse-from-path (string start)
               (declare (type integer start))
               (multiple-value-bind (string start end)
                   (parse-path string :start start)
                 (declare (type integer start end)
                          (type string string))
                 (unless (= start end)
                   (setq path (subseq string start end)))
                 (multiple-value-bind (parsed-string path-start path-end)
                     (parse-query string :start end :end len)
                   (when parsed-string
                     (setq query (subseq (the string parsed-string) (the integer path-start) (the integer path-end))))
                   (multiple-value-bind (string start end)
                       (parse-fragment string :start (or path-end end) :end len)
                     (when string
                       (setq fragment (subseq (the string string) (the integer start) (the integer end)))))))))
        (multiple-value-bind (string start end)
            (handler-case (parse-scheme string)
              (uri-malformed-string ()
                ;; assume this is a relative uri.
                (return (parse-from-path string 0))))
          (declare (type string string)
                   (type integer start end))
          (setq scheme
                (cond
                  ((string-equal string "http"
                                 :start1 start :end1 end)
                   :http)
                  ((string-equal string "https"
                                 :start1 start :end1 end)
                   :https)
                  (T (intern (string-upcase (subseq string start end)) :keyword))))
          (incf end)
          (unless (= end len)
            (multiple-value-bind (string userinfo-start userinfo-end
                                  host-start host-end port-start port-end)
                (parse-authority string :start end :end len)
              (declare (type string string)
                       (type integer host-start host-end))
              (when userinfo-start
                (setq userinfo (subseq string (the integer userinfo-start) (the integer userinfo-end))))
              (unless (= host-start host-end)
                (setq host (subseq string host-start host-end)))
              (when port-start
                (handler-case
                    (setq port
                          (parse-integer string :start (the integer port-start) :end (the integer port-end)))
                  (error ()
                    (error 'uri-invalid-port))))
              (parse-from-path string (or port-end host-end)))))))
    (values scheme userinfo host port path query fragment)))

(declaim (inline scheme-char-p))
(defun scheme-char-p (char)
  (declare (type character char)
           (optimize (speed 3) (safety 0)))
  (or (alphanumericp char)
      (char= char #\+)
      (char= char #\-)
      (char= char #\.)))

(defun parse-scheme (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type integer start end)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-array-parsing (char p string start end)
    (declare (type character char)
             (type integer p))
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
    (:eof (error 'uri-malformed-string))))

(defun parse-authority (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type integer start end)
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
      (declare (type character char)
               (type integer p))
      (parsing-first
       (cond
         ((char= char #\/)
          (gonext))
         (T
          (return-from parse-authority
            (values string nil nil start start nil nil)))))

      (parsing-authority-starting
       (unless (char= char #\/)
         (error 'uri-malformed-string))
       (gonext))

      (parsing-authority-start
       (setq authority-mark p)
       (if (char= char #\[)
           (goto parsing-ipliteral)
           (gonext 0)))

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

      (parsing-ipliteral
       (if (char= char #\])
           (goto parsing-authority)
           (redo)))

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
           (declare (type integer ,p))
           (let ((,char (aref ,string ,p)))
             (declare (type character ,char))
             (when (or ,@(loop for delim in delimiters
                               collect `(char= ,delim ,char)))
               (return (values ,string ,start ,p)))))))))

(defun parse-path (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type integer start end)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (parse-until (#\? #\#) string :start start :end end))

(defun parse-query (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type integer start end)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((?-pos (position #\? string :start start :end end)))
    (when ?-pos
      (parse-until (#\#) string :start (1+ (the integer ?-pos)) :end end))))

(defun parse-fragment (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type integer start end)
           (optimize (speed 3) (safety 2)))
  (let ((|#-pos| (position #\# string :start start :end end)))
    (when |#-pos|
      (values string (1+ (the integer |#-pos|)) end))))
