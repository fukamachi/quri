(in-package :cl-user)
(defpackage quri.parser
  (:use :cl
        :quri.error
        :quri.util)
  (:import-from :alexandria
                :with-gensyms)
  (:export :parse-uri))
(in-package :quri.parser)

(deftype simple-byte-vector () '(simple-array (unsigned-byte 8) (*)))

(defun convert-to-scheme-keyword (data start end)
  (etypecase data
    (string
     (cond ((string-equal data "http"
                          :start1 start :end1 end)
            :http)
           ((string-equal data "https"
                          :start1 start :end1 end)
            :https)
           (T (intern (string-upcase (subseq data start end)) :keyword))))
    (simple-byte-vector
     (let ((data-str (make-string (- end start))))
       (do ((i start (1+ i))
            (j 0 (1+ j)))
           ((= i end))
         (let ((code (aref data i)))
           (setf (aref data-str j)
                 (code-char
                  (if (<= #.(char-code #\a) code #.(char-code #\z))
                      (- code 32)
                      code)))))
       (cond ((string-equal data-str "http")
              :http)
             ((string-equal data-str "https")
              :https)
             (T (intern data-str :keyword)))))))

(defun parse-uri (data &key (start 0) end)
  (declare (optimize (speed 3) (safety 2)))
  (let (scheme userinfo host port path query fragment
        (subseq-fn (etypecase data
                     (string #'subseq)
                     (simple-byte-vector (lambda (data &optional start end)
                                           (babel:octets-to-string data :start start :end end)))))
        (parse-start start)
        (parse-end (or end (length data))))
    (declare (type integer parse-start parse-end))
    (flet ((subseq* (&rest args)
             (apply subseq-fn args)))
      (block nil
        (flet ((parse-from-path (data start)
                 (declare (type integer start))
                 (multiple-value-bind (data start end)
                     (parse-path data :start start)
                   (declare (type integer start end))
                   (unless (= start end)
                     (setq path (subseq* data start end)))
                   (multiple-value-bind (parsed-data path-start path-end)
                       (parse-query data :start end :end parse-end)
                     (when parsed-data
                       (setq query (subseq* parsed-data (the integer path-start) (the integer path-end))))
                     (multiple-value-bind (data start end)
                         (parse-fragment data :start (or path-end end) :end parse-end)
                       (when data
                         (setq fragment (subseq* data (the integer start) (the integer end)))))))))
          (multiple-value-bind (data start end)
              (handler-case (parse-scheme data :start parse-start :end parse-end)
                (uri-malformed-string ()
                  ;; assume this is a relative uri.
                  (return (parse-from-path data parse-start))))
            (declare (type integer start end))
            (setq scheme
                  (convert-to-scheme-keyword data start end))
            (incf end)
            (unless (= end parse-end)
              (multiple-value-bind (data userinfo-start userinfo-end
                                    host-start host-end port-start port-end)
                  (parse-authority data :start end :end parse-end)
                (declare (type integer host-start host-end))
                (when userinfo-start
                  (setq userinfo (subseq* data (the integer userinfo-start) (the integer userinfo-end))))
                (unless (= host-start host-end)
                  (setq host (subseq* data host-start host-end)))
                (when port-start
                  (handler-case
                      (setq port
                            (parse-integer data :start (the integer port-start) :end (the integer port-end)))
                    (error ()
                      (error 'uri-invalid-port))))
                (parse-from-path data (or port-end host-end))))))))
    (values scheme userinfo host port path query fragment)))

(defmacro defun-with-array-parsing (name (char p data start end &rest other-args) &body body)
  (with-gensyms (args)
    `(progn
       (defun ,name (,data &rest ,args &key ,start ,end)
         (declare (ignore ,start ,end))
         (etypecase ,data
           (string (apply ',(intern (format nil "~A-~A" name :string)) data ,args))
           (simple-byte-vector (apply ',(intern (format nil "~A-~A" name :byte-vector)) data ,args))))

       (defun ,(intern (format nil "~A-~A" name :string)) (,data &key (,start 0) (,end (length ,data)) ,@other-args)
         (declare (type string ,data)
                  (type integer ,start ,end)
                  (optimize (speed 3) (safety 2))
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (block ,name
           (with-array-parsing (,char ,p ,data ,start ,end)
             (declare (type integer ,p))
             ,@body)))

       (defun ,(intern (format nil "~A-~A" name :byte-vector)) (,data &key (,start 0) (,end (length ,data)) ,@other-args)
         (declare (type simple-byte-vector ,data)
                  (type integer ,start ,end)
                  (optimize (speed 3) (safety 2))
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (block ,name
           (with-array-parsing (,char ,p ,data ,start ,end #'code-char)
             (declare (type integer ,p))
             ,@body))))))

(declaim (inline scheme-char-p))
(defun scheme-char-p (char)
  (declare (type character char)
           (optimize (speed 3) (safety 0)))
  (or (standard-alphanumeric-p char)
      (char= char #\+)
      (char= char #\-)
      (char= char #\.)))

(defun-with-array-parsing parse-scheme (char p data start end)
  (parsing-scheme-start
   (unless (standard-alpha-char-p char)
     (error 'uri-invalid-scheme))
   (gonext))

  (parsing-scheme
   (cond
     ((char= char #\:)
      (return-from parse-scheme
        (values data start p)))
     ((scheme-char-p char)
      (redo))
     (T
      (error 'uri-invalid-scheme))))
  (:eof (error 'uri-malformed-string)))

(defun-with-array-parsing parse-authority (char p data start end
                                                &aux
                                                (authority-mark nil)
                                                (colon-mark nil)
                                                userinfo-start
                                                userinfo-end
                                                host-start
                                                host-end
                                                port-start
                                                port-end)
  (parsing-first
   (cond
     ((char= char #\/)
      (gonext))
     (T
      (return-from parse-authority
        (values data nil nil start start nil nil)))))

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
     (values data
             userinfo-start userinfo-end
             host-start host-end
             port-start port-end))))

(defmacro parse-until-string (delimiters data &key start end)
  (with-gensyms (p char)
    `(block nil
       (progn
         (do ((,p ,start (1+ ,p)))
             ((= ,p ,end)
              (values ,data ,start ,end))
           (declare (type integer ,p))
           (let ((,char (aref ,data ,p)))
             (declare (type character ,char))
             (when (or ,@(loop for delim in delimiters
                               collect `(char= ,delim ,char)))
               (return (values ,data ,start ,p)))))))))

(defmacro parse-until-byte-vector (delimiters data &key start end)
  (with-gensyms (p byte)
    `(block nil
       (progn
         (do ((,p ,start (1+ ,p)))
             ((= ,p ,end)
              (values ,data ,start ,end))
           (declare (type integer ,p))
           (let ((,byte (aref ,data ,p)))
             (declare (type (unsigned-byte 8) ,byte))
             (when (or ,@(loop for delim in delimiters
                               collect `(= ,(char-code delim) ,byte)))
               (return (values ,data ,start ,p)))))))))

(defun parse-path (data &key (start 0) (end (length data)))
  (etypecase data
    (string
     (parse-path-string data :start start :end end))
    (simple-byte-vector
     (parse-path-byte-vector data :start start :end end))))

(defun parse-path-string (data &key (start 0) (end (length data)))
  (declare (type string data)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (parse-until-string (#\? #\#) data :start start :end end))

(defun parse-path-byte-vector (data &key (start 0) (end (length data)))
  (declare (type simple-byte-vector data)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (parse-until-byte-vector (#\? #\#) data :start start :end end))

(defun parse-query (data &key (start 0) (end (length data)))
  (etypecase data
    (string
     (parse-query-string data :start start :end end))
    (simple-byte-vector
     (parse-query-byte-vector data :start start :end end))))

(defun parse-query-string (data &key (start 0) (end (length data)))
  (declare (type string data)
           (type integer start end)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((?-pos (position #\? data :start start :end end)))
    (when ?-pos
      (parse-until-string (#\#) data :start (1+ (the integer ?-pos)) :end end))))

(defun parse-query-byte-vector (data &key (start 0) (end (length data)))
  (declare (type simple-byte-vector data)
           (type integer start end)
           (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((?-pos (position #.(char-code #\?) data :start start :end end)))
    (when ?-pos
      (parse-until-byte-vector (#\#) data :start (1+ (the integer ?-pos)) :end end))))

(defun parse-fragment (data &key (start 0) (end (length data)))
  (declare (type integer start end)
           (optimize (speed 3) (safety 2)))
  (let ((|#-pos| (position #\# data
                           :start start
                           :end end
                           :key (etypecase data
                                  (string #'identity)
                                  (simple-byte-vector #'code-char)))))
    (when |#-pos|
      (values data (1+ (the integer |#-pos|)) end))))
