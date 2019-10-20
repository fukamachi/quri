(in-package :cl-user)
(defpackage quri
  (:use :cl
        :quri.uri
        :quri.uri.ftp
        :quri.uri.http
        :quri.uri.ldap
        :quri.uri.file
        :quri.error)
  (:import-from :quri.domain
                :uri-tld
                :uri-domain
                :ipv4-addr-p
                :ipv6-addr-p
                :ip-addr-p
                :ip-addr=
                :cookie-domain-p)
  (:import-from :quri.parser
                :parse-uri
                :parse-scheme
                :parse-authority
                :parse-path
                :parse-query
                :parse-fragment)
  (:import-from :quri.port
                :scheme-default-port)
  (:import-from :quri.decode
                :url-decode
                :url-decode-params)
  (:import-from :quri.encode
                :url-encode
                :url-encode-params)
  (:import-from :split-sequence :split-sequence)
  (:import-from :alexandria
                :delete-from-plist
                :when-let*)
  (:export :parse-uri
           :parse-scheme
           :parse-authority
           :parse-path
           :parse-query
           :parse-fragment

           :make-uri
           :uri
           :uri=
           :uri-p
           :uri-scheme
           :uri-userinfo
           :uri-host
           :uri-port
           :uri-path
           :uri-query
           :uri-fragment
           :uri-authority

           :uri-tld
           :uri-domain
           :ipv4-addr-p
           :ipv6-addr-p
           :ip-addr-p
           :ip-addr=
           :cookie-domain-p

           :urn
           :urn-p
           :urn-nid
           :urn-nss

           :uri-ftp
           :uri-ftp-p
           :uri-ftp-typecode

           :uri-http
           :uri-http-p
           :uri-query-params

           :uri-ldap
           :uri-ldap-p
           :uri-ldap-dn
           :uri-ldap-attributes
           :uri-ldap-scope
           :uri-ldap-filter
           :uri-ldap-extensions

           :uri-file
           :uri-file-p
           :uri-file-pathname

           :copy-uri
           :render-uri
           :merge-uris

           :url-decode
           :url-decode-params
           :url-encode
           :url-encode-params

           :uri-error
           :uri-malformed-string
           :uri-invalid-port
           :url-decoding-error
           :uri-malformed-urlencoded-string))
(in-package :quri)

(defun scheme-constructor (scheme)
  "Get a constructor function appropriate for the scheme."
  (cond
    ((string= scheme "http")  #'make-uri-http)
    ((string= scheme "https") #'make-uri-https)
    ((string= scheme "ldap")  #'make-uri-ldap)
    ((string= scheme "ldaps") #'make-uri-ldaps)
    ((string= scheme "ftp")   #'make-uri-ftp)
    ((string= scheme "file")  #'make-uri-file)
    ((string= scheme "urn")   #'make-urn)
    (t                        #'make-basic-uri)))

(defun uri (data &key (start 0) end)
  (if (uri-p data)
      data
      (multiple-value-bind (scheme userinfo host port path query fragment)
          (parse-uri data :start start :end end)
        (apply (scheme-constructor scheme)
               :scheme scheme
               :userinfo userinfo
               :host host
               :path path
               :query query
               :fragment fragment

               (and port
                    `(:port ,port))))))

(defun copy-uri (uri &key
                       (scheme (uri-scheme uri))
                       (userinfo (uri-userinfo uri))
                       (host (uri-host uri))
                       (port (uri-port uri))
                       (path (uri-path uri))
                       (query (uri-query uri))
                       (fragment (uri-fragment uri)))
  (make-uri :scheme scheme
            :userinfo userinfo
            :host host
            :port port
            :path path
            :query query
            :fragment fragment))

(defun make-uri (&rest initargs &key scheme userinfo host port path query fragment defaults)
  (declare (ignore userinfo host port path fragment))
  (setf initargs (delete-from-plist initargs :defaults))
  (if defaults
      (apply #'copy-uri (uri defaults) initargs)
      (progn
        (when (consp query)
          (setf (getf initargs :query) (url-encode-params query)))
        (apply (scheme-constructor scheme) initargs))))

(defun render-uri (uri &optional stream)
  (cond
    ((uri-ftp-p uri)
     (format stream
             "~@[~(~A~):~]~@[//~(~A~)~]~@[~A~]~@[;type=~A~]~@[?~A~]~@[#~A~]"
             (uri-scheme uri)
             (uri-authority uri)
             (uri-path uri)
             (uri-ftp-typecode uri)
             (uri-query uri)
             (uri-fragment uri)))
    ((uri-file-p uri)
     (format stream
             "~@[~(~A~)://~]~@[~(~a~)~]"
             (uri-scheme uri)
             (uri-path uri)))
    (t
     (format stream
             "~@[~(~A~):~]~@[//~(~A~)~]~@[~A~]~@[?~A~]~@[#~A~]"
             (uri-scheme uri)
             (uri-authority uri)
             (uri-path uri)
             (uri-query uri)
             (uri-fragment uri)))))

(defun uri= (uri1 uri2)
  (check-type uri1 uri)
  (check-type uri2 uri)
  (when (eq (type-of uri1) (type-of uri2))
    (and (equalp (uri-scheme uri1)    (uri-scheme uri2))
         ;; An empty path should be normalized to a path of "/".
         ;; RFC 3986 (https://tools.ietf.org/html/rfc3986#section-6.2.3)
         (equal  (or (uri-path uri1) "/") (or (uri-path uri2) "/"))
         (equal  (uri-query uri1)     (uri-query uri2))
         (equal  (uri-fragment uri1)  (uri-fragment uri2))
         (equalp (uri-authority uri1) (uri-authority uri2))
         (or (not (uri-ftp-p uri1))
             (eql (uri-ftp-typecode uri1) (uri-ftp-typecode uri2))))))

(defmethod print-object ((uri uri) stream)
  (if (and (null *print-readably*) (null *print-escape*))
      (render-uri uri stream)
      (format stream "#<~S ~A>"
              (type-of uri)
              (render-uri uri))))


(defun merge-uri-paths (ref-path base-path)
  (declare (type (or string null) ref-path base-path))
  (let* ((path-list (and base-path (nreverse (split-sequence #\/ base-path))))
         (ref-components (and ref-path (split-sequence #\/ ref-path)))
         ending-slash-p)
    ;; remove last component of base
    (pop path-list)
    (dolist (component ref-components)
      (cond ((string= ".." component)
             (pop path-list)
             (setf ending-slash-p t))
            ((string= "." component)
             (setf ending-slash-p t))
            (t
             (push component path-list)
             (setf ending-slash-p nil))))
    (setf path-list (nreverse path-list))
    (with-output-to-string (s)
      (loop for (component . more) on path-list
         do (progn
              (write-string component s)
              (when (or more ending-slash-p)
                (write-char #\/ s)))))))

(defun merge-uris (reference base)
  (let ((reference (uri reference))
        (base (uri base)))
    (declare (uri reference base))
    "Merge a reference URI into the base URI as described in RFC 2396 Section 5.2.
The returned URI may or may not be a new instance. Neither REFERENCE nor BASE is
mutated."
    ;; Step 2 -- return base if same document
    (when (uri= reference base)
      (return-from merge-uris base))

    ;; Step 3 -- scheme
    (when (uri-scheme reference)
      (return-from merge-uris reference))
    (let ((uri (copy-uri reference :scheme (uri-scheme base))))
      (when (null (uri-port uri))
        (setf (uri-port uri) (scheme-default-port (uri-scheme uri))))
      (macrolet ((done () '(return-from merge-uris uri)))

        ;; Step 4 -- Authority
        (when (uri-host uri)
          (done))
        (setf (uri-userinfo uri) (uri-userinfo base))
        (setf (uri-host uri) (uri-host base))
        (setf (uri-port uri) (uri-port base))

        ;; Step 5 -- Empty path
        (when (null (uri-path uri))
          (setf (uri-path uri) (uri-path base))
          (done))

        ;; Step 6 -- Absolute path
        (alexandria:when-let* ((p (uri-path uri))
                               (first-char (and (> (length p) 0) (char p 0))))
                              (when (char= #\/ first-char)
                                (done)))

        ;; Step 7 -- Relative path
        (setf (uri-path uri) (merge-uri-paths (uri-path uri) (uri-path base)))

        ;; Step 8 -- Finish
        (done)))))
