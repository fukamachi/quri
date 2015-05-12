(in-package :cl-user)
(defpackage quri.copy
  (:use :cl
        :quri.uri
        :quri.uri.ftp
        :quri.uri.http
        :quri.uri.ldap
        :split-sequence)
  (:import-from :alexandria
                :when-let*)
  (:import-from :quri.port
                :scheme-default-port)
  (:export :scheme-constructor
           :copy-uri
           :merge-uris))

(in-package :quri.copy)

(defun scheme-constructor (scheme)
  "Get a constructor function appropriate for the scheme."
  (cond
    ((string= scheme "http")  #'make-uri-http)
    ((string= scheme "https") #'make-uri-https)
    ((string= scheme "ldap")  #'make-uri-ldap)
    ((string= scheme "ldaps") #'make-uri-ldaps)
    ((string= scheme "ftp")   #'make-uri-ftp)
    ((string= scheme "urn")   #'make-urn)
    (T                        #'make-uri)))

(defun copy-uri (uri &key
                       (scheme (uri-scheme uri))
                       (userinfo (uri-userinfo uri))
                       (host (uri-host uri))
                       (port (uri-port uri))
                       (path (uri-path uri))
                       (query (uri-query uri))
                       (fragment (uri-fragment uri)))
  (funcall (scheme-constructor scheme)
           :scheme scheme
           :userinfo userinfo
           :host host
           :port port
           :path path
           :query query
           :fragment fragment))

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
  (declare (uri reference base))
  "Merge a reference URI into the base URI as described in RFC 2396 Section 5.2.
The returned URI may or may not be a new instance. Neither REFERENCE nor BASE is
mutated."
  ;; Step 2 -- return base if same document
  (when (and (null (uri-path reference))
             (null (uri-scheme reference))
             (null (uri-authority reference))
             (null (uri-query reference)))
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

      ;; Step 5 -- Absolute path
      (when-let* ((p (uri-path uri))
                  (first-char (and (> (length p) 0) (char p 0))))
        (when (char= #\/ first-char)
          (done)))

      ;; Step 6 -- Relative path
      (setf (uri-path uri) (merge-uri-paths (uri-path uri) (uri-path base)))

      ;; Step 7 -- Finish
      (done))))
