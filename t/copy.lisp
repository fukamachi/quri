(in-package :cl-user)
(defpackage quri-test.copy
  (:use :cl
        :quri.copy
        :prove)
  (:import-from :quri
                :uri
                :uri=
                :render-uri)
  (:import-from :quri.uri
                :make-uri))

(in-package :quri-test.copy)
(plan nil)

(defparameter *base-uri* (uri "http://www.example.com/path/a/b.html"))

(defparameter *merge-test-cases*
  `((,(uri "file:///tmp/junk.txt") . "file:///tmp/junk.txt") ; the output should probably have three slashes
    (,(make-uri :userinfo "auth" :host "secretplace.com") . "http://auth@secretplace.com")
    (,(make-uri :host "example.com" :path "/path" :query "query") . "http://example.com/path?query")
    (,(uri "/new/path") . "http://www.example.com/new/path")
    (,(uri "foo.txt") . "http://www.example.com/path/a/foo.txt")
    (,(uri "../bar/") . "http://www.example.com/path/bar/")
    (,(uri "other/./car") . "http://www.example.com/path/a/other/car")
    (,(uri "./../.") . "http://www.example.com/path/")))

(subtest "merge-uris"
  (loop for (test-uri . result-uri) in *merge-test-cases* do
       (let ((merged-uri (merge-uris test-uri *base-uri*)))
         (is (render-uri merged-uri) result-uri :test 'string=))))

(finalize)

(in-package :cl-user)
