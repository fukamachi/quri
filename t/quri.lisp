(in-package :cl-user)
(defpackage quri-test
  (:use :cl
        :quri
        :prove))
(in-package :quri-test)

(plan nil)

(subtest "uri="
  (let ((prove:*default-test-function* #'uri=))
    (is (uri "http://b.hatena.ne.jp")
        (uri "http://b.hatena.ne.jp:80"))
    (is (uri "http://b.hatena.ne.jp")
        (make-uri :scheme "http" :host "b.hatena.ne.jp"))
    (is (uri "http://b.hatena.ne.jp")
        (uri "http://b.hatena.ne.jp/"))
    (isnt (uri "http://b.hatena.ne.jp/")
          (uri "http://b.hatena.ne.jp/?"))
    (isnt (uri "http//b.hatena.ne.jp/#foo")
          (uri "http//b.hatena.ne.jp/#bar"))
    #+todo
    (is (uri "mailto:Joe@Example.COM")
        (uri "mailto:Joe@example.com"))
    #+todo
    (is (uri "mailto:Postmaster@example.com")
        (uri "mailto:POSTMASTER@example.com"))))

(defparameter *test-cases*
  '(("file:///tmp/junk.txt" .
     ("file" nil nil "/tmp/junk.txt" nil nil))
    ("imap://mail.common-lisp.net/mbox1" .
     ("imap" nil "mail.common-lisp.net" "/mbox1" nil nil))
    ("mms://wms.sys.hinet.net/cts/Drama/09006251100.asf" .
     ("mms" nil "wms.sys.hinet.net" "/cts/Drama/09006251100.asf" nil nil))
    ("nfs://server/path/to/file.txt" .
     ("nfs" nil "server" "/path/to/file.txt" nil nil))
    ("svn+ssh://svn.zope.org/repos/main/ZConfig/trunk/" .
     ("svn+ssh" nil "svn.zope.org" "/repos/main/ZConfig/trunk/" nil nil))
    ("git+ssh://git@github.com/user/project.git" .
     ("git+ssh" "git" "github.com" "/user/project.git" nil nil))
    ("http://common-lisp.net" .
     ("http" nil "common-lisp.net" nil nil nil))
    ("http://common-lisp.net#abc" .
     ("http" nil "common-lisp.net" nil nil "abc"))
    ("http://common-lisp.net?q=abc" .
     ("http" nil "common-lisp.net" nil "q=abc" nil))
    ("http://common-lisp.net/#abc" .
     ("http" nil "common-lisp.net" "/" nil "abc"))
    ("http://a/b/c/d;p?q#f" .
     ("http" nil "a" "/b/c/d;p" "q" "f"))
    ("http" .
     (nil nil nil "http" nil nil))
    ("http:" .
     ("http" nil nil nil nil nil))
    ("ldap://[2001:db8::7]/c=GB?objectClass?one" .
     ("ldap" nil "[2001:db8::7]" "/c=GB" "objectClass?one" nil))
    ("http://[dead:beef::]:/foo/" .
     ("http" nil "[dead:beef::]" "/foo/" nil nil))
    ("tel:+31-641044153" .
     ("tel" nil nil "+31-641044153" nil nil))
    ("http://" .
     ("http" nil nil nil nil nil))))

(loop for (test-uri . params) in *test-cases* do
  (subtest (format nil "~A (string)" test-uri)
    (let ((uri (uri test-uri)))
      (is (uri-scheme uri)   (nth 0 params) "scheme")
      (is (uri-userinfo uri) (nth 1 params) "userinfo")
      (is (uri-host uri)     (nth 2 params) "host")
      (is (uri-path uri)     (nth 3 params) "path")
      (is (uri-query uri)    (nth 4 params) "query")
      (is (uri-fragment uri) (nth 5 params) "fragment")))
  (subtest (format nil "~A (byte-vector)" test-uri)
    (let ((uri (uri (babel:string-to-octets test-uri))))
      (is (uri-scheme uri)   (nth 0 params) "scheme")
      (is (uri-userinfo uri) (nth 1 params) "userinfo")
      (is (uri-host uri)     (nth 2 params) "host")
      (is (uri-path uri)     (nth 3 params) "path")
      (is (uri-query uri)    (nth 4 params) "query")
      (is (uri-fragment uri) (nth 5 params) "fragment")))
  (subtest (format nil "~A (copy-uri)" test-uri)
    (let ((uri (uri test-uri)))
      (is uri (copy-uri uri) :test #'uri=))))

#+nil
(is-error (uri "//www.youtube.com/embed/”6j0LpmSdWg4”")
          'uri-malformed-string)

(defparameter *base-uri* (uri "http://www.example.com/path/a/b.html"))

(defparameter *merge-test-cases*
  `((,(uri "file:///tmp/junk.txt") . "file:///tmp/junk.txt")
    (,(make-uri :userinfo "auth" :host "secretplace.com") . "http://auth@secretplace.com")
    (,(make-uri :host "example.com" :path "/path" :query "query") . "http://example.com/path?query")
    (,(uri "/new/path") . "http://www.example.com/new/path")
    (,(uri "foo.txt") . "http://www.example.com/path/a/foo.txt")
    (,(uri "../bar") . "http://www.example.com/path/bar")
    (,(uri "other/./car") . "http://www.example.com/path/a/other/car")
    (,(uri "./../.") . "http://www.example.com/path/")
    (,(make-uri :query "name=fukamachi") . "http://www.example.com/path/a/b.html?name=fukamachi")))

(subtest "merge-uris"
  (loop for (test-uri . result-uri) in *merge-test-cases* do
       (let ((merged-uri (merge-uris test-uri *base-uri*)))
         (is (render-uri merged-uri) result-uri :test 'string=))))

(finalize)
