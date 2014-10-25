(in-package :cl-user)
(defpackage quri-test
  (:use :cl
        :quri
        :prove))
(in-package :quri-test)

(plan nil)

(defparameter *test-cases*
  '(("file:///tmp/junk.txt" .
     (:file nil nil "/tmp/junk.txt" nil nil))
    ("imap://mail.common-lisp.net/mbox1" .
     (:imap nil "mail.common-lisp.net" "/mbox1" nil nil))
    ("mms://wms.sys.hinet.net/cts/Drama/09006251100.asf" .
     (:mms nil "wms.sys.hinet.net" "/cts/Drama/09006251100.asf" nil nil))
    ("nfs://server/path/to/file.txt" .
     (:nfs nil "server" "/path/to/file.txt" nil nil))
    ("svn+ssh://svn.zope.org/repos/main/ZConfig/trunk/" .
     (:svn+ssh nil "svn.zope.org" "/repos/main/ZConfig/trunk/" nil nil))
    ("git+ssh://git@github.com/user/project.git" .
     (:git+ssh "git" "github.com" "/user/project.git" nil nil))
    ("http://common-lisp.net" .
     (:http nil "common-lisp.net" nil nil nil))
    ("http://common-lisp.net#abc" .
     (:http nil "common-lisp.net" nil nil "abc"))
    ("http://common-lisp.net?q=abc" .
     (:http nil "common-lisp.net" nil "q=abc" nil))
    ("http://common-lisp.net/#abc" .
     (:http nil "common-lisp.net" "/" nil "abc"))
    ("http://a/b/c/d;p?q#f" .
     (:http nil "a" "/b/c/d;p" "q" "f"))))

(loop for (test-uri . params) in *test-cases* do
  (let ((uri (uri test-uri)))
    (subtest test-uri
      (is (uri-scheme uri)   (nth 0 params) "scheme")
      (is (uri-userinfo uri) (nth 1 params) "userinfo")
      (is (uri-host uri)     (nth 2 params) "host")
      (is (uri-path uri)     (nth 3 params) "path")
      (is (uri-query uri)    (nth 4 params) "query")
      (is (uri-fragment uri) (nth 5 params) "fragment"))))

(finalize)
