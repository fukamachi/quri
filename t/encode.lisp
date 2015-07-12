(in-package :cl-user)
(defpackage quri-test.encode
  (:use :cl
        :quri.encode
        :prove))
(in-package :quri-test.encode)

(plan 2)

(subtest "url-encode"
  (is (url-encode "Tiffany") "Tiffany")
  (is (url-encode "Tiffany & Co.") "Tiffany%20%26%20Co.")
  (is (url-encode "Tiffany & Co." :space-to-plus t)
      "Tiffany+%26+Co."))

(subtest "url-encode-params"
  (is (url-encode-params '(("a" . "b") ("c" . "d")))
      "a=b&c=d"))

(finalize)
