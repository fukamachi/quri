(in-package :cl-user)
(defpackage quri-test.decode
  (:use :cl
        :quri.decode
        :prove))
(in-package :quri-test.decode)

(plan 4)

(is (url-decode-params "a=b&c=d")
    '(("a" . "b") ("c" . "d"))
    "normal case")

(is (url-decode-params "a=b&c=d&e")
    '(("a" . "b") ("c" . "d") ("e"))
    "field only")

(is-error (url-decode-params "a=b=c")
          'quri:uri-malformed-urlencoded-string
          "Raise a malformed error")

(is (url-decode-params "a=b=c" :lenient t)
    '(("a" . "b=c"))
    ":lenient t")

(finalize)
