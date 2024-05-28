(in-package :cl-user)
(defpackage quri-test.decode
  (:use :cl
        :quri.decode
        :prove))
(in-package :quri-test.decode)

(plan 8)

(is (url-decode-params "a=b&c=d")
    '(("a" . "b") ("c" . "d"))
    "normal case")

(is (url-decode-params "a=b&c=d&e")
    '(("a" . "b") ("c" . "d") ("e"))
    "field only")

(is (url-decode-params "alpha=%D0%B0%D0%B1%D0%B2")
    '(("alpha" . "абв"))
    "percent decode")

(is (url-decode-params "alpha=%D0%B0%D0%B1%D0%B2" :percent-decode nil)
    '(("alpha" . "%D0%B0%D0%B1%D0%B2"))
    "no percent decode")

(is-error (url-decode-params "a=b=c")
          'quri:uri-malformed-urlencoded-string
          "Raise a malformed error")

(is (url-decode-params "a=b=c" :lenient t)
    '(("a" . "b=c"))
    ":lenient t")

(is-error (url-decode-params "a=%!@#&b=1")
          'quri:url-decoding-error
          "Raise a decoding error")

(is (url-decode-params "a=%!@#&b=1" :lenient t)
    '(("a" . "%!@#") ("b" . "1")))

(defvar *replacement-character*
  #+abcl
  (babel:octets-to-string (coerce #(239 191 189) '(array (unsigned-byte 8) (3))))
  #-abcl
  (princ-to-string #\replacement_character))

(is (url-decode "%bf" :lenient t)
    *replacement-character*)

(is (url-decode-params "%bf" :lenient t)
    `((,*replacement-character*)))

(finalize)
