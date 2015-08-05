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
      "Tiffany+%26+Co.")
  (is (url-encode "{\"field\": \"test\", \"data\": 0, \"products\": {\"name\": \"apples\"}, \"status\": true}")
      "%7B%22field%22%3A%20%22test%22%2C%20%22data%22%3A%200%2C%20%22products%22%3A%20%7B%22name%22%3A%20%22apples%22%7D%2C%20%22status%22%3A%20true%7D"))

(subtest "url-encode-params"
  (is (url-encode-params '(("a" . "b") ("c" . "d")))
      "a=b&c=d"))

(finalize)
