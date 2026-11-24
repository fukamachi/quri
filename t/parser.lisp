(in-package :cl-user)
(defpackage quri-test.parser
  (:use :cl
        :quri.parser
        :prove))
(in-package :quri-test.parser)

(plan nil)

(subtest "parser string bounds"
  (is (nth-value 0 (parse-uri "foo://bar")) "foo")
  (is (nth-value 0 (parse-uri "foo://bar" :start 4)) nil)
  (is (nth-value 4 (parse-uri "foo://bar/xyz?a=b#c")) "/xyz")
  (is (nth-value 4 (parse-uri "foo://bar/xyz?a=b#c" :end 12)) "/xy")
  (is (nth-value 5 (parse-uri "foo://bar/xyz?a=b#c")) "a=b")
  (is (nth-value 5 (parse-uri "foo://bar/xyz?a=b#c" :end 13)) nil)
  (is (nth-value 6 (parse-uri "foo://bar/xyz?a=b#c")) "c")
  (is (nth-value 6 (parse-uri "foo://bar/xyz?a=b#c" :end 17)) nil))

(subtest "userinfo before an IP literal"
  (is (nth-value 2 (parse-uri "redis://:pw@[::1]:6379/2")) "[::1]")
  (is (nth-value 3 (parse-uri "redis://:pw@[::1]:6379/2")) 6379)
  (is (nth-value 3 (parse-uri "http://user@[2001:db8::7]/x")) 80)
  (is-error (parse-uri "redis://:pw@host[::1]/") 'quri:uri-malformed-string)
  (is-error (parse-uri "redis://a@b@[::1]:6379/") 'quri:uri-malformed-string)
  (is-error (parse-uri "redis://:pw@[::1]@host/") 'quri:uri-malformed-string))

(finalize)
