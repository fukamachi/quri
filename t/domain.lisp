(in-package :cl-user)
(defpackage quri-test.domain
  (:use :cl
        :quri.domain
        :prove))
(in-package :quri-test.domain)

(plan nil)

(subtest "ipv4-addr-p"
  (ok (ipv4-addr-p "127.0.0.1")
      "127.0.0.1 is valid")
  (ok (ipv4-addr-p "255.255.255.255")
      "255.255.255.255 is valid")
  (ok (not (ipv4-addr-p "256.255.255.255"))
      "256.255.255.255 is not valid")
  (ok (not (ipv4-addr-p "345.23.1.0"))
      "345.23.1.0 is not valid")
  (ok (not (ipv4-addr-p "127.0"))
      "127.0 is not valid")
  (ok (not (ipv4-addr-p "127.0.0.0.1"))
      "127.0.0.0.1 is not valid")
  (ok (not (ipv4-addr-p "2ch.net"))
      "2ch.net is not valid")
  (ok (not (ipv4-addr-p "127..0.1"))
      "127..0.1 is not valid")
  (ok (not (ipv4-addr-p "..."))
      "... is not valid"))

(subtest "ipv6-addr-p"
  (ok (ipv6-addr-p "2001:0db8:bd05:01d2:288a:1fc0:0001:10ee"))
  (ok (ipv6-addr-p "2001:db8:20:3:1000:100:20:3"))
  (ok (ipv6-addr-p "2001:db8::1234:0:0:9abc"))
  (ok (ipv6-addr-p "2001:db8::9abc"))
  (ok (ipv6-addr-p "::1")))

(finalize)
