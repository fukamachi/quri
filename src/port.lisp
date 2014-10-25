(in-package :cl-user)
(defpackage quri.port
  (:use :cl)
  (:import-from :alexandria
                :plist-hash-table)
  (:export :scheme-default-port))
(in-package :quri.port)

(defvar +default-ports+
  (plist-hash-table
   '(:ftp 21
     :ssh 22
     :telnet 23
     :http 80
     :ldap 389
     :https 443)
   :test 'eq))

(defun scheme-default-port (scheme)
  (gethash scheme +default-ports+))
