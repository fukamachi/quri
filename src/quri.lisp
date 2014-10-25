(in-package :cl-user)
(defpackage quri
  (:use :cl)
  (:import-from :quri.parser
                :parse-uri)
  (:export :parse-uri))
(in-package :quri)
