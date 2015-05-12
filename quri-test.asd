#|
  This file is a part of quri project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage quri-test-asd
  (:use :cl :asdf))
(in-package :quri-test-asd)

(defsystem quri-test
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:quri
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "quri")
                 (:test-file "decode")
                 (:test-file "domain")
                 (:test-file "etld")
                 (:test-file "copy")
                 (:file "benchmark"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
