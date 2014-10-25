#|
  This file is a part of quri project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage quri-asd
  (:use :cl :asdf))
(in-package :quri-asd)

(defsystem quri
  :version "0.0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:alexandria)
  :components ((:module "src"
                :components
                ((:file "quri" :depends-on ("parser"))
                 (:file "parser" :depends-on ("error" "util"))
                 (:file "util")
                 (:file "error"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op quri-test))))
