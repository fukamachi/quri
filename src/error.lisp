(in-package :cl-user)
(defpackage quri.error
  (:use :cl)
  (:export :uri-error

           :uri-malformed-string
           :uri-invalid-port

           :url-decoding-error

           :uri-malformed-urlencoded-string))
(in-package :quri.error)

(define-condition uri-error (simple-error) ())

(define-condition uri-malformed-string (uri-error) ())
(define-condition uri-invalid-port (uri-malformed-string) ())

(define-condition url-decoding-error (uri-error) ())

(define-condition uri-malformed-urlencoded-string (uri-error) ())
