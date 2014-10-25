(in-package :cl-user)
(defpackage quri.error
  (:use :cl)
  (:export :uri-error

           :uri-parse-error

           :uri-malformed-string
           :uri-invalid-scheme
           :uri-invalid-port

           :uri-callback-error))
(in-package :quri.error)

(define-condition uri-error (simple-error) ())

(define-condition uri-parse-error (uri-error) ())

(define-condition uri-malformed-string (uri-parse-error) ())
(define-condition uri-invalid-scheme (uri-malformed-string) ())
(define-condition uri-invalid-port (uri-malformed-string) ())
(define-condition uri-malformed-urlencoded-string (uri-malformed-string) ())

(define-condition uri-callback-error (uri-error)
  ((name :initarg :name
         :initform (error ":name is required"))
   (error :initarg :error
          :initform (error ":error is required")))
  (:report (lambda (condition stream)
             (format stream "Error while executing a callback: ~S~%    ~A"
                     (slot-value condition 'name)
                     (slot-value condition 'error)))))
