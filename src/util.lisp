(in-package :cl-user)
(defpackage quri.util
  (:use :cl)
  (:import-from :alexandria
                :with-gensyms)
  (:export :with-array-parsing
           :redo
           :gonext
           :goto))
(in-package :quri.util)

(define-condition parsing-end-unexpectedly (simple-error)
  ((state :initarg :state
          :initform nil))
  (:report (lambda (condition stream)
             (format stream "Parsing ended unexpectedly~:[~;~:* at ~A~]"
                     (slot-value condition 'state)))))

(define-condition no-next-state (simple-error) ())

(defmacro with-array-parsing ((elem p seq &optional (start 0) end) &body body)
  (with-gensyms (g-end no-next-state last)
    (let ((eof-exists nil))
      `(let ((,p ,start)
             (,g-end (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
                       (or ,end (length ,seq))))
             ,elem)
         (declare (ignorable ,p ,g-end ,elem))
         ,@(loop for (exp . rest) on body
                 while (and (listp exp) (eq (car exp) 'declare))
                 collect exp
                 do (setq body rest))
         (macrolet ((goto (tag &optional (amount 1))
                      `(locally (declare (optimize (speed 3) (safety 0)))
                         (incf ,',p ,amount)
                         ,@(if (eql amount 0)
                               ()
                               `((when (= ,',p ,',g-end)
                                   (go :eof))
                                 (setq ,',elem (aref ,',seq ,',p))))
                         (go ,tag))))
           (tagbody
              (when (= ,p ,g-end)
                (go :eof))
              (setq ,elem (aref ,seq ,p))
              ,@(loop for (tagpart . rest) on body
                      for (tag . part) = tagpart
                      if (eq tag :eof)
                        append (progn
                                 (setf eof-exists t)
                                 `(,@tagpart
                                   (go ,last)))
                      else
                        append
                        (list tag
                              `(macrolet ((redo (&optional (amount 1))
                                            `(goto ,',tag ,amount))
                                          (gonext (&optional (amount 1))
                                            `(goto ,',(or (caar rest) no-next-state)
                                                   ,amount)))
                                 ,@part
                                 (error 'parsing-end-unexpectedly :state ',tag))))

              ,no-next-state
              (error 'no-next-state)

              ,@(if eof-exists
                    ()
                    '(:eof))

              ,last))))))
