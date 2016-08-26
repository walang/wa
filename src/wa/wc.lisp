; Wa compiler.

(in-package :cl-user)

(declaim (ftype function wc))

; helper ---------------------------------------------------------------------

(declaim (inline careq))
(defun careq (x y)
  (and (consp x) (eq (car x) y)))

; namespace ------------------------------------------------------------------

(defpackage wa)

(defun lexp (x env)
  (member x env))

(defun wa-sym (x)
  (intern (string x) :wa))

(defun wa-var (x env)
  (if (lexp x env) x (wa-sym x)))

; literal --------------------------------------------------------------------

(defun literalp (x)
  (or (numberp x)
      (stringp x)
      (characterp x)))

; if -------------------------------------------------------------------------

(defun wc-if (args env)
  (cond ((null args) nil)
        ((null (cdr args)) (wc (car args) env))
        (t `(if ,(wc (car args) env)
                ,(wc (cadr args) env)
                ,(wc-if (cddr args) env)))))

; assign ---------------------------------------------------------------------

(defconstant +reserved-words+ '(t nil))

(defun wc-assign (x env)
  (let ((a (car x))
        (b (wc (cadr x) env)))
    (cond ((not (symbolp a)) (error "first arg to set must be a symbol: ~S" a))
          ((member a +reserved-words+) (error "can't rebind: ~(~A~)" a))
          ((lexp a env) `(setf ,a ,b))
          (t `(defparameter ,(wa-sym a) ,b)))))

; compiler -------------------------------------------------------------------

(defun wc (s env)
  (cond ((literalp s) s)
        ((symbolp s) (wa-var s env))
        ((careq s 'quote) s)
        ((careq s 'if) (wc-if (cdr s) env))
        ((careq s 'assign) (wc-assign (cdr s) env))
        (t (error "bad object in expression: ~A" s))))
