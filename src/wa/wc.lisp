; Wa compiler.

(in-package :cl-user)

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

; compiler -------------------------------------------------------------------

(defun wc (s env)
  (cond ((literalp s) s)
        ((symbolp s) (wa-var s env))
        ((careq s 'quote) s)
        (t (error "bad object in expression: ~A" s))))
