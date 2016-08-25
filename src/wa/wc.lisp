; Wa compiler.

(in-package :cl-user)

; literal --------------------------------------------------------------------

(defun literalp (x)
  (or (numberp x)
      (stringp x)
      (characterp x)))

; compiler -------------------------------------------------------------------

(defun wc (s env)
  (cond ((literalp s) s)
        (t (error "bad object in expression: ~A" s))))
