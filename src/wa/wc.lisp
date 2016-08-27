; Wa compiler.

(in-package :cl-user)

(load #P"../../quicklisp/setup.lisp")

(ql:quickload 'named-readtables)

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

; read table -----------------------------------------------------------------

(defun read-backquote (s c)
  (declare (ignore c))
  `(quasiquote ,(read s t nil t)))

(defun read-comma (s c)
  (declare (ignore c))
  (if (char= (peek-char nil s) #\@)
      (progn
        (read-char s)
        `(unquote-splice ,(read s t nil t)))
      `(unquote ,(read s t nil t))))

(named-readtables:defreadtable :wa
  (:merge :standard)
  (:macro-char #\` #'read-backquote t)
  (:macro-char #\, #'read-comma t)
  (:case :invert))

; literal --------------------------------------------------------------------

(defun literalp (x)
  (or (numberp x)
      (stringp x)
      (characterp x)))

; quasiquote -----------------------------------------------------------------

(defun wc-qq1 (lv x env)
  (cond ((= lv 0)
         (wc x env))
        ((careq x 'unquote)
         (sb-impl::unquote (wc-qq1 (- lv 1) (cadr x) env)))
        ((and (= lv 1) (careq x 'unquote-splice))
         (sb-impl::unquote-splice (wc-qq1 (- lv 1) (cadr x) env)))
        ((careq x 'quasiquote)
         (list 'sb-impl::quasiquote (wc-qq1 (+ lv 1) (cadr x) env)))
        ((consp x)
         (mapcar (lambda (x) (wc-qq1 lv x env)) x))
        (t x)))

(defun wc-qq (x env)
  (list 'sb-impl::quasiquote (wc-qq1 1 x env)))

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
        ((careq s 'quasiquote) (wc-qq (cadr s) env))
        ((careq s 'if) (wc-if (cdr s) env))
        ((careq s 'assign) (wc-assign (cdr s) env))
        (t (error "bad object in expression: ~A" s))))
