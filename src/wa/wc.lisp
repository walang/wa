; compiler.

(load "cf.lisp")
(load "../../quicklisp/setup.lisp")

(ql:quickload 'named-readtables)

(declaim (ftype function wc))

; helper ---------------------------------------------------------------------

(declaim (inline careq))
(defun careq (x y)
  (and (consp x) (eq (car x) y)))

; namespace ------------------------------------------------------------------

(defpackage :wa (:import-from :cl :t :nil))

(defun lexp (x env)
  (member x env))

(defun wa-sym (x)
  (intern (string x) :wa))

(defun wa-var (x env)
  (if (lexp x env) x (wa-sym x)))

(defun wa-boundp (x)
  (let ((s (wa-sym x)))
    (when (boundp s) s)))

(defun wa-sym-val (x)
  (symbol-value (wa-boundp x)))

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

(defun read-bracket (s c)
  (declare (ignore c))
  `(fn (_) ,(read-delimited-list #\] s t)))

(defun read-cl (s c1 c2)
  (declare (ignore c1 c2))
  (let ((*readtable* (named-readtables:find-readtable :cl)))
    `(cl ,(read s t nil t))))

(defun read-shebang (s c1 c2)
  (declare (ignore c1 c2))
  (read-line s))

(named-readtables:defreadtable :wa
  (:merge :standard)
  (:macro-char #\` #'read-backquote t)
  (:macro-char #\, #'read-comma t)
  (:macro-char #\[ #'read-bracket t)
  (:syntax-from :standard #\) #\])
  (:dispatch-macro-char #\# #\d #'read-cl)
  (:dispatch-macro-char #\# #\! #'read-shebang)
  (:case :invert))

(named-readtables:defreadtable :cl
  (:merge :standard)
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

; assign ---------------------------------------------------------------------

(defconstant +reserved-words+ '(t nil))

(defun wc-assign (x env)
  (let ((a (car x))
        (b (wc (cadr x) env)))
    (cond ((not (symbolp a)) (error "first arg to set must be a symbol: ~S" a))
          ((member a +reserved-words+) (error "can't rebind: ~(~A~)" a))
          ((lexp a env) `(setf ,a ,b))
          (t `(defparameter ,(wa-sym a) ,b)))))

; if -------------------------------------------------------------------------

(defun wc-if (args env)
  (cond ((null args) nil)
        ((null (cdr args)) (wc (car args) env))
        (t `(if ,(wc (car args) env)
                ,(wc (cadr args) env)
                ,(wc-if (cddr args) env)))))

; fn -------------------------------------------------------------------------

; TODO: optional, keyword args

(declaim (inline wc-body))
(defun wc-body (body env)
  (mapcar (lambda (x) (wc x env)) body))

(declaim (inline wc-args))
(defun wc-args (args)
  (cond ((null args) nil)
        ((symbolp args) `(,args))
        ((symbolp (cdr args)) `(,(car args) ,(cdr args)))
        (t (cons (car args) (wc-args (cdr args))))))

(declaim (inline wc-build-args))
(defun wc-build-args (args)
  (cond ((null args) nil)
        ((symbolp args) `(,args))
        ((not (listp (cdr args))) `(,(car args) &rest ,(cdr args)))
        (t (cons (car args) (wc-build-args (cdr args))))))

(defun wc-fn (args body env)
  `(lambda ,(if (listp args)
                (if (cdr (last args)) (wc-build-args args) args)
                `(&rest ,args))
    ,@(wc-body body (append (wc-args args) env))))

; tag ------------------------------------------------------------------------

(defun taggedp (x)
  (and (vectorp x) (eq (aref x 0) 'tagged)))

(defun tag-name (x)
  (and (taggedp x) (aref x 1)))

(defun tag (type rep)
  (if (eq (tag-name rep) type) rep (vector 'tagged type rep)))

(defun rep (x)
  (if (taggedp x) (aref x 2) x))

; mac ------------------------------------------------------------------------

(defun macp (fn)
  (when (symbolp fn)
    (let ((fn (wa-sym-val fn)))
      (when (eq (tag-name fn) 'mac)
        (rep fn)))))

(defun macex (e &optional once)
  (if (consp e)
      (let ((mac (macp (car e))))
        (if mac
            (let ((ex (apply mac (cdr e))))
              (if once ex (macex ex)))
            e))
      e))

; call -----------------------------------------------------------------------

(declaim (inline wa-apply))
(defun wa-apply (fn &rest args)
  (cond ((functionp fn) (apply fn args))
        ((hash-table-p fn) (gethash (car args) fn (cadr args)))
        ((consp fn) (nth (car args) fn))
        ((stringp fn) (char fn (car args)))
        (t (error "function call on inappropriate object: ~A ~A" fn args))))

(defun wc-call (fn args env)
  (let ((mac (macp fn)))
    (cond (mac
           (wc (apply mac args) env))
          ((careq fn 'fn)
           `(,(wc fn env) ,@(wc-body args env)))
          ((and (symbolp fn) (not (lexp fn env)) (functionp (wa-sym-val fn)))
           `(funcall ,(wa-var fn env) ,@(wc-body args env)))
          (t `(wa-apply ,(wc fn env) ,@(wc-body args env))))))

; compiler -------------------------------------------------------------------

(defun wc (s env)
  (cond ((literalp s) s)
        ((symbolp s) (wa-var s env))
        ((careq s 'quote) s)
        ((careq s 'quasiquote) (wc-qq (cadr s) env))
        ((careq s 'assign) (wc-assign (cdr s) env))
        ((careq s 'if) (wc-if (cdr s) env))
        ((careq s 'fn) (wc-fn (cadr s) (cddr s) env))
        ((careq s 'cl) (cadr s))
        ((consp s) (wc-call (car s) (cdr s) env))
        (t (error "bad object in expression: ~A" s))))

; eval -----------------------------------------------------------------------

(defun wa-eval (x)
  (eval (wc x nil)))

; load -----------------------------------------------------------------------

(defun wa-load (file)
  (with-open-file (i file)
    (let ((eof (gensym)))
      (named-readtables:in-readtable :wa)
      (loop for x = (read i nil eof) until (eq x eof) do
        (wa-eval x)))))

; compile --------------------------------------------------------------------

(defun wa-compile (src &optional (verbose nil))
  (let ((dst (concatenate 'string src ".lisp"))
        (eof (gensym)))
    (with-open-file (i src)
      (with-open-file (o dst :direction :output :if-exists :supersede)
        (format o "; generated by the Wa v~A compiler.~2%" +wa-version+)
        (named-readtables:in-readtable :wa)
        (loop for x = (read i nil eof) until (eq x eof) do
          (when verbose (format o "#|~%~S~%|#~%" x))
          (format o "~S~2%" (wc x nil)))))
    (compile-file dst)))
