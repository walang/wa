; Wa compiler.

(load "../../quicklisp/setup.lisp")
(ql:quickload 'cl-ppcre)

(load "cf.lisp")

(declaim (ftype function wc))

(defconstant +eof+ (gensym "eof"))

; namespace ------------------------------------------------------------------

(defpackage wa)

(defun lexp (x env)
  (member x env))

(defun wa-name (x)
  (intern (string x) :wa))

(defun wa-var (x env)
  (if (lexp x env)
      x
      (wa-name x)))

(defun wa-boundp (x)
  (let ((s (wa-name x)))
    (when (boundp s) s)))

(defun wa-sym-val (x)
  (symbol-value (wa-boundp x)))

; helper ---------------------------------------------------------------------

(defun careq (x y)
  (and (consp x) (eq (car x) y)))

; literal --------------------------------------------------------------------

(defun literalp (x)
  (or (null x)
      (eq x t)
      (stringp x)
      (numberp x)
      (characterp x)))

; quasiquote -----------------------------------------------------------------

(defun wc-qq1 (lv x env)
  (cond ((= lv 0)
         (wc x env))
        ((careq x 'unquote)
         (sb-impl::unquote (wc-qq1 (- lv 1) (cadr x) env)))
        ((and (careq x 'unquote-splice) (= lv 1))
         (sb-impl::unquote-splice (wc-qq1 (- lv 1) (cadr x) env)))
        ((careq x 'quasiquote)
         (list 'sb-impl::quasiquote (wc-qq1 (+ lv 1) (cadr x) env)))
        ((consp x)
         (mapcar (lambda (x) (wc-qq1 lv x env)) x))
        (t x)))

(defun wc-qq (args env)
  (list 'sb-impl::quasiquote (wc-qq1 1 args env)))

; if -------------------------------------------------------------------------

(defun wc-if (args env)
  (cond ((null args) nil)
        ((null (cdr args)) (wc (car args) env))
        (t `(if ,(wc (car args) env)
                ,(wc (cadr args) env)
                ,(wc-if (cddr args) env)))))

; tag ------------------------------------------------------------------------

(defun taggedp (x)
  (and (vectorp x) (eq (aref x 0) 'tagged)))

(defun tag-type (x)
  (and (taggedp x) (aref x 1)))

(defun tag (type rep)
  (if (eq (tag-type rep) type)
      rep
      (vector 'tagged type rep)))

(defun rep (x)
  (if (taggedp x)
      (aref x 2)
      x))

; mac ------------------------------------------------------------------------

(defun macp (fn)
  (when (symbolp fn)
    (let ((fn (wa-sym-val fn)))
      (when (eq (tag-type fn) 'mac)
        (rep fn)))))

(defun macex (e &optional once)
  (if (consp e)
      (let ((mac (macp (car e))))
        (if mac
            (let ((expansion (apply mac (cdr e))))
              (if (null once)
                  (macex expansion)
                  expansion))
            e))
      e))

; fn -------------------------------------------------------------------------

; TODO: transform to (lambda (&optional ...

(defun wc-complex-args-p (args)
  (cond ((null args) nil)
        ((symbolp args) nil)
        ((symbolp (car args)) (wc-complex-args-p (cdr args)))
        (t t)))

(defun wc-complex-opt (var expr env ra)
  (list (list var `(if (consp ,ra) (car ,ra) ,(wc expr env)))))

(defun wc-complex-getargs (a)
  (mapcar (lambda (x) (car x)) a))

(defun wc-complex-args (args env ra is-params)
  (cond ((null args) nil)
        ((symbolp args) (list (list args ra)))
        ((consp args)
         (let* ((x (if (and (consp (car args)) (eq (caar args) 'o))
                       (wc-complex-opt (cadar args)
                                       (when (consp (cddar args))
                                           (caddar args))
                                       env
                                       ra)
                       (wc-complex-args (car args)
                                        env
                                        `(car ,ra) nil)))
                (xa (wc-complex-getargs x)))
           (append x (wc-complex-args (cdr args)
                                      (append xa env)
                                      `(cdr ,ra)
                                      is-params))))
        (t (error "can't understand fn arguments list: ~A" args))))

(defun wc-body (body env)
  (mapcar (lambda (x) (wc x env)) body))

(defun wc-complex-fn (args body env)
  (let* ((ra '| COMPLEX-ARGS |) (z (wc-complex-args args env ra t)))
    `(lambda (&rest ,ra)
       (let* ,z
         ,@(wc-body body (append (wc-complex-getargs z) env))))))

(defun wc-arglist (a)
  (cond ((null a) nil)
        ((symbolp a) (list a))
        ((symbolp (cdr a)) (list (car a) (cdr a)))
        (t (cons (car a) (wc-arglist (cdr a))))))

; XXX:
(defun wc-build-args (a)
  (cond ((null a) nil)
        ((symbolp a) (list a))
        ((not (listp (cdr a))) (list (car a) '&rest (cdr a)))
        (t (cons (car a) (wc-build-args (cdr a))))))

(defun wc-fn (args body env)
  (if (wc-complex-args-p args)
      (wc-complex-fn args body env)
      `(lambda ,(if (listp args)
                    (let ((rest (cdr (last args))))
                      (if rest
                          (wc-build-args args)
                          args))
                    (list '&rest args))
        ,@(wc-body body (append (wc-arglist args) env)))))

; call -----------------------------------------------------------------------

(defun wc-apply (fn &rest args)
  (cond ((functionp fn) (apply fn args))
        ((hash-table-p fn) (gethash (car args) fn (cadr args)))
        ((consp fn) (nth (car args) fn))
        ((stringp fn) (char fn (car args)))
        (t (error "function call on inappropriate object: ~A ~A" fn args))))

(defun wc-args (x env)
  (mapcar (lambda (x) (wc x env)) x))

(defun wc-call (fn args env)
  (let ((macfn (macp fn)))
    (cond (macfn
           (wc (apply macfn args) env))
          ((careq fn 'fn)
           `(,(wc fn env) ,@(wc-args args env)))
          ((and (symbolp fn) (not (lexp fn env)) (functionp (wa-sym-val fn)))
           `(funcall ,(wa-var fn env) ,@(wc-args args env)))
          (t `(wc-apply ,(wc fn env) ,@(wc-args args env))))))

; assign ---------------------------------------------------------------------

(defun wc-assign (x env)
  (let ((a (car x))
        (b (wc (cadr x) env)))
    (if (symbolp a)
        (cond ((eq a nil) (error "can't rebind nil"))
              ((eq a t) (error "can't rebind t"))
              ((lexp a env) `(setf ,a ,b))
              (t `(setf ,(wa-name a) ,b)))
        (error "first arg to set must be a symbol: ~A" a))))

; ssyntax --------------------------------------------------------------------

; TODO: refactor

(defun string-value (x)
  (let* ((x (coerce x 'string))
         (s (read-from-string x)))
    (if (symbolp s)
        (intern x)
        s)))

(defun expand-compose (s)
  (let ((elts (mapcar (lambda (tok)
                        (if (careq tok #\~)
                            (if (null (cdr tok))
                                'no
                                `(complement ,(string-value (cdr tok))))
                            (string-value tok)))
                   (mapcar (lambda (x) (coerce x 'list))
                           (ppcre:split ":" s)))))
    (if (null (cdr elts))
        (car elts)
        (cons 'compose elts))))

(defun build-sexpr (toks orig)
  (cond ((null toks) 'get)
        ((or (string= (car toks) ".") (string= (car toks) "!"))
         (error "bad ssyntax: ~A" orig))
        ((null (cdr toks)) (string-value (car toks)))
        (t (list (build-sexpr (cddr toks) orig)
                 (cond ((string= (cadr toks) "!")
                        (list 'quote (intern (car toks))))
                       (t (string-value (car toks))))))))

(defun expand-sexpr (s)
  (build-sexpr (reverse (ppcre:split "(\\.)|(!)"
                                     s
                                     :with-registers-p t
                                     :omit-unmatched-p t))
               s))

(defun expand-and (s)
  (let ((elts (mapcar 'string-value (ppcre:split "&" s))))
    (if (null (cdr elts))
        (car elts)
        (cons 'andf elts))))

(defun expand-ssyntax (sym)
  (let ((s (string sym)))
    (cond ((ppcre:scan "[:~]" s) (expand-compose s))
          ((ppcre:scan "[.!]" s) (expand-sexpr s))
          ((ppcre:scan "[&]" s) (expand-and s))
          (t (error "unknown ssyntax: ~A" sym)))))

(defun ssyntaxp (x)
  (and (symbolp x) (ppcre:scan "[:~&.!]" (string x))))

; read table -----------------------------------------------------------------

(defun read-backquote (s c)
  (declare (ignore c))
  (list 'quasiquote (read s t nil t)))

(defun read-comma (s c)
  (declare (ignore c))
  (if (char= (peek-char nil s) #\@)
      (progn
        (read-char s)
        (list 'unquote-splice (read s t nil t)))
      (list 'unquote (read s t nil t))))

(defun read-bracket (s c)
  (declare (ignore c))
  (list 'fn '(_) (read-delimited-list #\] s t)))

(defmacro with-wa-readtable (&body body)
  `(unwind-protect
     (progn
       (declaim (muffle-conditions warning))
       (set-macro-character #\` 'read-backquote)
       (set-macro-character #\, 'read-comma)
       (set-macro-character #\[ 'read-bracket)
       (set-macro-character #\] (get-macro-character #\)))
       (setf (elt sb-impl::*constituent-trait-table* ,(char-code #\:))
             sb-impl::+char-attr-constituent+)
       (setf (readtable-case *readtable*) :invert)
       ,@body)
     (setf (readtable-case *readtable*) :upcase)
     (setf (elt sb-impl::*constituent-trait-table* ,(char-code #\:))
           sb-impl::+char-attr-package-delimiter+)
     (setf *readtable* (copy-readtable nil))
     (declaim (unmuffle-conditions warning))))

; compiler -------------------------------------------------------------------

(defun wc (s env)
  (cond ((literalp s) s)
        ((ssyntaxp s) (wc (expand-ssyntax s) env))
        ((symbolp s) (wa-var s env))
        ((ssyntaxp (car s)) (wc (cons (expand-ssyntax (car s)) (cdr s)) env))
        ((careq s 'quote) s)
        ((careq s 'quasiquote) (wc-qq (cadr s) env))
        ((careq s 'if) (wc-if (cdr s) env))
        ((careq s 'fn) (wc-fn (cadr s) (cddr s) env))
        ((careq s 'assign) (wc-assign (cdr s) env))
        ((careq s 'cl) (cadr s))
        ((consp s) (wc-call (car s) (cdr s) env))
        (t (error "bad object in expression: ~A" s))))

; eval -----------------------------------------------------------------------

(defun wa-eval (x)
  (eval (wc x nil)))

; repl -----------------------------------------------------------------------

(defun wa-repl ()
  (with-wa-readtable
    (loop
      (princ "> ")
      (force-output)
      (handler-case
        (format t "~S~%" (wa-eval (read)))
        (sb-sys:interactive-interrupt (c)
          (declare (ignore c))
          (terpri))
        (end-of-file (c)
          (declare (ignore c))
          (terpri)
          (exit))
        (error (c)
          (format *error-output* "Error: ~A~%" c))))))

; load -----------------------------------------------------------------------

(defun wa-load (filename)
  (with-open-file (ip filename)
    (with-wa-readtable
      (loop for x = (read ip nil +eof+)
            until (eq x +eof+)
            do (wa-eval x)))))

; compile --------------------------------------------------------------------

(defun wa-compile (filename &optional (debug nil))
  (let ((*standard-output* (make-broadcast-stream)))
    (with-open-file (ip filename)
      (with-open-file (op (concatenate 'string filename ".lisp")
                          :direction :output
                          :if-exists :supersede)
        (with-wa-readtable
          (format op "; generated by the Wa v~A compiler.~2%" +version+)
          (format op "(declaim (muffle-conditions warning))~2%")
          (loop for x = (read ip nil +eof+)
                until (eq x +eof+)
                do (let ((cl (wc x nil)))
                     (eval cl)
                     (if debug (format op "#|~%~S~%|#~%" x))
                     (format op "~S~2%" cl)))
          (format op "(declaim (unmuffle-conditions warning))~%"))))))
