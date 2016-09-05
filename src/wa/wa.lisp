; main.

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(load "wc.lisp")
(load "bi.wa.lisp")


; repl -----------------------------------------------------------------------

(defun wa-repl ()
  (named-readtables:in-readtable :wa)
  (loop (princ "> ")
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
            (format *error-output* "Error: ~A~%" c)))))

; main -----------------------------------------------------------------------

(defun main ()
  (let ((file (cadr *posix-argv*)))
    (if file
        (wa-load file)
        (wa-repl))))

; dump image -----------------------------------------------------------------

(loop repeat 10 do (gc :full t))

(save-lisp-and-die "wa" :toplevel #'main :executable t :save-runtime-options t)
