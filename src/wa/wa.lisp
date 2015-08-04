; Wa main.

(in-package :cl-user)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(load "wc.lisp")
(load "bi.wa.lisp")

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

(defun main ()
  (let ((file (cadr *posix-argv*)))
    (if file
        (wa-load file)
        (wa-repl))))

(loop repeat 10 do (gc :full t))

(save-lisp-and-die "wa" :toplevel #'main :executable t)
