; Wa main.

(in-package :cl-user)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(load "wc.lisp")
(load "bi.lisp")

(defun main ()
  (let ((file (cadr *posix-argv*)))
    (if file
        (with-wa-readtable (wa-load file))
        (with-wa-readtable (wa-repl)))))

(loop repeat 10 do (gc :full t))

(save-lisp-and-die "wa" :toplevel #'main :executable t)
