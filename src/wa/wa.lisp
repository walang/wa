; Wa main.

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(load "wc.lisp")
(load "bi.wa.lisp")

; TODO: command line arguments
(defun main ()
  (let ((file (cadr *posix-argv*)))
    (if file
        (wa-load file)
        (wa-repl))))

(save-lisp-and-die "wa" :toplevel #'main :executable t)
