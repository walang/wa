; main.

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(load "wc.lisp")
(load "bi.wa.lisp")

; main -----------------------------------------------------------------------

(defun main ()
  ; TODO: load, repl
  (princ *posix-argv*)
  (terpri))

; dump image -----------------------------------------------------------------

(loop repeat 10 do (gc :full t))

(save-lisp-and-die "wa" :toplevel #'main :executable t :save-runtime-options t)
