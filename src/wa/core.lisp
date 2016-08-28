; Wa core.

(in-package :cl-user)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(load "wc.lisp")
(load "bi.wa.lisp")

(loop repeat 10 do (gc :full t))

(save-lisp-and-die "wa.core" :executable t)
