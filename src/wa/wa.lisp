; main.

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(load "wc.lisp")
(load "bi.wa.lisp")

(ql:quickload :getopt)
(ql:quickload :string-case)

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

(defparameter *opts* '(("help" :NONE) ("version" :NONE)))

(defun invalid-opt (opt)
  (format *error-output* "wa: invalid option: -~A~%" opt)
  (exit :code 1))

(defun usage ()
  (format t "Usage: wa [options] [--] [programfile] [arguments]~2%")
  (format t "Options:~%")
  (format t "  -v, --version   print version number~%")
  (format t "  -h, --help      show this message~%")
  (exit))

(defun version ()
  (format t "v~A~%" +wa-version+)
  (exit))

(defun main ()
  (multiple-value-bind (args opts errs)
    (getopt:getopt (cdr *posix-argv*) *opts*)
    (unless (null errs) (invalid-opt (car errs)))
    (dolist (opt opts)
      (string-case:string-case ((car opt))
        ("help" (usage))
        ("version" (version))))
    (let ((file (car args)))
      (if file
        (wa-load file)
        (wa-repl)))))

; dump image -----------------------------------------------------------------

(loop repeat 10 do (gc :full t))

(save-lisp-and-die "wa" :toplevel #'main :executable t :save-runtime-options t)
