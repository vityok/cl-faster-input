
;;---------------------------------------------------------

(defpackage :cl-faster-input
  (:use :common-lisp :iterate :com.informatimago.common-lisp.cesarum.ascii)
  (:nicknames :fio)
  (:export
   :benchmark-count-lines
   :benchmark-read-line
   :benchmark-mmap
   ))

;;---------------------------------------------------------

(in-package :cl-faster-input)

;; got idea from CL-PPCRE. Need to place it here so that the Lisp
;; reader will have an idea about this variable when parsing other
;; package source files
(defvar *standard-optimize-settings*
  '(optimize
    (speed 3)
    (safety 0)
    (space 0)
    (debug 1)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 0))
  "The standard optimize settings used by most declaration expressions.")

;;---------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize
    speed
    (safety 0)
    (space 0)
    (debug 0)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 0))))

;; EOF
