;; --------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-faster-input.system)
    (defpackage :cl-faster-input.system
      (:use :common-lisp :asdf))))

;; --------------------------------------------------------

(in-package :cl-faster-input.system)

;; --------------------------------------------------------

(defsystem :cl-faster-input
  :description "Benchmarking different i/o options available on Common Lisp"
  :license "BSD"
  :components ((:module :src
			:serial T
			:components ((:file "package")
				     (:file "benchmark-count-lines")
				     (:file "benchmark-mmap")
				     (:file "benchmark-read-line"))))
  :depends-on (:fast-io
	       :com.informatimago.common-lisp.cesarum
	       :ascii-strings
	       :iterate			; for various places
	       :osicat
	       :cffi
	       :alexandria
	       ))

;; EOF
