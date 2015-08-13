;;; benchmarking memory mapped files

;; This benchmark relies on OSICAT POSIX system functions wrapper
;; library to call the mmap system function. Currently the OSICAT must
;; be patched according to the advice at:
;;
;; http://blog.gmane.org/gmane.lisp.osicat.devel/month=20091101
;;
;; Instead of long, unsigned-long must be used in the errno wrapper.

(in-package :cl-faster-input)

;; -----------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim #.*standard-optimize-settings*))

;; -----------------------------------------------------------

#+ccl
(defun run-test-mmap-ccl ()

  (let ((data (CCL:MAP-FILE-TO-OCTET-VECTOR +fname+))
	(count 0))

    (loop :for b :across data
       :when (= b #.(char-code #\Newline))
       :do (incf count))

    (CCL:UNMAP-OCTET-VECTOR data)))

;; -----------------------------------------------------------

;; Ideas for this function and OSICAT related  is borrowed from
;; http://wandrian.net/2012-04-07-1352-mmap-files-in-lisp.html

(defun mmap-file (path)
  (let ((fd (osicat-posix:open path (logior osicat-posix:o-rdonly))))
    (unwind-protect
         (let* ((size (osicat-posix:stat-size (osicat-posix:fstat fd))))
	   (let ((addr (osicat-posix:mmap (cffi:null-pointer) size
					  (logior osicat-posix:prot-read)
					  (logior osicat-posix:map-private)
					  fd 0)))
	     (values addr size)))
      (osicat-posix:close fd))))

(defun munmap-file (addr size)
  (osicat-posix:munmap addr size))

(defmacro with-mmapped-file ((file addr size) &body body)
  (let ((original-addr (gensym "ADDR-"))
        (original-size (gensym "SIZE-")))
    `(multiple-value-bind (,addr ,size)
         (mmap-file ,file)
       (let ((,original-addr ,addr)
             (,original-size ,size))
         (unwind-protect
              (progn ,@body)
           (munmap-file ,original-addr ,original-size))))))

;; -----------------------------------------------------------

(defun run-test-osicat ()
  (declare (optimize
	    speed
	    (safety 0)
	    (space 0)
	    (debug 0)
	    (compilation-speed 0)
	    #+:lispworks (hcl:fixnum-safety 0)))

  (let ((count 0))
    (declare (fixnum count))
    (with-mmapped-file (+fname+ orig-addr size)
      (dotimes (i 10)
	(let ((addr orig-addr))
	  (loop :for l fixnum :from 0 :to size
	     :for b = (cffi:mem-aref addr :ushort)
	     :when (= b #.(char-code #\Newline))
	     :do (incf count)
	     :do (cffi:incf-pointer addr)))))))

;; -----------------------------------------------------------

(defun benchmark-mmap ()
  (unless (probe-file +fname+)
    (prepare))

  #+ccl
  (progn
    (format t  "RUN-TEST-MMAP-CCL")
    (time (dotimes (i 10)  (RUN-TEST-MMAP-CCL))))

  (format t  "RUN-TEST-MMAP-OSICAT")
  (time (RUN-TEST-OSICAT))
  )

;; EOF
