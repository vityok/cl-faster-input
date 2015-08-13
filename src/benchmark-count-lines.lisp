;;; benchmarking READ-LINE and alternatives

;; Tests are doing essentially what unix command "wc -l" does: reading
;; the file and counting number of lines in it. The implementation
;; differ however in the amount of memory used and the time it takes
;; to complete the task.
;;
;; These tests just count lines, however, they don't perform per-line
;; processing: it is performed in the benchmark-read-line.lisp

(in-package :cl-faster-input)

;; -----------------------------------------------------------

(defconstant +kb+ 1024)
(defconstant +mb+ #.(* 1024 1024))
(defconstant +file-size+ (* 10 +mb+)
  "This will produce a file 10MB in size.")
(defconstant +fname+ "data.tmp")

;; -----------------------------------------------------------

(defun prepare ()
  "Create a temporary file with a predefined size +FILE-SIZE+ times
+KB+.

 The file is not that very large but will be processed multiple
times. The rationale is that we hope that the file is small enough to
be cached, but is also large enough not to overshadow the tests with
file open and close operations."

  (let* ((line-length 64)
	 (line (make-string 64
			   :initial-element #\A)))

    (with-open-file (of +fname+
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
		    (declare (stream of))
      (dotimes (i (round +file-size+ line-length))
	(format of "~a~%" line)))))

;; -----------------------------------------------------------

(defun run-test-read-line ()
  "Count lines using standard READ-LINE."

  (with-open-file (if +fname+
		      :direction :input)

    (loop :for line = (read-line if nil)
       :while line :count line)))

;; -----------------------------------------------------------

(defun run-test-read-sequence-string ()
  "Count lines by reading data in chunks into a string."

  (with-open-file (is +fname+
		      :direction :input)

    (let ((line (make-string (* 4 +kb+)
			     :initial-element #\?))
	  (count 0))
      (declare (fixnum count))

      (loop :while (> (read-sequence line is) 0)
	 :do (loop :for c :across line
		:when (char= c #\Newline)
		:do (incf count)))
      count)))

;; -----------------------------------------------------------

(defun run-test-read-sequence-byte-array ()
  "Reading into a bytes array.

Taken a que from Stackoverflow:
http://stackoverflow.com/a/15813006"

  (with-open-file (input-stream +fname+
				:direction :input
				:element-type '(unsigned-byte 8))
    (let ((buf (make-array (* 16 +kb+) :element-type '(unsigned-byte 8)))
	  (count 0))

      (declare (fixnum count)
	       (type (array (unsigned-byte 8)) buf))

      (loop :for pos = (read-sequence buf input-stream)
	 :while (plusp pos) :do
	 (loop :for c :across buf
	    :when (= c #.(char-code #\Newline))
	    :do (incf count)))
      count)))

;; -----------------------------------------------------------

#+sbcl
(defun run-test-read-posix ()
  "Reading into a bytes array.

Taken a que from Stackoverflow:
http://stackoverflow.com/a/15813006"

  (let* ((fd (sb-posix:open +fname+ sb-posix:o-rdonly))
	 (buf-len (* 16 +kb+))
	 (buf (make-array buf-len :element-type '(unsigned-byte 8)))
	 (count 0))

    (declare (fixnum fd)
	     (fixnum count)
	     (type (array (unsigned-byte 8)) buf))

    (loop :for pos of-type fixnum = (sb-posix:read fd (sb-sys:vector-sap buf) buf-len)
	  :while (> pos 0) :do
	  (loop :for c :across buf
		:when (= c #.(char-code #\Newline))
		:do (incf count)))
    (sb-posix:close fd)
    count))

;; -----------------------------------------------------------

(defun run-test-read-sequence-by-char ()
  "Read the file char by char."

  (with-open-file (input-stream +fname+
				:direction :input)
    (let ((count 0))

      (declare (fixnum count))

      (loop :for c = (read-char input-stream nil)
	 :while c
	 :when (char= c #\Newline)
	 :do (incf count))
      count)))

;; -----------------------------------------------------------

(defun run-test-read-sequence-by-byte ()
  "Read the file byte by byte."

  (with-open-file (input-stream +fname+
				:element-type 'unsigned-byte
				:direction :input)
    (let ((count 0))

      (declare (fixnum count))

      (loop :for b :of-type unsigned-byte = (read-byte input-stream nil 0)
	 :while (/= b 0)
	 :when (= b #.(char-code #\Newline))
	 :do (incf count))

      count)))

;; -----------------------------------------------------------

(defun benchmark-count-lines ()

  (unless (probe-file +fname+)
    (prepare))

  (format t "WARMING UP~%")
  (dotimes (i 10) (run-test-read-line))

  (format t "READ-LINE~%")
  (time (dotimes (i 10) (run-test-read-line)))

  (format t "READ-SEQUENCE~%")
  (time (dotimes (i 10) (run-test-read-sequence-string)))

  (format t "RUN-TEST-READ-SEQUENCE-BYTE-ARRAY~%")
  (time (dotimes (i 10) (run-test-read-sequence-byte-array)))

  #+sbcl
  (progn
    (format t "RUN-TEST-READ-POSIX~%")
    (time (dotimes (i 10) (RUN-TEST-READ-POSIX))))

  (format t "RUN-TEST-READ-SEQUENCE-BY-CHAR~%")
  (time (dotimes (i 10) (RUN-TEST-READ-SEQUENCE-BY-CHAR)))

  (format t "RUN-TEST-READ-SEQUENCE-BY-BYTE~%")
  (time (dotimes (i 10) (RUN-TEST-READ-SEQUENCE-BY-BYTE)))
  )

;; EOF
