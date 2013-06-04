;;; benchmarking READ-LINE and alternatives

;; Tests are doing essentially what unix command "wc -l" does: reading
;; the file and counting number of lines in it. The implementation
;; differ however in the amount of memory used and the time it takes
;; to complete the task.
;;
;; How to run with Clozure CL:
;;
;; lx86cl --load benchmark-read-line.lisp --eval '(run)' --eval '(quit)'

;; With SBCL:
;; sbcl --load benchmark-read-line.lisp --eval '(run)' --eval '(quit)'

;; With ECL it is much better first to compile the file and only then run it:
;; ecl -compile benchmark-read-line.lisp
;; ecl -load benchmark-read-line.fas -eval "(run)" -eval "(quit)"


(defconstant +kb+ 1024)
(defconstant +mb+ #.(* 1024 1024))
(defconstant +file-size+ (* 10 +kb+)
  "This will produce a file 10MB in size.")
(defconstant +fname+ "data.tmp")

;; -----------------------------------------------------------

(defconstant +standard-optimize-settings+
  '(optimize
    speed
    (safety 0)
    (space 0)
    (debug 0)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 0))
  "The standard optimize settings used by most declaration expressions.")

;; -----------------------------------------------------------

(defun prepare ()
  "Create a temporary file with a predefined size +FILE-SIZE+ times
+KB+.

 The file is not that very large but will be processed multiple
times. The rationale is that we hope that the file is small enough to
be cached, but is also large enough not to overshadow the tests with
file open and close operations."

  (let ((line (make-string (- +kb+ 1)
			   :initial-element #\A)))

    (with-open-file (of +fname+
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
      (dotimes (i +file-size+)
	(format of "~a~%" line)))))

;; -----------------------------------------------------------

(defun run-test-read-line ()
  "Count lines using standard READ-LINE."

  (declare #.+standard-optimize-settings+)

  (with-open-file (if +fname+
		      :direction :input)

    (loop :for line = (read-line if nil)
       :while line :count line)))

;; -----------------------------------------------------------

(defun run-test-read-sequence-string ()
  "Count lines by reading data in chunks into a string."

  (declare #.+standard-optimize-settings+)

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

  (declare #.+standard-optimize-settings+)

  (with-open-file (input-stream +fname+
				:direction :input
				:element-type '(unsigned-byte 8))
    (let ((buf (make-array #.(* 16 +kb+) :element-type (stream-element-type input-stream)))
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

(defun run-test-read-sequence-by-char ()
  "Read the file char by char."
  (declare #.+standard-optimize-settings+)
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

  (declare #.+standard-optimize-settings+)

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

(defun run ()
  (prepare)

  (format t "WARMING UP~%")
  (dotimes (i 10) (run-test-read-line))

  (format t "READ-LINE~%")
  (time (dotimes (i 10) (run-test-read-line)))

  (format t "READ-SEQUENCE~%")
  (time (dotimes (i 10) (run-test-read-sequence-string)))

  (format t "RUN-TEST-READ-SEQUENCE-BYTE-ARRAY~%")
  (time (dotimes (i 10) (run-test-read-sequence-byte-array)))

  (format t "RUN-TEST-READ-SEQUENCE-BY-CHAR~%")
  (time (dotimes (i 10) (RUN-TEST-READ-SEQUENCE-BY-CHAR)))

  (format t "RUN-TEST-READ-SEQUENCE-BY-BYTE~%")
  (time (dotimes (i 10) (RUN-TEST-READ-SEQUENCE-BY-BYTE)))

  )

;; EOF