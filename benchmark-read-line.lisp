
;; How to run with Clozure CL:
;;
;; lx86cl --load benchmark-read-line.lisp --eval '(run)' --eval '(quit)'


(defconstant +kb+ 1024)
(defconstant +mb+ #.(* 1024 1024))
(defconstant +file-size+ (* 10 +kb+)
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
  (with-open-file (if +fname+
		      :direction :input)

    (loop :for line = (read-line if nil)
       :while line
       :count line)))

;; -----------------------------------------------------------

(defun run-test-read-sequence-string ()
  "Count lines by reading data in chunks into a string."

  (with-open-file (is +fname+
		      :direction :input)

    (let ((line (make-string (* 4 +kb+)
			     :initial-element #\?))
	  (count 0))
      
      (loop :while (> (read-sequence line is) 0)
	 :do (incf count (count #\Newline line :test #'char=)))
      count)))

;; -----------------------------------------------------------

(defun run-test-read-sequence-byte-array ()
  "Reading into a bytes array.

Taken a que from Stackoverflow:
http://stackoverflow.com/a/15813006"

  (with-open-file (input-stream +fname+
				:direction :input
				:element-type '(unsigned-byte 8))
    (let ((buf (make-array 4096 :element-type (stream-element-type input-stream)))
	  (count 0))
      (loop :for pos = (read-sequence buf input-stream)
	 :while (plusp pos)
	 :do (incf count (count #.(char-code #\Newline) buf :test #'=))))))

;; -----------------------------------------------------------

(defun run ()
  (prepare)

  (format t "READ-LINE~%")
  (time (dotimes (i 10) (run-test-read-line)))

;;  (format t "READ-SEQUENCE~%")
;;  (time (dotimes (i 10) (run-test-read-sequence)))

  (format t "RUN-TEST-READ-SEQUENCE-BYTE-ARRAY~%")
  (time (dotimes (i 10) (run-test-read-sequence-byte-array)))
  )

;; EOF