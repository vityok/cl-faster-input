;;; Benchmark reading file line-by-line

;; Standard read-line implementations allocate a new string for every
;; line read. This is convenient for some applications, but comes at a
;; cost: higher memory usage and as the result longer execution time.
;;
;; An alternative approach was proposed in a discussion sparked by a
;; question by Alexandre Landi he asked on June 3, 2015 in
;; comp.lang.lisp:
;;
;; https://groups.google.com/forum/#!msg/comp.lang.lisp/8nzWKfdSySM/jQuaVBQpkMEJ
;;
;; Instead of allocating a new string for every line read, some
;; application would find it totally sufficient to recycle the same
;; buffer for every line. A reference implementation was suggested by
;; Pascal Bourguignon:
;;
;; https://groups.google.com/forum/#!msg/comp.lang.lisp/8nzWKfdSySM/HL7DbRiP95oJ
;;
;; This benchmark is intended to compare performance of both
;; approaches.
;;
;; sbcl --load benchmark-read-line.lisp --eval '(run)' --eval '(quit)'
;;
;; sbcl --eval '(compile-file "benchmark-read-line.lisp")' 
;; sbcl --load benchmark-read-line.fasl --eval '(run)' --eval '(quit)'

#+ignore
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :costly-assert *features*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize speed)))

;; the same as in benchmark-count-lines.lisp
(defconstant +fname+ "data.tmp")

(declaim (inline %read-char-until %finish-read-line-into-sequence))

(defun %read-char-until (stream recursivep store)
  (loop
    :for ch := (read-char stream nil nil recursivep)
    :while (and ch (funcall store ch))
    :finally (return ch)))

(defun %finish-read-line-into-sequence (ch buffer stream eof-error-p eof-value start)
  (if (null ch)
      (if eof-error-p
          (error 'end-of-file :stream stream)
          (values eof-value start nil))
      (values buffer start (eql ch #\Newline))))

(defgeneric read-line-into-sequence (sequence input-stream
                                     &key
                                       eof-error-p eof-value recursivep
                                       start end)

  (:documentation "

Reads characters from the INPUT-STREAM until a #\\Newline is found, and
store the characters read into the SEQUENCE, from START, up to below
END.  If END is reached before the #\\Newline character is read, then
reading stops there and the third result value is NIL.  The #\Newline
character is not stored.   No other slot of the SEQUENCE is modified
than those between START and POSITION.

RETURN:         VALUE, POSITION, NEWLINE-SEEN-P

VALUE:          Either SEQUENCE or EOF-VALUE depending on whether an
                end-of-file has been seen.

SEQUENCE:       A sequence (OR LIST VECTOR).  If specialized, the vector
                must have an element-type that is a supertype of the
                stream element-type.  If a fill-pointer is present, it
                is ignored.

POSITION:       The index in the SEQUENCE of the first element not
                written. (<= START POSITION (OR END (LENGTH BUFFER)))


NEWLINE-SEEN-P: Whether a #\\Newline has been read.

INPUT-STREAM:   an input stream.  The element-type of the INPUT-STREAM
                must be a subtype of CHARACTER.

EOF-ERROR-P:    a generalized boolean. The default is true.  If true,
                then an END-OF-FILE error is signaled upon end of file.

EOF-VALUE:      an object. The default is NIL.

RECURSIVE-P:    a generalized boolean. The default is NIL.  If
                RECURSIVE-P is true, this call is expected to be
                embedded in a higher-level call to read or a similar
                function used by the Lisp reader.

START, END:     bounding index designators of SEQUENCE.
                The defaults for START and END are 0 and NIL, respectively.

")

  (:method ((buffer vector) (stream stream) &key
                                              (eof-error-p t)
                                              (eof-value nil)
                                              (recursivep nil)
                                              (start 0)
                                              (end nil))
    (let ((end (or end (length buffer))))
      (check-type start (and fixnum (integer 0)))
      (check-type end   (or null (and fixnum (integer 0))))
      (cond
        ((and (= end start) (<= start (length buffer)))
         (values buffer start nil))
        ((or (< end start) (< (length buffer) start))
         (error "Bad interval for sequence operation on ~S: start = ~A, end = ~A"
                buffer start end))
        (t
         (%finish-read-line-into-sequence
          (%read-char-until stream recursivep
                            (lambda (ch)
                              (if (char= #\Newline ch)
                                  nil
                                  (progn
                                    (setf (aref buffer start) ch)
                                    (incf start)
                                    (< start end)))))
          buffer stream eof-error-p eof-value start)))))

  (:method ((buffer list) (stream stream) &key
                                            (eof-error-p t)
                                            (eof-value nil)
                                            (recursivep nil)
                                            (start 0)
                                            (end nil))
    (check-type start (and fixnum (integer 0)))
    (check-type end   (or null (and fixnum (integer 0))))
    (let ((current buffer))
      (loop
        :repeat start
        :do (if (null current)
                (error "Bad interval for sequence operation on ~S: start = ~A, end = ~A"
                       buffer start end)
                (pop current)))
      #+costly-assert (assert (<= start (length buffer)))
      (cond
        ((if end
             (= start end)
             (null current))
         (values buffer start nil))
        ((or (null current) (and end (< end start)))
         (error "Bad interval for sequence operation on ~S: start = ~A, end = ~A"
                buffer start end))
        (t
         #+costly-assert (assert (and (or (null end) (<= start end))
                                      (< start (length buffer))))
         (%finish-read-line-into-sequence
          (%read-char-until stream recursivep
                            (lambda (ch)
                              (if (char= #\Newline ch)
                                  nil
                                  (progn
                                    (setf (car current) ch
                                          current (cdr current))
                                    (incf start)
                                    (if end
                                        (< start end)
                                        current)))))
          buffer stream eof-error-p eof-value start))))))

(defun run-buffer ()
  (let ((buffer (make-array 80 :element-type 'character :initial-element #\space))
	(lines 0))
    (with-open-file (is +fname+ :direction :input)
		    (loop for (VALUE POSITION NEWLINE-SEEN-P) = (multiple-value-list (read-line-into-sequence buffer is :eof-error-p nil))
			  while VALUE
			  do (incf lines)))))

(defun run-read-line ()
  (with-open-file (is +fname+ :direction :input)
		  (loop for line = (read-line is nil)
			while line
			count line)))
  

(defun run ()
  (format t "RUN-BUFFER~%")
  (time (dotimes (i 10) (run-buffer)))
  (format t "RUN-READ-LINE~%")
  (time (dotimes (i 10) (run-read-line))))