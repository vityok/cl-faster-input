;; A set of unit-tests that come as a complement to the reference
;; implementation suggested by Pascal Bourguignon:
;;
;; https://groups.google.com/forum/#!msg/comp.lang.lisp/8nzWKfdSySM/HL7DbRiP95oJ
;;

(defun test/read-line-into-sequence ()
  (let ((buffer (make-array 40 :element-type 'character :initial-element #\space)))
    (with-input-from-string (input "Hello
World
Howdy")
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 1 :end 1))
               '("                                        "
                 1 nil)))
      (assert (handler-case
                  (read-line-into-sequence buffer input
                                           :eof-error-p nil :eof-value :eof :recursivep nil
                                           :start 2 :end 1)
                (:no-error (&rest results)
                  (declare (ignore results))
                  nil)
                (error (err)
                  (declare (ignore err))
                  t)))
      (assert (handler-case
                  (read-line-into-sequence buffer input
                                           :eof-error-p nil :eof-value :eof :recursivep nil
                                           :start 41)
                (:no-error (&rest results)
                  (declare (ignore results))
                  nil)
                (error (err)
                  (declare (ignore err))
                  t)))
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 0))
               '("Hello                                   "
                 5 t)))
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 10 :end 13))
               '("Hello     Wor                           "
                 13 nil)))
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 20))
               '("Hello     Wor       ld                  "
                 22 t)))
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 37))
               '("Hello     Wor       ld               How"
                 40 nil)))
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 30))
               '(:eof 32 nil)))))

  (let ((buffer (make-list 40 :initial-element 0)))
    (with-input-from-string (input "Hello
World
Howdy")
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 1 :end 1))
               '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                  0 0 0 0 0 0 0 0 0 0 0 0 0) 1 nil)))
      (assert (handler-case
                  (read-line-into-sequence buffer input
                                           :eof-error-p nil :eof-value :eof :recursivep nil
                                           :start 2 :end 1)
                (:no-error (&rest results)
                  (declare (ignore results))
                  nil)
                (error (err)
                  (declare (ignore err))
                  t)))
      (assert (handler-case
                  (read-line-into-sequence buffer input
                                           :eof-error-p nil :eof-value :eof :recursivep nil
                                           :start 41)
                (:no-error (&rest results)
                  (declare (ignore results))
                  nil)
                (error (err)
                  (declare (ignore err))
                  t)))
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 0))
               '((#\H #\e #\l #\l #\o 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 5 t)))
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 10 :end 13))
               '((#\H #\e #\l #\l #\o 0 0 0 0 0 #\W #\o #\r 0 0 0 0 0 0
                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 13 nil)))
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 20))
               '((#\H #\e #\l #\l #\o 0 0 0 0 0 #\W #\o #\r 0 0 0 0 0 0 0
                  #\l #\d 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 22 t) ))
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 37))
               '((#\H #\e #\l #\l #\o 0 0 0 0 0 #\W #\o #\r 0 0 0 0 0 0 0
                  #\l #\d 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 #\H #\o #\w) 40
                 nil)))
      (assert (equal
               (multiple-value-list
                (read-line-into-sequence buffer input
                                         :eof-error-p nil :eof-value :eof :recursivep nil
                                         :start 30))
               '(:eof 32 nil)))))
  :success)

(test/read-line-into-sequence) 