(defparameter *r0* 0)
(defparameter *pc* 0)

(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

(defun load-memory (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((mem (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence mem s)
      mem)
    ))

(defun myhd (vec)
  (loop for x across vec
	for i = 0 then (+ i 1)
	do (when (= (rem i 16) 0)
	     (format t "~%~8,'0x " i))
	   (format t "~2,'0x " x)))

(defun read16 (mem i)
  (logior (elt mem i) (ash (elt mem (+ i 1)) 8)))

(defun reasem (mem)
  (let ((i 0))
    (while (< i (length mem))
      (format t "~4,'0x: " i)
      (case (read16 mem i)
	(#x15c0 (format t "~4,'0x ~4,'0x mov $~x, r0~%"
			(read16 mem i) (read16 mem (+ i 2)) (read16 mem (+ i 2)))
	        (incf i 4))

	(#x8901 (format t "~4,'0x sys 1 ; exit~%" (read16 mem i))
	        (incf i 2))
	
        (#x8904 (format t "~4,'0x sys 4 ; write~%" (read16 mem i))
	        (incf i 2)
		(format t "~4,'0x: ~4,'0x ; arg~%" i (read16 mem i))
		(incf i 2)
		(format t "~4,'0x: ~4,'0x ; arg~%" i (read16 mem i))
		(incf i 2))

	(t (format t "~4,'0x ???~%" (read16 mem i))
	   (incf i 2))
	)
      )))

(defun run-d (path)
  (let* ((mem (load-memory path))
	 (tsize (read16 mem 2))
	 (dsize (read16 mem 4)))
    (reasem (subseq mem 16 (+ 16 tsize) ))))

(defun mov-n-r0 (mem)
  (setf *r0* (read16 mem (+ *pc* 2)))
  (incf *pc* 4))

(defun sys-exit (mem)
  (setf *pc* -1))

(defun sys-write (mem)
  (let* ((s (read16 mem (+ *pc* 2)))
	 (len (read16 mem (+ *pc* 4)))
	 (str (map 'string #'character (subseq mem s (+ s len)))))
    (case *r0*
      (1 (format t str))
      )
    (incf *pc* 6)
    ))
(defun run1 (mem tsize)
  (let ((*pc* 0)
	(*r0* 0))
    (while (and (< *pc* tsize) (> *pc* -1))
	   (case (read16 mem *pc*)
	     (#x15c0 (mov-n-r0 mem))
	     (#x8901 (sys-exit mem))
	     (#x8904 (sys-write mem))
	     (t (format t "~4,'0x: ~4,'0x ???~%" *pc* (read16 mem *pc*))
		(setf *pc* -1))
	     )
	   )))

(defun run (path)
  (let* ((mem (load-memory path))
	 (tsize (read16 mem 2))
	 (dsize (read16 mem 4)))
    (run1 (subseq mem 16) tsize)))

;(defvar *vec* (make-array 5 :fill-pointer 0 :adjustable t))
(defparameter *testmem* (vector #xc0 #x15 #x01 #x00 
				#x04 #x89 #x10 #x00 #x06 #x00 ))
