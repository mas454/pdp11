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
	(#x15c0 (format t "~4,'0x ~4,'0x mov $~x, r0"
			(read16 mem i) (read16 mem (+ i 4)) (read16 mem (+ i 4)))
	        (incf i 4))
	
        (#x8904 (format t "~4,'0x sys 4 ; write" (read16 mem i))
	        (incf i 2))
	)
      )))

;(defvar *vec* (make-array 5 :fill-pointer 0 :adjustable t))
(defparameter *testmem* (vector #xc0 #x15 #x01 #x00))
