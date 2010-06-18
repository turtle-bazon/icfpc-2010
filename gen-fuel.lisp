(defun make-node (in1 in2 out1 out2 &key no-newline)
  (concatenate 'string
	       in1 in2 "0" "\\#"
	       out1 out2 (if no-newline "" "\\n")))

(defun possible-connects-list (nodes-num)
  (let ((list nil))
    (dotimes (i nodes-num)
      (let ((strnum (write-to-string i)))
	(push (concatenate 'string strnum "L") list)
	(push (concatenate 'string strnum "R") list)
	(push (concatenate 'string strnum "L") list)
	(push (concatenate 'string strnum "R") list)))
    (nreverse list)))

(defun shuffle-list (l)
  (loop for i below (length l) do
    (rotatef
      (elt l i)
      (elt l (random (length l)))))
  l)

(defun random-factory (nodes-num)
  (let ((strlst nil)
	(connects (shuffle-list (possible-connects-list nodes-num)))
	(head-connect (concatenate 'string (write-to-string (1- nodes-num)) "L")))
    (setf connects (remove head-connect connects :test #'string=))
    (push head-connect strlst)
    (push ":\\n" strlst)
    (dotimes (i (1- nodes-num))
      (push (make-node (pop connects) (pop connects) (pop connects) (pop connects))
	    strlst))
    (if (= (random 2) 1)
	(push (make-node (pop connects) "X" (pop connects) "X" :no-newline t) strlst)	      
	(push (make-node "X" (pop connects) "X" (pop connects) :no-newline t) strlst))
    (push ":\\n" strlst)
    (push head-connect strlst)
    (apply #'concatenate 'string (nreverse strlst))))

(defun test-submit-factory (factory)
  (let ((string
	 (apply #'concatenate 'string
		(coerce (with-auth (post-fuel "219" (random-factory 2))) 'list))))
    ;(if (ppcre:scan-to-strings "unexpected" string)
    ;"NO" "YES")))
    ))
