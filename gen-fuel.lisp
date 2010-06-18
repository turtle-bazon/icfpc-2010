(defun strings (&rest strings)
  (apply #'concatenate 'string strings))

(defun possible-connects-list (nodes-num)
  (let ((list nil))
    (dotimes (i (1- nodes-num))
      (let ((strnum (write-to-string i)))
	(push (strings strnum "L") list)
	(push (strings strnum "R") list)))
    (push "X" list)
    (push (strings (write-to-string (1- nodes-num)) "R") list)
    (nreverse list)))

(defun shuffle-list (l)
  (loop for i below (length l) do
    (rotatef
      (elt l i)
      (elt l (random (length l)))))
  l)

(defun random-column (nodes-num)
  (let ((strlst nil)
	(connects (shuffle-list (possible-connects-list nodes-num))))
    (dotimes (i nodes-num)
      (push (strings (pop connects) (pop connects)) strlst))
    (nreverse strlst)))

(defun random-factory (nodes-num)
  (let ((left-column (random-column nodes-num))
	(right-column (random-column nodes-num))
	(header (strings (write-to-string (1- nodes-num)) "L"))
	(factory nil))
    (push (strings header ":") factory)
    (dotimes (i (1- nodes-num))
      (push (strings (elt left-column i) "0#" (elt right-column i) ",") factory))
    (push (strings (elt left-column (1- nodes-num)) "0#"
		   (elt right-column (1- nodes-num)) ":")
	  factory)
    (push header factory)
    (apply #'strings (nreverse factory))))

(defun print-factory (factory &optional (stream t))
  (dotimes (i (length factory))
    (format stream "~c" (elt factory i))
    (when (or (char= (elt factory i) #\,)
	      (char= (elt factory i) #\:))
      (terpri stream))))

(defun submit-factory (factory)
  (apply #'strings
	 (coerce (with-auth (post-fuel "219" factory)) 'list)))

(defun test-submit-factory (factory)
  (let ((string
	 (submit-factory factory)))
    (when (not (or (ppcre:scan-to-strings "unexpected" string)
		   (ppcre:scan-to-strings "inconsistent" string)
		   (ppcre:scan-to-strings "illegal" string)))
      (list factory string))))

(defun search-proper-factories (factory-size max-iter)
  (let ((proper-factories nil))
    (dotimes (i max-iter)
      (let ((result (test-submit-factory (random-factory factory-size))))
	(if result (push result proper-factories))))
    (nreverse proper-factories)))
      
