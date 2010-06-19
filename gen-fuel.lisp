(in-package :cl-user)

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
      (push (strings (elt left-column  i) "0#"
                     (elt right-column i) ",")
            factory))
    (push (strings (elt left-column  (1- nodes-num)) "0#"
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
  (let ((string (submit-factory factory)))
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



;;; new variant

(defun range (n)
  (let (rez)
    (dotimes (i n)
      (push i rez))
    rez))

(defun random-elt (lst)
  (elt lst (random (length lst))))

(defun make-node-array (n)
  "Return 2d array of indexes, that can be used as a basis for factory"
  (let ((rez (make-array (list n 4) :initial-element nil))
        (1st (range n))
        (2nd (range n))
        (i 0))
    (dotimes (i n)
      (let* ((r1 (random-elt 1st))
             (r2 (random-elt 2nd))
             (j1 (+ 2 (random 2)))
             (j2 (+ 2 (random 2))))
        (when (aref rez r1 j1)
          (setf j1 (if (= j1 2) 3 2)))
        (when (aref rez r2 j2)
          (setf j2 (if (= j2 2) 3 2)))
        (when (= r1 r2)
          (setf j1 2
                j2 3))
        (setf (aref rez i 0)   r1
              (aref rez i 1)   r2
              (aref rez r1 j1) i
              (aref rez r2 j2) i)

        (setf 1st (remove r1 1st)
              2nd (remove r2 2nd))))

    rez))

(defun make-circuit (n)
  (let ((proto (make-node-array n)))
    (