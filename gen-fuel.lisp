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
  (loop
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
       (when (connected? rez)
         (return rez)))))

(defun connected? (2d-array)
  (let* ((n (array-dimension 2d-array 0))
         (gates (range n)))
    (labels ((recur (next step)
               (unless (= step 0)
                 (setf gates (remove next gates))
                 (unless gates
                   (return-from connected? t))
                 (recur (aref 2d-array next 0) (1- step))
                 (recur (aref 2d-array next 1) (1- step)))))
      (recur 0 n))))

(defun make-circuit (n)
  (let ((proto (make-node-array n))
        rez)
    (dotimes (i n)
      (dotimes (j 4)
        (let* ((circuit (copy-array proto))
               (j1 (if (logbitp 0 j) 1 0))
               (j2 (if (logbitp 1 j) 2 3))
               (in-subst (aref circuit i j1))
               (ou-subst (aref circuit i j2))
               (in-j (if (= (aref circuit in-subst 2) i) 2 3))
               (ou-j (if (= (aref circuit ou-subst 0) i) 0 1)))
            (setf (aref circuit in-subst in-j) ou-subst
                  (aref circuit ou-subst ou-j) in-subst
                  (aref circuit i j1) "X"
                  (aref circuit i j2) "X")
            (push circuit rez))))
    (values (random-elt rez)
            rez)))

(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of <_:arg array />, with same
<_:arg fill-pointer /> and <_:arg adjustab />ility (if any)
as the original, unless overridden by the keyword arguments"
  (let ((dims (array-dimensions array)))
    ;; Dictionary entry for ADJUST-ARRAY requires adjusting a
    ;; displaced array to a non-displaced one to make a copy
    (adjust-array
     (make-array dims
                 :element-type element-type :fill-pointer fill-pointer
                 :adjustable adjustable :displaced-to array)
     dims)))
