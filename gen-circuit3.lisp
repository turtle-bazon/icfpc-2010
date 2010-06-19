;;;
;;; random-factory.lisp
;;;

(in-package :icfpc)

(defun make-gate-array (n)
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
  (let ((proto (make-gate-array n))
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
