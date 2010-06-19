;;;
;;; utils.lisp -- unsorted utils.
;;;

;;; strings:

(defun strings (&rest strings)
  (apply #'concatenate 'string strings))

;;; sequences:

(defun range (n)
  (let (rez)
    (dotimes (i n)
      (push i rez))
    rez))

(defun random-elt (lst)
  (elt lst (random (length lst))))

;;; arrays:

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

;;; hash tables:

(defun hash-table-from-list (lst &optional test)
  (loop
     :with ht = (make-hash-table :test (or test 'eql))
     :for (k v) :on lst :by #'cddr
     :do (setf (gethash k ht) v)
     :finally (return ht)))
