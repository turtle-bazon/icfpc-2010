


(defun defgate (index I0 I1 O0 O1)
  (list index (list I0 I1 O0 O1)))

(defun gate-index (gate) (first gate))
(defun I0-gate (gate) (first (second gate)))
(defun I1-gate (gate) (second (second gate)))
(defun O0-gate (gate) (third (second gate)))
(defun O1-gate (gate) (fourth (second gate)))
(defun (setf I0-gate) (index gate) (setf (first (second gate)) index))
(defun (setf I1-gate) (index gate) (setf (second (second gate)) index))
(defun (setf O0-gate) (index gate) (setf (third (second gate)) index))
(defun (setf O1-gate) (index gate) (setf (fourth (second gate)) index))

(defun free-inputs (gates-num)
  (let ((inputs nil))
    (dotimes (i gates-num)
      (push i inputs) (push i inputs)) ;; (cons 'I0 i)?
    (reverse inputs)))

(defun free-outputs (gates-num)
  (let ((outputs nil))
    (dotimes (i gates-num)
      (push i outputs) (push i outputs)) ;; (cons 'O0 i)?
    (reverse outputs)))

(defun init-factory (gates-num)
  (let ((factory nil))
    (dotimes (i gates-num)
      (push 
       (apply #'defgate i (make-list 4))
       factory))
    (nreverse factory)))

(defun print-factory (factory &optional (stream t))
  (dolist (gate factory)
    (format stream  "~A: I0=~A, I1=~A | O0=~A, O1=~A~%"
	    (gate-index gate)
	    (I0-gate gate) (I1-gate gate) (O0-gate gate) (O1-gate gate))))

;(defun ith-gate (factory index)
;  (assoc index factory :test #'=))

(defun ith-gate (factory index) (elt factory index))
(defun random-element (list) (elt list (random (length list))))

(defun remove-one-elem (elem list)
  (setf list (remove elem list :count 1)))

(defun insert-connection (factory input-gate output-gate)
  (assert (and (member input-gate *free-inputs*)
	       (member output-gate *free-outputs*))
	  nil "Such input or output is already in use")
  ;; быдлокод inside
  (let ((I0? 
	 (cond ((null (I0-gate (ith-gate factory output-gate))) t)
	       ((null (I1-gate (ith-gate factory output-gate))) nil)
	       (t (error "Illegal connection: ~A gate has already two inputs" output-gate))))
	(O0?
	 (cond ((null (O0-gate (ith-gate factory input-gate))) t)
	       ((null (O1-gate (ith-gate factory input-gate))) nil)
	       (t (error "Illegal connection: ~A gate has already two outputs" input-gate)))))
    (if I0?
	(setf (I0-gate (ith-gate factory output-gate)) input-gate)
	(setf (I1-gate (ith-gate factory output-gate)) input-gate))
    (if O0?
	(setf (O0-gate (ith-gate factory input-gate)) output-gate)
	(setf (O1-gate (ith-gate factory input-gate)) output-gate))
    (setf *free-inputs* (remove input-gate *free-inputs* :count 1))
    (setf *free-outputs* (remove output-gate *free-outputs* :count 1))
    factory))
    
