


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
    (push -1 inputs)
    (dotimes (i gates-num)
      (push i inputs) (push i inputs)) ;; (cons 'I0 i)?
    (reverse inputs)))

(defun free-outputs (gates-num)
  (let ((outputs nil))
    (push -1 outputs)
    (dotimes (i gates-num)
      (push i outputs) (push i outputs)) ;; (cons 'O0 i)?
    (reverse outputs)))

(defun make-factory (gates-num)
  (let ((factory nil))
    (push (list -1 nil nil) factory)
    (dotimes (i gates-num)
      (push 
       (apply #'defgate i (make-list 4))
       factory))
    (nreverse factory)))

(defun external-gate (factory) (first factory))
(defun internal-gates (factory) (cdr factory))
(defun external-gate-input (factory) (second (first factory)))
(defun external-gate-output (factory) (third (first factory)))
(defun (setf external-gate-input) (index factory) (setf (second (first factory)) index))
(defun (setf external-gate-output) (index factory) (setf (third (first factory)) index))

(defun print-factory (factory &optional (stream t))
  (format stream "~A:~%" (external-gate-input factory)) ;; or output???
  (dolist (gate (internal-gates factory))
    (format stream  "~A: I0=~A, I1=~A | O0=~A, O1=~A~%"
	    (gate-index gate)
	    (I0-gate gate) (I1-gate gate) (O0-gate gate) (O1-gate gate)))
  (format stream "~A~%" (external-gate-output factory))) ;; or input???

;(defun ith-gate (factory index)
;  (assoc index factory :test #'=))

(defun ith-gate (factory index) (elt factory (1+ index)))
(defun random-element (list) (elt list (random (length list))))

(defun remove-one-elem (elem list)
  (setf list (remove elem list :count 1)))

(defun insert-connection (factory input-gate output-gate)
  (assert (and (member input-gate *free-inputs*)
	       (member output-gate *free-outputs*))
	  nil "Such input or output is already in use")
  ;; быдлокод inside
  (let ((input-type
	 (cond ((= output-gate -1) 'X)
	       ((null (I0-gate (ith-gate factory output-gate))) 'I0)
	       ((null (I1-gate (ith-gate factory output-gate))) 'I1)
	       (t (error "Illegal connection: ~A gate has already two inputs" output-gate))))
	(output-type
	 (cond ((= input-gate -1) 'X)
	       ((null (O0-gate (ith-gate factory input-gate))) 'O0)
	       ((null (O1-gate (ith-gate factory input-gate))) 'O1)
	       (t (error "Illegal connection: ~A gate has already two outputs" input-gate)))))
    (case input-type
      (I0 (setf (I0-gate (ith-gate factory output-gate)) input-gate))
      (I1 (setf (I1-gate (ith-gate factory output-gate)) input-gate))
      (X (setf (external-gate-input factory) input-gate)))
    (case output-type
      (O0 (setf (O0-gate (ith-gate factory input-gate)) output-gate))
      (O1 (setf (O1-gate (ith-gate factory input-gate)) output-gate))
      (X (setf (external-gate-output factory) output-gate)))
    (setf *free-inputs* (remove input-gate *free-inputs* :count 1))
    (setf *free-outputs* (remove output-gate *free-outputs* :count 1))
    factory))
    
(defun init-factory (size)
  (defparameter *factory* (make-factory size))
  (defparameter *free-inputs* (free-inputs size))
  (defparameter *free-outputs* (free-outputs size)))

;; filling factory:
;; (print-factory
;;	  (insert-connection *factory*
;;			     (random-element *free-inputs*)
;;			     (random-element *free-outputs*)))
