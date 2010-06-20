(in-package :icfpc)

(defun (setf external-input) (val s-circuit)
  (setf (first s-circuit) val))

(defun (setf external-output) (val s-circuit)
  (setf (second s-circuit) val))

(defun (setf circuit-body) (val s-circuit)
  (setf (third s-circuit) val))

(defun (setf nth-gate) (val i s-circuit)
  (setf (nth i (circuit-body s-circuit)) val))

(defun (setf 1-in-gate) (val s-gate)
  (setf (first (first s-gate)) val))

(defun (setf 2-in-gate) (val s-gate)
  (setf (second (first s-gate)) val))

(defun (setf 3-in-gate) (val s-gate)
  (setf (first (second s-gate)) val))

(defun (setf 4-in-gate) (val s-gate)
  (setf (second (second s-gate)) val))

(defun push-node (s-circuit)
  (setf (circuit-body s-circuit)
	(append (circuit-body s-circuit)
		'(()))))

(defun nth-in-gate (pos s-circuit)
  ;; pos: '(номер гейта позиция), позиция = 1,2,3,4
  (funcall
   (ecase (second pos)
     (1 #'1-in-gate)
     (2 #'2-in-gate)
     (3 #'3-in-gate)
     (4 #'4-in-gate))
   (nth-gate (first pos) s-circuit)))

(defun (setf nth-in-gate) (val pos s-circuit)
  (ecase (second pos)
    (1 (setf (1-in-gate (nth-gate (first pos) s-circuit)) val))
    (2 (setf (2-in-gate (nth-gate (first pos) s-circuit)) val))
    (3 (setf (3-in-gate (nth-gate (first pos) s-circuit)) val))
    (4 (setf (4-in-gate (nth-gate (first pos) s-circuit)) val))))



#|
(defun add-loop (s-circuit input output)
  (let ((new (circuit-length s-circuit)))
    (push-node s-circuit)
    (setf (nth-gate new s-circuit) 
	  (list (list (list :L new) (list :R new))
		(list (list :L new) (list :R new))))
    (if (eq (first input) :L)
	(rotatef (nth-in-gate (list new 2)

	(rotatef (2-in-gate (nth-gate new s-circuit))
		 (3-in-gate (nth-gate (second input) s-circuit)))
	(rotatef (2-in-gate (nth-gate new s-circuit))
		 (4-in-gate (nth-gate (second input) s-circuit))))
    (if (eq (first output) :L)
	(rotatef (4-in-gate (nth-gate new s-circuit))
		 (1-in-gate (nth-gate (second output) s-circuit)))
	(rotatef (4-in-gate (nth-gate new s-circuit))
		 (2-in-gate (nth-gate (second output) s-circuit))))
    s-circuit))
    
  
|#
