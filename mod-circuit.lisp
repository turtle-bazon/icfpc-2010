
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

(defun swap-wires (pos1 pos2 s-circuit)
  (rotatef (nth-in-gate pos1 s-circuit)
           (nth-in-gate pos2 s-circuit))
  s-circuit)

(defun swap-with-ext-input (pos s-circuit)
  (rotatef (nth-in-gate pos s-circuit)
	   (external-input s-circuit))
  s-circuit)

(defun swap-with-ext-output (pos s-circuit)
  (rotatef (nth-in-gate pos s-circuit)
	   (external-output s-circuit))
  s-circuit)

(defun add-loop-with-ext-gate (s-circuit gate &key (X-role :input))
  (let ((new (circuit-length s-circuit)))
    (push-node s-circuit)
    (setf (nth-gate new s-circuit)
	  (list (list (list :L new) (list :R new))
		(list (list :L new) (list :R new))))
    (let ((gate-pos (list (second gate)
			  (let ((pos
				 (if (eq (first gate) :L)
				     1 2)))
			    (if (eq X-role :input)
				pos (+ pos 2))
			    pos))))
      (if (eq X-role :input)
	  (swap-with-ext-input gate-pos s-circuit)
	  (swap-with-ext-output gate-pos s-circuit))
      s-circuit)))

(defun add-loop (s-circuit input output)
  (let ((new (circuit-length s-circuit)))
    (push-node s-circuit)
    (setf (nth-gate new s-circuit) 
          (list (list (list :L new) (list :R new))
                (list (list :L new) (list :R new))))
    (let ((input-pos (list (second input)
                           (if (eq (first input) :L)
                               3 4)))
          (output-pos (list (second output)
                            (if (eq (first output) :L)
                                1 2))))
      (swap-wires (list new 2) input-pos s-circuit)
      (swap-wires (list new 4) output-pos s-circuit)
      s-circuit)))
