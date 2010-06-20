(in-package :icfpc)

(defun circuit-connects (s-circuit)
  (let ((connects nil))
    (dotimes (i (circuit-length s-circuit))
      (let ((ith-gate (nth-gate i s-circuit)))
	(push (list (3-in-gate ith-gate) (list :L i)) connects)
	(push (list (4-in-gate ith-gate) (list :R i)) connects)
	(push (list (list :L i) (1-in-gate ith-gate)) connects)
	(push (list (list :R i) (2-in-gate ith-gate)) connects)))
    (remove-duplicates (nreverse connects) :test #'equal)))

(defun convert-side (side)
  (ecase side
    (:X #\e)
    (:L #\w)
    (:R #\e)))

(defun format-connect (connect stream)
  (format stream "~A:~A->~A:~A;~%"
	  (or (second (first connect)) :X)
	  (convert-side (first (first connect)))
	  (or (second (second connect)) :X)
	  (convert-side (first (second connect)))))
    

(defun circuit->dotfile (s-circuit file)
  (with-open-file (s file
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (let ((connects (circuit-connects s-circuit)))
      (format s "digraph A {~%~%rankdir=tb;~%~%")
      (dolist (c connects)
	(format-connect c s))
      (format s "~%};~%"))
    file))
