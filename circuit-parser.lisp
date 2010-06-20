;;;
;;; circuit-parser.lisp -- parse circuits.
;;;

(in-package :icfpc)

;;; parse string to s-circuit form:

(defun delete-whitespaces (string)
  (coerce
    (loop for char across string
          if (not (member char '(#\Space #\Newline)))
             collect char)
    'string))

(defun parse-token (string)
  (cond ((scan "L" string)    (list :l (parse-integer string :junk-allowed t)))
        ((scan "R" string)    (list :r (parse-integer string :junk-allowed t)))
        ((string= "X" string) (list :x))))

(defun %parse-gate (string)
  (register-groups-bind (left rigth)
                        ("(\\d*[L|R|X])(\\d*[L|R|X])" string)
    (list (parse-token left) (parse-token rigth))))

(defun parse-gate (string)
  (let* ((split (split-sequence #\# string))
         (left  (subseq (first split) 0 (1- (length (first split)))))
         (rigth (second split)))
    (list (%parse-gate left) (%parse-gate rigth))))

(defun parse-circuit (string)
  (let* ((split           (split-sequence #\: (delete-whitespaces string)))
         (external-input  (first split))
         (external-output (third split))
         (circuit-body    (mapcar #'(lambda (e)
                                      (if (string= e "")
                                          nil
                                          (parse-gate e)))
                                  (split-sequence #\, (second split)))))
    (list (parse-token external-input) (parse-token external-output) circuit-body)))

;;; s-circuit accessors:

(defun external-input (s-circuit)
  (first s-circuit))

(defun external-output (s-circuit)
  (second s-circuit))

(defun circuit-body (s-circuit)
  (third s-circuit))

(defun circuit-length (s-circuit)
  (length (circuit-body s-circuit)))

(defun nth-gate (i s-circuit)
  (nth i (circuit-body s-circuit)))

(defun 1-in-gate (s-gate)
  (first (first s-gate)))

(defun 2-in-gate (s-gate)
  (second (first s-gate)))

(defun 3-in-gate (s-gate)
  (first (second s-gate)))

(defun 4-in-gate (s-gate)
  (second (second s-gate)))

(defun point-value (s-point)
  (typecase s-point
    (atom nil)
    (cons (second s-point))))

;;; convert s-circuit to node-array:

(defun make-node-array (s-circuit)
  "Return 2d array of indexes, that can be used as a basis for factory"
  (let ((array (make-array (list (circuit-length s-circuit) 4) :initial-element nil)))
    (dotimes (i (circuit-length s-circuit))
      (let ((s-gate (nth-gate i s-circuit)))
        (print s-gate)
        (setf (aref array i 0) (1-in-gate s-gate)
              (aref array i 1) (2-in-gate s-gate)
              (aref array i 2) (3-in-gate s-gate)
              (aref array i 3) (4-in-gate s-gate))))
    array))

;;; back convertion s-circuit to factory-string:

(defun unparse-token (token)
  (case (first token)
    (:l (format nil "~AL" (second token)))
    (:r (format nil "~AR" (second token)))
    (:x "X")))

(defun unparse-gate (gate)
  (format nil "~A~A0#~A~A,~%"
          (unparse-token (1-in-gate gate))
          (unparse-token (2-in-gate gate))
          (unparse-token (3-in-gate gate))
          (unparse-token (4-in-gate gate))))

(defun unparse-gates (gates)
  (apply #'concatenate 'string (mapcar #'unparse-gate gates)))

(defun unparse-circuit (s-circuit)
  (format nil "~A:~%~A:~A"
          (unparse-token (external-input s-circuit))
          (unparse-gates (circuit-body s-circuit))
          (unparse-token (external-output s-circuit))))
