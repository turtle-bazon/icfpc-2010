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
        ((string= "X" string) :x)))

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
  (let* ((split-one       (split-sequence #\: (delete-whitespaces string)))
         (external-input  (first split-one))
         (external-output (third split-one))
         (circuit-body    (mapcar #'(lambda (e)
                                      (parse-gate e))
                                  (split-sequence #\, (second split-one)))))
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

;;; examples:

(parse-circuit
"19L:
12R13R0#1R12R,
14R0L0#4R9L,
9R10R0#3L8L,
2L17R0#5L9R,
15R1L0#10R13R,
3L18R0#6L15L,
5L11R0#13L12L,
19R16R0#11R8R,
2R7R0#11L10L,
1R3R0#18L2L,
8R4L0#16L2R,
8L7L0#15R6R,
6R0R0#14L0L,
6L4R0#14R0R,
12L13L0#17L1L,
5R11L0#16R4L,
10L15L0#17R7R,
14L16L0#18R3R,
9L17L0#19R5R,
X18L0#X7L:
19L"
)

;; =>

#|
((:L 19) (:L 19)
 ((((:R 12) (:R 13)) ((:R 1) (:R 12))) (((:R 14) (:L 0)) ((:R 4) (:L 9)))
  (((:R 9) (:R 10)) ((:L 3) (:L 8))) (((:L 2) (:R 17)) ((:L 5) (:R 9)))
  (((:R 15) (:L 1)) ((:R 10) (:R 13))) (((:L 3) (:R 18)) ((:L 6) (:L 15)))
  (((:L 5) (:R 11)) ((:L 13) (:L 12))) (((:R 19) (:R 16)) ((:R 11) (:R 8)))
  (((:R 2) (:R 7)) ((:L 11) (:L 10))) (((:R 1) (:R 3)) ((:L 18) (:L 2)))
  (((:R 8) (:L 4)) ((:L 16) (:R 2))) (((:L 8) (:L 7)) ((:R 15) (:R 6)))
  (((:R 6) (:R 0)) ((:L 14) (:L 0))) (((:L 6) (:R 4)) ((:R 14) (:R 0)))
  (((:L 12) (:L 13)) ((:L 17) (:L 1))) (((:R 5) (:L 11)) ((:R 16) (:L 4)))
  (((:L 10) (:L 15)) ((:R 17) (:R 7))) (((:L 14) (:L 16)) ((:R 18) (:R 3)))
  (((:L 9) (:L 17)) ((:R 19) (:R 5))) ((:X (:L 18)) (:X (:L 7)))))
|#

(make-node-array
  (parse-circuit
"19L:
12R13R0#1R12R,
14R0L0#4R9L,
9R10R0#3L8L,
2L17R0#5L9R,
15R1L0#10R13R,
3L18R0#6L15L,
5L11R0#13L12L,
19R16R0#11R8R,
2R7R0#11L10L,
1R3R0#18L2L,
8R4L0#16L2R,
8L7L0#15R6R,
6R0R0#14L0L,
6L4R0#14R0R,
12L13L0#17L1L,
5R11L0#16R4L,
10L15L0#17R7R,
14L16L0#18R3R,
9L17L0#19R5R,
X18L0#X7L:
19L"
))

;; =>

#|
#2A(((:R 12) (:R 13) (:R 1) (:R 12))
    ((:R 14) (:L 0) (:R 4) (:L 9))
    ((:R 9) (:R 10) (:L 3) (:L 8))
    ((:L 2) (:R 17) (:L 5) (:R 9))
    ((:R 15) (:L 1) (:R 10) (:R 13))
    ((:L 3) (:R 18) (:L 6) (:L 15))
    ((:L 5) (:R 11) (:L 13) (:L 12))
    ((:R 19) (:R 16) (:R 11) (:R 8))
    ((:R 2) (:R 7) (:L 11) (:L 10))
    ((:R 1) (:R 3) (:L 18) (:L 2))
    ((:R 8) (:L 4) (:L 16) (:R 2))
    ((:L 8) (:L 7) (:R 15) (:R 6))
    ((:R 6) (:R 0) (:L 14) (:L 0))
    ((:L 6) (:R 4) (:R 14) (:R 0))
    ((:L 12) (:L 13) (:L 17) (:L 1))
    ((:R 5) (:L 11) (:R 16) (:L 4))
    ((:L 10) (:L 15) (:R 17) (:R 7))
    ((:L 14) (:L 16) (:R 18) (:R 3))
    ((:L 9) (:L 17) (:R 19) (:R 5))
    (:X (:L 18) :X (:L 7)))
|#
