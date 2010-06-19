;;;
;;; circuit-eval.lisp -- eval circuits.
;;;

(in-package :icfpc)

(defvar *circuit*)

;; node structure

(defstruct node 
  (out-r 0)
  (out-l 0)
  (new-l 0))

(defun make-nodeset (n)
  (let ((rez (make-array n)))
    (dotimes (i n)
      (setf (aref rez i) (make-node)))
    rez))

(defvar *nodes*)  ; (make-nodeset 20))

(defun node (i)
  (aref *nodes* i))

(defparameter *node-func*
  (hash-table-from-list '((0 . 0) (2 0)
                          (0 . 1) (2 2)
                          (0 . 2) (2 1)
                          (1 . 0) (2 1)
                          (1 . 1) (0 0)
                          (1 . 2) (1 2)
                          (2 . 0) (2 2)
                          (2 . 1) (1 1)
                          (2 . 2) (0 0))
                        #'equal))

(defun node-func (l r)
  (apply #'values (gethash (cons l r) *node-func*)))

(defun nodecall (i)
  (multiple-value-bind (o-r o-l)
      (node-func (out-val (in-l i))
                 (out-val (in-r i)))
    (let ((node (node i)))
      (setf (node-out-r node) o-r
            (node-new-l node) o-l
            (aref *nodes* i) node))))

;; get at data utils

(macrolet ((in (dir i)
             `(if (equal (elt *circuit* 0) (list ,dir ,i))
                  '(:x)
                  (let* ((array (elt *circuit* 2))
                         (in1 (caar (elt array ,i)))
                         (in2 (cadar (elt array ,i))))
                    (cond ((equal in1 '(:x)) in2)
                          ((equal in2 '(:x)) in1)
                          (t (let* ((out11 (caadr (elt array (cadr in1))))
                                    (out12 (cadadr (elt array (cadr in1)))))
                               (if (find (list ,dir ,i) (list out11 out12)
                                         :test #'equal)
                                   in1 in2))))))))
  (defun in-l (i) (in :l i))
  (defun in-r (i) (in :r i)))

(defvar *in* 0 "Current data at input")

(defun out-val (outspec &key force)
  (let ((i (cadr outspec)))
    (if i
        (if (eq (car outspec) :l)
            (if force
                (node-new-l (node i))
                (node-out-l (node i)))
            (node-out-r (node i)))
        *in*)))

(defun next-node (i)
  (unless (equal (elt *circuit* 1) (list :r i))  ; output is (:R i)
    (let* ((array (elt *circuit* 2))
           (out1 (caadr (elt array i)))
           (out2 (cadadr (elt array i))))
      (cond ((equal out1 '(:x)) (cadr out2))
            ((equal out2 '(:x)) (cadr out1))
            (t (let* ((in11 (caar (elt array (cadr out1))))
                      (in12 (cadar (elt array (cadr out1)))))
                 (cadr (if (find (list :r i) (list in11 in12)
                                 :test #'equal)
                           out1 out2))))))))

(defun circuit-eval (circuit input)
  (let* ((*circuit* circuit)
         (n (length (elt circuit 2)))
         (*nodes* (make-nodeset n))
         (start (cadr (elt circuit 0)))
         (end (elt circuit 1))
         rez)
    (dolist (x input)
      (let ((remaining-nodes (range n)))
        (labels ((calc-node (i)
                   (nodecall i)
                   (setf remaining-nodes (remove i remaining-nodes))
                   (let ((next (next-node i)))
                     (when (and next (find next remaining-nodes))
                       (calc-node next)))))
          (setf *in* x)
          (calc-node start)
          (push (out-val end :force t) rez)
          (loop :while remaining-nodes :do
             (calc-node (car remaining-nodes)))))
      (loop :for i :from 0 :to (1- n) :do
         (setf (node-out-l (aref *nodes* i))
               (node-new-l (aref *nodes* i)))))
;      (print *nodes*) (terpri))
    (nreverse rez)))

;; test

(defparameter *test-sequence*
  '(0 1 2 0 2 1 0 1 2 1 0 2 0 1 2 0 2))

;; 0L: X0R0#X0R: 0L  02120112100002120
(circuit-eval '((:L 0)
                (:L 0)
                ((((:X) (:R 0)) ((:X) (:R 0)))))
              *test-sequence*)

;; 0L: X0L0#0RX: 0R  22120221022022120
(circuit-eval '((:L 0)
                (:R 0)
                ((((:X) (:L 0)) ((:R 0) (:X)))))
              *test-sequence*)

;; 0R: 0RX0#X0L: 0L  01210221200001210
(circuit-eval '((:R 0)
                (:L 0)
                ((((:R 0) (:X)) ((:X) (:L 0)))))
              *test-sequence*)

;; 0R: 0LX0#0LX: 0R  22022022022022022
(circuit-eval '((:R 0)
                (:R 0)
                ((((:L 0) (:X)) ((:L 0) (:X)))))
              *test-sequence*)

#|
;;;
;;; circuit-eval-functions? delete this later.
;;;
;;; input-ternary-stream -> circuit -> [factory] -> fuel-ternary-stream.
;;;

;;; GATE

;; \ /
;;  *
;; / \

;; 3^6 or 18! ???

(defun left-gate-function (left-in rigth-in)
  (case left-in
    (0 (case rigth-in
         (0 ?)
         (1 ?)
         (2 ?)))
    (1 (case rigth-in
         (0 ?)
         (1 ?)
         (2 ?)))
    (2 (case rigth-in
         (0 ?)
         (1 ?)
         (2 ?)))))

(defun rigth-gate-function (left-in rigth-in)
  (case left-in
    (0 (case rigth-in
         (0 ?)
         (1 ?)
         (2 ?)))
    (1 (case rigth-in
         (0 ?)
         (1 ?)
         (2 ?)))
    (2 (case rigth-in
         (0 ?)
         (1 ?)
         (2 ?)))))

;;; puzzzle:

(defun puzzzle-function (left-in rigth-in)
  (case left-in
    (0 (case rigth-in
         (0 1)
         (1 0)
         (2 0)))
    (1 (case rigth-in
         (0 1)
         (1 0)
         (2 2)))
    (2 (case rigth-in
         (0 2)
         (1 2)
         (2 1)))))
|#
