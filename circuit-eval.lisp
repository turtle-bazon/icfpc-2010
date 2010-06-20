;;;
;;; circuit-eval.lisp -- eval circuits.
;;;

(in-package :icfpc)

(defvar *circuit*)

;; node structure

(defstruct node
  (out-r 0)
  (out-l (list 0)))

(defun make-nodeset (n)
  (let ((rez (make-array n)))
    (dotimes (i n)
      (setf (aref rez i) (make-node)))
    rez))

(defvar *nodes*)  ; (make-nodeset 20))

(defun node (i)
  (aref *nodes* i))

(defparameter *node-func*
                       ;   L   R  L  R
  (hash-table-from-list '((0 . 0) (0 2)
                          (0 . 1) (2 2)
                          (0 . 2) (1 2)
                          (1 . 0) (1 2)
                          (1 . 1) (0 0)
                          (1 . 2) (2 1)
                          (2 . 0) (2 2)
                          (2 . 1) (1 1)
                          (2 . 2) (0 0))
                        #'equal))

(defun node-func (l r)
  (apply #'values (gethash (cons l r) *node-func*)))

(defun nodecall (i)
  (multiple-value-bind (o-l o-r)
      (node-func (out-val (in-l i))
                 (out-val (in-r i)))
    (let ((node (node i)))
      (push o-l (node-out-l node))
      (setf (node-out-r node) o-r
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
            (let ((node (node i)))
              (prog1 (if force
                         (cadr (node-out-l node))
                         (car  (node-out-l node)))
                (setf (node-out-l node) (cdr (node-out-l node))
                      (aref *nodes* i) node)))
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
             (calc-node (car remaining-nodes))))))
      (print *nodes*) (terpri)
    (nreverse rez)))
