;;;
;;; circuit-eval.lisp -- eval circuits.
;;;

(in-package :cl-user)

(defvar *circuit*)

;; ((:L 19)
;;  (:L 19)
;;  ((((:R 12) (:R 13)) ((:R 1) (:R 12)))
;;   (((:R 14) (:L 0)) ((:R 4) (:L 9)))
;;   (((:R 9) (:R 10)) ((:L 3) (:L 8)))
;;   (((:L 2) (:R 17)) ((:L 5) (:R 9)))
;;   (((:R 15) (:L 1)) ((:R 10) (:R 13)))
;;   (((:L 3) (:R 18)) ((:L 6) (:L 15)))
;;   (((:L 5) (:R 11)) ((:L 13) (:L 12)))
;;   (((:R 19) (:R 16)) ((:R 11) (:R 8)))
;;   (((:R 2) (:R 7)) ((:L 11) (:L 10)))
;;   (((:R 1) (:R 3)) ((:L 18) (:L 2)))
;;   (((:R 8) (:L 4)) ((:L 16) (:R 2)))
;;   (((:L 8) (:L 7)) ((:R 15) (:R 6)))
;;   (((:R 6) (:R 0)) ((:L 14) (:L 0)))
;;   (((:L 6) (:R 4)) ((:R 14) (:R 0)))
;;   (((:L 12) (:L 13)) ((:L 17) (:L 1)))
;;   (((:R 5) (:L 11)) ((:R 16) (:L 4)))
;;   (((:L 10) (:L 15)) ((:R 17) (:R 7)))
;;   (((:L 14) (:L 16)) ((:R 18) (:R 3)))
;;   (((:L 9) (:L 17)) ((:R 19) (:R 5)))
;;   (((:X) (:L 18)) ((:X) (:L 7))))))

;; !!! note: (:X)

;; utils

(defun hash-table-from-list (lst &optional test)
  (loop
     :with ht = (make-hash-table :test (or test 'eql))
     :for (k v) :on lst :by #'cddr
     :do (setf (gethash k ht) v)
     :finally (return ht)))


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
  (defun in-l (i)
    (in :l i))
  (defun in-r (i)
    (in :r i)))

(defun out-val (outspec)
  (let ((i (cadr outspec)))
    (if i
        (if (print (eq (car outspec) :l))
            (let ((node (node i)))
              (prog1 (node-out-l node)
                (setf (node-out-l node) (node-new-l node)
                      (aref *nodes* i) node)))
            (node-out-r (node i)))
        *in*)))



(defvar *in* 0 "Current data at input")

(defun circuit-eval (circuit input)
  (let* ((*circuit* circuit)
         (n (length (elt circuit 2)))
         (*nodes* (make-nodeset n))
         (start (cadr (elt circuit 0)))
         (end (elt circuit 0))
         rez)
    (dolist (x input)
      (let ((nodes (range n)))
        (labels ((recur (i)
                   (nodecall i)
                   (setf nodes (remove i nodes))
;                   (print *nodes*) (terpri) (terpri)
                   (let ((next (next-node i)))
                     (when (and next (find next nodes))
                       (recur next)))))
          (setf *in* x)
          (recur start)
          (push (out-val end) rez)
          (loop :while nodes :do
             (recur (car nodes))))))
    rez))

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

