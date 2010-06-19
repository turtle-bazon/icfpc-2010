;;;
;;; circuit-eval.lisp -- factory interpretater.
;;;
;;;                      input-ternary-stream -> circuit -> [factory] -> fuel-ternary-stream.
;;;

(in-package :icfpc)

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
