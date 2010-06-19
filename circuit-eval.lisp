;;;
;;; circuit-eval.lisp -- factory interpretater.
;;;
;;;                      input-ternary-stream -> circuit -> [factory] -> fuel-ternary-stream.
;;;

(in-package :icfpc)

;;; GATE

;;  |
;;  *
;; / \

;; ???:

(defun gate-function (left-in rigth-in)
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

;; \ /
;;  *
;; / \

