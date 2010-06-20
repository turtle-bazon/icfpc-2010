;;;
;;; gen-ternary.lisp -- just generate a {0,1,2}* sequences.
;;;

(in-package :icfpc)

(defun ternary (n)
  (if (= n 1)
     '("0" "1" "2")
      (let ((result nil))
        (dolist (a (ternary (1- n)))
          (dolist (b '("0" "1" "2"))
            (push (strings a b) result)))
        result)))

(defun all-ternary (n)
  (flatten (mapcar #'ternary (mapcar #'1+ (range n)))))
