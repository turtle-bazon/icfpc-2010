;;;
;;; car-fuels.lisp
;;;

(in-package :icfpc)

;; Вычисления на секциях труб двигателя.

;; out(k) = c(1,k) * in(1) + .. + c(n,k) * in(n)
;; c(i,k) >= 0
