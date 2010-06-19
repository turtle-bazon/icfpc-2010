;;;
;;; package.lisp -- common packege for our ICFPC project.
;;;

(defpackage :icfpc
  (:use :cl :rutils :cl-ppcre :drakma)
  (:export :get-car-ids :get-car-code :post-fuel))
