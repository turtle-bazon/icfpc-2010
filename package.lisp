;;;
;;; package.lisp -- common package for our ICFPC project.
;;;

(defpackage :icfpc
  (:use :cl :split-sequence :cl-ppcre :drakma)
  (:export :get-car-ids :get-car-code :post-fuel))
