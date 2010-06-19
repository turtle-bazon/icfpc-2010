;;;
;;; package.lisp -- common package for our ICFPC project.
;;;

(defpackage :icfpc
  (:use :cl :cl-ppcre :drakma)
  (:export :get-car-ids :get-car-code :post-fuel))
