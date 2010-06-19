;;;
;;; package.lisp -- ASDF definition for our ICFPC project.
;;;

(asdf:defsystem :icfpc
  :depends-on (:cl-ppcre :rutils :drakma)
  :components ((:file "package")
               ;; utils & tools
               (:file "utils")
               (:file "get-and-post")
               ;; models
               (:file "car")
               (:file "gen-fuel")
               (:file "circuit-parser")
               (:file "circuit-eval")))
