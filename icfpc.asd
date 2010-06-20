;;;
;;; package.lisp -- ASDF definition for our ICFPC project.
;;;

(asdf:defsystem :icfpc
  :depends-on (:split-sequence :cl-ppcre :drakma)
  :components ((:file "package")

               ;; utils & tools

               (:file "utils")
               (:file "get-and-post")

               ;; models

               (:file "car")
               (:file "car-proper")
               (:file "car-fuels")

               (:file "car-ternary-streams")
               (:file "fuel-ternary-streams")

               (:file "circuit-parser")
               (:file "circuit-eval")
               (:file "circuit-eval2")

               (:file "mod-circuit")

               ;; brute-forces, generators

               (:file "gen-ternary")
               (:file "gen-circuit1")
               (:file "gen-circuit2")
               (:file "gen-circuit3")

               ;; graphvis generator

               (:file "graphviz-gen")

               ))
