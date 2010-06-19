
(asdf:defsystem :icfpc
  :depends-on (:rutils :cl-ppcre :drakma )
  :components (;; tools
               (:file "post")
               ;; models
               (:file "car")
               (:file "gen-fuel")
               (:file "circuit-parser")))
