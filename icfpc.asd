
(asdf:defsystem :icfpc
  :depends-on (:cl-ppcre :rutils :drakma)
  :components ((:file "package")
               ;; tools
               (:file "get-and-post")
               ;; models
               (:file "car")
               (:file "gen-fuel")
               (:file "circuit-parser")
               (:file "circuit-eval")))
