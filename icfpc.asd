
(asdf:defsystem :icfpc
  :depends-on (:drakma :cl-ppcre)
  :components (;; tools
               (:file "post")
               ;; models
               (:file "car")
	       (:file "gen-fuel")))
