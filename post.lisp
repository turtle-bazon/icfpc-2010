(in-package :cl-user)

(require :drakma)
(require :cl-ppcre)

(defvar *auth-cookies* nil
  "Auth cooki-jar for DRAKMA to access site in authenticated mode")

(defvar *user* "Skobochka")
(defvar *pass* "158874878014849802423980469050775541970745292308997461923937")

(defmacro with-auth (&body body)
  `(if *auth-cookies* (progn ,@body)
       (let ((,auth-cookies (make-instance 'drakma:cookie-jar)))
         (http-request
          "http://icfpcontest.org/icfp10/static/j_spring_security_check"
          :method :post
          :parameters `(("j_username" . ,*user*)
                        ("j_password"  . ,*pass*))
          :cookie-jar ,auth-cookies)
         (setf *auth-cookies* ,auth-cookies)
         ,@body)))


(defun post-fuel (car-number factory-string)
  ;; Example usage: (with-auth (post-fuel "219" "0L:\n0LX0\#0L0R:\n0L"))
  (nth-value
   1
   (cl-ppcre:scan-to-strings
    "<pre>(.*)</pre>"
    (remove #\Newline
            (drakma:http-request
             (format nil "http://icfpcontest.org/icfp10/instance/~a/solve" car-number)
             :method :post
             :parameters (list (cons "contents" factory-string))
             :cookie-jar *auth-cookies*)))))
