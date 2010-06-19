;;;
;;; get-and-post.lisp -- conversation with their server.
;;;

(in-package :icfpc)

(defvar *user*       "Skobochka")
(defvar *pass*       "158874878014849802423980469050775541970745292308997461923937")
(defvar *address*    "http://icfpcontest.org/icfp10/static/j_spring_security_check")
(defvar *parameters* `(("j_username" . ,*user*) ("j_password" . ,*pass*)))

(defmacro with-cookie ((var address parameters) &body body)
 `(let ((,var (make-instance 'cookie-jar)))
    (http-request ,address
                  :method :post
                  :parameters ,parameters
                  :cookie-jar ,var)
    ,@body))

(defun post-fuel (car-id factory-string)
  (with-cookie (cookie *address* *parameters*)
    (nth-value
     1
     (scan-to-strings
      "<pre>(.*)</pre>"
      (substitute #\Space #\Newline
                  (http-request
                   (format nil "http://icfpcontest.org/icfp10/instance/~a/solve" car-id)
                   :method :post
                   :parameters (list (cons "contents" factory-string))
                   :cookie-jar cookie))))))

(defun get-car-ids ()
  (with-cookie (cookie *address* *parameters*)
    (mapcar #'(lambda (e)
                (parse-integer (scan-to-strings "(\\d+)" e :start 22)))
            (all-matches-as-strings
             "<td style=\"width: 20%;\">(\\d+)</td>"
             (http-request "http://icfpcontest.org/icfp10/score/instanceTeamCount"
                           :cookie-jar cookie)))))

(defun get-car-code (car-id)
  (with-cookie (cookie *address* *parameters*)
    (parse-integer
     (aref
      (nth-value
       1
       (scan-to-strings
        "Car:</label>(\\d+)</div>"
        (http-request (format nil "http://icfpcontest.org/icfp10/instance/~A/solve/form?x=8&y=6" car-id)
                      :cookie-jar cookie)))
      0))))

(defun format-cars (car-codes car-ids file &optional (line-length 66))
  (with-open-file (s file
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (dotimes (i (length car-codes))
      (let* ((car (write-to-string (elt car-codes i)))
	     (length (length car)))
	(format s "~%~A~%" (elt car-ids i))
	(dotimes (j (floor length line-length))
	  (format s "~A~%" (subseq car (* line-length j) (* line-length (1+ j)))))
	(format s "~A~%" (subseq car (* line-length (floor length line-length))))))))
