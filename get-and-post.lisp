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

(defun format-cars (cars file)
  (with-open-file (s file
		     :if-exists :error
		     :if-does-not-exist :create)
    (map nil #'(lambda (car) (format s "~A~%" car)) cars)))

;;; usage:

#|

(defvar *car-ids* (get-car-ids))

(10317 10255 10220 10134 10103 10094 10086 10060 10047 9987 9748 9577 9559 9480
 9258 5310 5293 5231 5224 5162 5157 5151 5145 5138 5130 5109 5076 5011 4991
 4979 4970 4964 4953 4951 4943 4938 4934 4920 4893 4854 4847 4840 4827 4824
 4819 4816 4791 4731 4619 4552 4528 4522 4507 4487 4484 4454 4438 4432 4412
 4374 4314 4282 4248 4220 4206 4202 4200 4196 4190 4181 4157 4141 4125 4106
 4074 4057 4051 4046 4044 4040 4036 4029 4017 4014 4009 4007 4002 4000 3995
 3989 3986 3975 3969 3941 3939 3937 3932 3930 3927 3925 3916 3865 3861 3856
 3842 3831 3820 3817 3815 3811 3808 3786 3778 3770 3762 3746 3734 3729 3726
 3720 3715 3705 3680 3670 3666 3661 3658 3633 3630 3626 3615 3610 3608 3606
 3589 3562 3533 3529 3519 3498 3470 3467 3465 3460 3450 3385 3336 3283 3270
 3268 3227 3211 3049 3044 3039 3032 3028 2992 2912 2888 2886 2884 2881 2878
 2875 2869 2864 2801 2795 2785 2782 2780 2778 2776 2774 2772 2770 2767 2765
 2763 2761 2759 2756 2754 2752 2750 2748 2745 2738 2735 2733 2731 2728 2726
 2723 2721 2719 2717 2715 2713 2711 2708 2689 2685 2658 2655 2652 2650 2648
 2646 2644 2641 2638 2634 2627 2625 2591 2498 2416 2386 1728 1726 1187 297 219)

(defvar *car-codes* (mapcar #'get-car-code *car-ids*))

TODO! need time.

(post-fuel "219" "0L:\n0LX0\#0L0R:\n0L")

|#