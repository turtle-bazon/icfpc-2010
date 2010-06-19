;;;
;;; arena.lisp -- use icfpc package.
;;;

(in-package :icfpc)

;;;  ,-------------------------------------------------------------------------.
;;;  |  CARS                                                                   |
;;;  `-------------------------------------------------------------------------^

  CAR with one CHAMBER.

      upper pipe:  -> section -> section ------------.
    /                   /          /                  \
   /         fuel 0  --<----------'--------.      difference      positive
 air                    \                   \       engine  --->  energy
   \                     \                   \        /
    \ lower pipe:  -> section -> section -> section -'
                                    /
             fuel 1 ---------------'

(make-car-engine           ;; one chamber
 '(((fuel0 fuel0)          ;; <<-- first  list of fuel-tank names.
    (fuel0 fuel1 fuel0)))) ;; <<-- second list of fuel-tank names.

;; =>

:CAR-ENGINE 

:CHAMBER 

:PIPE 
<:section to fuel-tank FUEL0> 
<:section to fuel-tank FUEL0>  

:PIPE 
<:section to fuel-tank FUEL0> 
<:section to fuel-tank FUEL1> 
<:section to fuel-tank FUEL0>

(s-car-engine
 (make-car-engine
  '(((fuel0 fuel0)
     (fuel0 fuel1 fuel0)))))

;; =>

(:CAR-ENGINE
 ((:UPPER-PIPE :PIPE (:SECTION FUEL0 0) (:SECTION FUEL0 0))
  (:LOWER-PIPE :PIPE (:SECTION FUEL0 0) (:SECTION FUEL1 0) (:SECTION FUEL0 0))))

;;;  ,-------------------------------------------------------------------------.
;;;  |  CIRCUITS                                                               |
;;;  `-------------------------------------------------------------------------^

;;;
;;; parser
;;;

(parse-circuit
"19L:
12R13R0#1R12R,
14R0L0#4R9L,
9R10R0#3L8L,
2L17R0#5L9R,
15R1L0#10R13R,
3L18R0#6L15L,
5L11R0#13L12L,
19R16R0#11R8R,
2R7R0#11L10L,
1R3R0#18L2L,
8R4L0#16L2R,
8L7L0#15R6R,
6R0R0#14L0L,
6L4R0#14R0R,
12L13L0#17L1L,
5R11L0#16R4L,
10L15L0#17R7R,
14L16L0#18R3R,
9L17L0#19R5R,
X18L0#X7L:
19L"
)

;; =>

((:L 19) (:L 19)
 ((((:R 12) (:R 13)) ((:R 1) (:R 12))) (((:R 14) (:L 0)) ((:R 4) (:L 9)))
  (((:R 9) (:R 10)) ((:L 3) (:L 8))) (((:L 2) (:R 17)) ((:L 5) (:R 9)))
  (((:R 15) (:L 1)) ((:R 10) (:R 13))) (((:L 3) (:R 18)) ((:L 6) (:L 15)))
  (((:L 5) (:R 11)) ((:L 13) (:L 12))) (((:R 19) (:R 16)) ((:R 11) (:R 8)))
  (((:R 2) (:R 7)) ((:L 11) (:L 10))) (((:R 1) (:R 3)) ((:L 18) (:L 2)))
  (((:R 8) (:L 4)) ((:L 16) (:R 2))) (((:L 8) (:L 7)) ((:R 15) (:R 6)))
  (((:R 6) (:R 0)) ((:L 14) (:L 0))) (((:L 6) (:R 4)) ((:R 14) (:R 0)))
  (((:L 12) (:L 13)) ((:L 17) (:L 1))) (((:R 5) (:L 11)) ((:R 16) (:L 4)))
  (((:L 10) (:L 15)) ((:R 17) (:R 7))) (((:L 14) (:L 16)) ((:R 18) (:R 3)))
  (((:L 9) (:L 17)) ((:R 19) (:R 5))) (((:X) (:L 18)) ((:X) (:L 7)))))

(make-node-array
  (parse-circuit
"19L:
12R13R0#1R12R,
14R0L0#4R9L,
9R10R0#3L8L,
2L17R0#5L9R,
15R1L0#10R13R,
3L18R0#6L15L,
5L11R0#13L12L,
19R16R0#11R8R,
2R7R0#11L10L,
1R3R0#18L2L,
8R4L0#16L2R,
8L7L0#15R6R,
6R0R0#14L0L,
6L4R0#14R0R,
12L13L0#17L1L,
5R11L0#16R4L,
10L15L0#17R7R,
14L16L0#18R3R,
9L17L0#19R5R,
X18L0#X7L:
19L"
))

;; =>

#2A(((:R 12) (:R 13) (:R 1) (:R 12))
    ((:R 14) (:L 0) (:R 4) (:L 9))
    ((:R 9) (:R 10) (:L 3) (:L 8))
    ((:L 2) (:R 17) (:L 5) (:R 9))
    ((:R 15) (:L 1) (:R 10) (:R 13))
    ((:L 3) (:R 18) (:L 6) (:L 15))
    ((:L 5) (:R 11) (:L 13) (:L 12))
    ((:R 19) (:R 16) (:R 11) (:R 8))
    ((:R 2) (:R 7) (:L 11) (:L 10))
    ((:R 1) (:R 3) (:L 18) (:L 2))
    ((:R 8) (:L 4) (:L 16) (:R 2))
    ((:L 8) (:L 7) (:R 15) (:R 6))
    ((:R 6) (:R 0) (:L 14) (:L 0))
    ((:L 6) (:R 4) (:R 14) (:R 0))
    ((:L 12) (:L 13) (:L 17) (:L 1))
    ((:R 5) (:L 11) (:R 16) (:L 4))
    ((:L 10) (:L 15) (:R 17) (:R 7))
    ((:L 14) (:L 16) (:R 18) (:R 3))
    ((:L 9) (:L 17) (:R 19) (:R 5))
    ((:X) (:L 18) (:X) (:L 7)))

;;;
;;; evalutor
;;;

;; input - 01202101210201202
(defparameter *test-sequence* '(0 1 2 0 2 1 0 1 2 1 0 2 0 1 2 0 2))

;; TODO: X -> X circuits:

(circuit-eval (parse-circuit "X::X") *test-sequence*)
;; 01202101210201202 ?

(circuit-eval (parse-circuit "X:0L0R0#0L0R:X") *test-sequence*)
;; 01202101210201202 ?

;; Basic circuits:

(dolist (circuit '("0L: X0L0#0RX: 0R"
                   "0R: 0LX0#0LX: 0R"
                   "0L: X0R0#X0R: 0L"
                   "0R: 0RX0#X0L: 0L"))
  (format t "~A == ~A~%"
            circuit
            (circuit-eval (parse-circuit circuit) *test-sequence*)))

(circuit-eval (parse-circuit "0L: X0L0#0RX: 0R") *test-sequence*)
;; 22120221022022120 - ok

(circuit-eval (parse-circuit "0R: 0LX0#0LX: 0R") *test-sequence*)
;; 22022022022022022 - ok

;; why {0} ? this is a basic circuits!

(circuit-eval (parse-circuit "0L: X0R0#X0R: 0L") *test-sequence*)
;; 02120112100002120 ?

(circuit-eval (parse-circuit "0R: 0RX0#X0L: 0L") *test-sequence*)
;; 01210221200001210 ?

;; yet another {0}...

(circuit-eval (parse-circuit "0L:X0R0#X0R:0L") *test-sequence*)

;; complex circuits:

(circuit-eval (parse-circuit "0R: 1LX0#1RX, 1R0L0#0L1L: 0R") *test-sequence*)
;; 22222122122121222 ?



;; circuit from Task

(defvar *task-circuit*
  (parse-circuit
"19L:
12R13R0#1R12R,
14R0L0#4R9L,
9R10R0#3L8L,
2L17R0#5L9R,
15R1L0#10R13R,
3L18R0#6L15L,
5L11R0#13L12L,
19R16R0#11R8R,
2R7R0#11L10L,
1R3R0#18L2L,
8R4L0#16L2R,
8L7L0#15R6R,
6R0R0#14L0L,
6L4R0#14R0R,
12L13L0#17L1L,
5R11L0#16R4L,
10L15L0#17R7R,
14L16L0#18R3R,
9L17L0#19R5R,
X18L0#X7L:
19L"
))

(defvar *task-sequence* '(0 2 2 2 2 2 2 0 2 1 0 1 1 0 0 1 1))

(circuit-eval *task-circuit* *test-sequence*)

;; why {0} ?

;;;  ,-------------------------------------------------------------------------.
;;;  |  GENERATE circuits                                                      |
;;;  `-------------------------------------------------------------------------^

???

pUt cOdE HeRe

!!!

We can generate, generate, computate, send to the server and get the key-factory.

;;;  ,-------------------------------------------------------------------------.
;;;  |  GET and POST from server                                               |
;;;  `-------------------------------------------------------------------------^

(defparameter *car-ids* (nreverse (get-car-ids)))

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

(post-fuel "219" "0L:\n0LX0\#0L0R:\n0L")
