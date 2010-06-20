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
;;; unparse
;;;

(unparse-circuit
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

;; bug:

(unparse-circuit (parse-circuit "0L: X0L0#0RX: 0R"))

"0L:
X0L0#0RX, <<
:0R"

;;;
;;; evalutor
;;;

;; input - 01202101210201202
(defparameter *test-sequence* '(0 1 2 0 2 1 0 1 2 1 0 2 0 1 2 0 2))

;; TODO: F -> F circuits:

(dolist (circuit '("X::X"
                   "X:0L0R0#0L0R:X"))
                   ;; more?
  (format t "~A == ~A~%"
            circuit
            (circuit-eval (parse-circuit circuit) *test-sequence*)))

;; ^ error and error

;; Basic circuits:

(dolist (circuit '("0L: X0L0#0RX: 0R"
                   "0R: 0LX0#0LX: 0R"
                   "0L: X0R0#X0R: 0L"
                   "0R: 0RX0#X0L: 0L"))
  (format t "~A == ~A~%"
            circuit
            (circuit-eval (parse-circuit circuit) *test-sequence*)))

;; 22120221022022120 - ok
;; 22022022022022022 - ok
;; error! {0}, not 02120112100002120
;; error! {0}, not 01210221200001210

;; yet another {0}...
(circuit-eval (parse-circuit "0L:X0R0#X0R:0L") *test-sequence*)
;; so, error!

;; complex circuits:

(circuit-eval (parse-circuit "0R: 1LX0#1RX, 1R0L0#0L1L: 0R") *test-sequence*)
;; 22222122122121222 - ok

;; circuit from Task

;; 10221220002011011

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

(circuit-eval *task-circuit* *task-sequence*)

;; why {0} ? error!

;;;  ,-------------------------------------------------------------------------.
;;;  |  GET and POST from server                                               |
;;;  `-------------------------------------------------------------------------^

(defparameter *car-ids* (nreverse (get-car-ids)))

(219 2416 2638 2627 2625 2655 2776 2770 2748 3032 2778 2644 2878 2658 2646 2884
 2641 2782 2772 2754 2767 2745 2759 2634 2765 2752 2801 2761 2648 2652 2650
 2785 2795 2886 2992 2888 3211 3039 2881 3986 3028 4454 3049 3044 4528 4522
 4619 2780 4791 4507 4552 10103 11893 11045 9480 9559 3778 4731 11083 10094
 9577 11887 10404 9258 10086 11866 10431 12030 15168 15077 15188 20280 20282
 20430 20838 20824 20902 20833 20819 20565 2498 20830 21168 10134 10486 10220
 10544 15253 10317 22872 23245 10463 3227 9987 2864 3529 3283 2869 2386 3815
 3726 3734 3630 3615 3606 3808 2912 11860 3658 21574 19145 10656 21832 3670
 3519 20104 3626 3470 3661 21605 21603 20193 1726 21619 15917 2750 25842 2728
 2721 4007 9748 5293 27032 4014 4009 19153 15164 3941 3925 5310 5231 3932 3969
 3975 4432 4438 4196 4202 5145 5138 4951 5157 5224 4190 19991 11156 4000 4040
 5162 4044 5151 1728 2774 10060 13252 20926 20069 12840 25190 11810 12816 12776
 20841 12785 12116 12072 3937 12034 11756 2726 12830 12797 12793 24172 2689
 26189 26143 26134 26311 26120 25677 13232 13010 13108 12850 13204 13129 13046
 13035 13016 13003 12881 10255 3465 3715 2735 2591 1187 12844 2731 11964 10047
 11772 27670 3666 2756 3820 3610 3811 3460 3720 12053 2717 12129 2733 22100
 2719 2713 2875 3270 26098 2738 2715 2711 20383 2763 2685 4220 26125 297 26154
 26890 27296 26967 26914 2723 27278 27021 27223 26499 24100 4206 26615 5130
 3927 26132 26741 4374 4248 26957 4181 4125 25849 26123 4002 26130 4200 4106
 3995 4057 4017 3989 4157 26919 3939 26933 26069 26105 26110 26116 3930 3916
 3865 3861 3856 3842 3831 26226 26683 3817 26871 3786 26186 3770 3746 3729 3705
 3680 3762 26980 26555 28643 3450 28766 26882 3562 3467 3268 26940 26946 3633
 26845 3608 26862 26875 26903 26886 26943 3498 3385 3336 26976 26978 26896
 26974 26986 27003 3589 3533 26910 26916 26927 26931 26923 26949 27001 26951
 26960 26972 26954 26965 26969 26993 27006 26998 27009 27013 27015 27019 27023
 27026 27028 27030 27131 29415 27213 27280 27353 2708 28464 27034 27509 27774
 28293 28838 27036 27039 27134 28381 29551 27235 4979 25192 4991 4964 25388
 25395 25409 4943 25416 4970 25422 4938 25711 25731 5109 5076 5011 4953 4934
 4920 25726 4893 25734 4854 4847 4840 4827 4824 4819 4816 25736 25743 25839
 25775 26103 4484 26127 25808 4487 4314 4051 4046 26291 26879 4412 4282 4074
 4036 4029 4141)

(defvar *car-codes* (mapcar #'get-car-code *car-ids*))

;;;  ,-------------------------------------------------------------------------.
;;;  |  TRY to GENERATE FUELs / CIRCUITs / CARs                                |
;;;  `-------------------------------------------------------------------------^

???

pUt cOdE HeRe

!!!

Нужно генерировать валидные схемы, вычислять, отправлять на сервер - тогда в конце концов можно
будет получить ключевую фабрику (ну и топливо). Т.е. *key-fuel* и prefix.

;; ключевая фабрика

(defvar *key-fuel*
"0R:
1LX0#2L1L,
0R3L0#0L2R,
0L1R0#X3R,
5L2R0#1R4L,
3R4R0#6R4R,
7R7L0#3L7L,
6R4L0#7R6L,
5R6L0#5R5L:
2L")

;; префикс

prefix >> 11021210112101221

Это простейшее топливо можно будет разослать существующим машинам.

;; отправим ключ на все существующие машины

(dolist (car *car-ids*)
  (format t "post key-fuel for ~A...~%" car)
  (format t "  ~A~%" (post-fuel car *key-fuel*)))
>>
circuit output starts with 11021210112101221 this is a legal prefix you
have produced fuel for 0 tanks using 0 ingredients of air dimension mismatch

Но - это ни к чему не приведёт.

После того как у нас будет пример топлива можно будет начать генерировать троичные коды для
машин и получать сообщения парсера для них - также как получали для схем. И постить машины
с простейшим топливом.

;; начнём перебирать все машины, мы можем видеть - валидные они, существуют ли они,
;; или их ещё не запостили, и наконец - сочетаются ли они с топливом.

(dolist (random-car (all-ternary 10)) ;; is so slooowe
  (format t "post car ~A...~%" random-car)
  (format t "  ~A~%" (post-car random-car *key-fuel*)))

Но - простейшее топливо не подходит для "рабочих" машин.

Уже этот бутфорс очень муторный, перебор машин/топлив будет просто нереален.

Зато там уже будет известно направление поиска, т.е. уже не совсем брутфорс // allchemist

Более сложное топливо получается путем prefix + dyn_part, где dyn_part вычисляется исходя
из конструкции машины (динамики топлива в трубках).

Альтернатива (перебору и генерации) - чёткое моделирование всех элементов. Она же позволит
создавать специальные топлива и машины.

Ещё - параметры для post-car имеют названия problem и solution, т.е. это намекает на то,
что тут нужно использовать какой-либо problem-solver (GPS, SAT, CNF ?).
