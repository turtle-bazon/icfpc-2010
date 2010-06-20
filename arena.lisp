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

(219 2416 2638 2625 2627 2770 2776 2748 2658 2644 2884 2655 2878 2641 2992 2759
 2772 2652 3032 2646 2648 2634 2754 2761 2801 2795 2782 2778 2767 2650 2785
 2745 3986 3039 2888 4507 2765 2886 2881 2752 3028 3049 4791 3211 2780 3044
 4619 4528 4522 4454 9559 10103 11887 3778 4552 9577 11045 9480 11893 10431
 4731 9258 11083 15168 10086 10094 10404 11866 15188 15077 12030 20430 20282
 20280 20824 20838 20819 20902 21168 20833 20830 20565 22872 23245 10486 2498
 10134 10544 10220 15253 27032 27670 21574 10317 21832 37603 37630 37527 37583
 21603 21605 25842 3227 2864 3529 3283 2869 21619 3815 3808 3606 3734 3726 3630
 9987 3615 2912 10463 3658 20104 40008 20193 2386 19145 3519 11860 3670 3470
 3661 3626 42594 42665 15917 42330 42521 41683 10656 42514 2750 43452 1726
 44460 37511 39760 45304 25677 4007 26120 2728 26143 26311 26134 26189 3925
 2721 40868 9748 15164 41447 5231 5157 3975 3941 4202 4196 3932 19153 11156
 4014 44195 5162 32874 19991 5310 5224 5293 10060 5138 5145 4438 4190 4044 4432
 33814 4009 4951 3969 2774 5151 33741 4040 36859 12785 13252 12034 20069 12793
 12797 4000 25190 33030 12816 12776 35939 37260 37216 35797 36448 33434 30852
 20926 13232 13010 13108 12850 20841 13204 13046 13035 13016 13003 12881 12844
 12840 12830 12116 12072 11810 11756 1728 13129 35966 12053 3937 11964 34260
 32970 11772 34232 2735 12129 24172 3715 2689 34285 2726 38844 44907 37170 3465
 34383 34540 36416 2731 3460 35762 3666 2756 37225 3610 3820 10255 2591 35835
 2719 3720 36425 36891 3811 37407 2713 33704 1187 2875 3270 39028 26098 10047
 2717 38475 39577 36499 35850 2738 2711 44011 32839 20383 27296 36075 31427
 29857 37854 2685 38606 27353 39740 39139 37942 38717 37515 32599 38238 39124
 39183 39292 39788 27223 38578 39249 39426 27021 39704 37446 26154 2733 37354
 26890 26914 35513 35221 35410 37534 37551 35042 35542 34302 37183 27278 32031
 30116 29551 36934 39074 34391 30940 30748 27774 26499 37503 22100 32267 38430
 38793 38819 39103 39633 39670 26967 36148 28381 2763 34692 2723 2715 35447
 36432 28766 26125 41692 27509 297 4220 43542 45672 38850 43309 40974 25192
 25388 25416 25734 25775 25808 26879 25849 45681 45677 26105 26116 26132 45689
 26741 26615 5130 43603 26555 26882 4816 26940 26875 26903 26943 26976 26896
 26974 43754 26986 4181 4125 27003 26910 39217 26927 39144 26931 40970 39822
 4206 26949 27001 26960 26972 26954 3927 26969 26993 27006 26998 27009 27013
 27015 39806 39849 27019 27023 27026 43909 27028 27034 3608 27039 45711 45970
 24100 3831 25726 3705 3680 42170 25731 40708 25711 3842 3856 3861 3865 3916
 3762 3930 43987 25422 25409 26965 41427 40978 27030 27131 26951 3939 4157
 27213 27280 26923 43621 3450 44332 3989 4017 4057 28464 3562 3467 3268 40033
 41838 3995 41378 4106 4200 26916 39892 40540 41843 4002 3633 25395 45697 4248
 4374 40976 4029 28293 28838 4036 3498 4074 3385 3336 4141 27036 45720 27134
 4282 4412 26978 4046 4051 3589 3533 4314 4487 41865 42321 43650 26886 27235
 4484 26862 26845 26946 4819 28643 4824 29415 4827 4840 4847 4854 4893 42683
 26980 4920 4934 4953 5011 5076 5109 44465 4938 4970 26186 45448 4943 26871
 46047 4964 4991 26683 26226 26110 45757 4979 2708 26069 26933 26919 26130
 26123 26957 26291 26127 26103 25839 41688 40093 25743 42466 43266 45076 3746
 3729 3770 25736 39830 3786 3817)

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

(dolist (random-car (all-ternary 10))
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

;;; Доп. как сервер принимает машины:

(post-car car-code fuel-circuit)

1) парсит car-code (тернарный код)
2) если нет ошибок проверяет - не существует ли такая машина уже,
3) если нет, то парсит fuel-circuit также как и при post-fuel (как строку-фабрику)
4) если всё ок запускает динамику машина + топливо
5) если динамика нехорошая - Ja или Nein
6) Иначе парочка субмититься

;;;
;;; Преобразования *key-fuel* к другим топливам путём добавления гейтов с петлями:
;;;

(let ((s-key-fuel (parse-circuit *key-fuel*)))
  (dolist (si '(:r :l))
    (dolist (sj '(:r :l))
      (format t "(~A :X) (~A :X)~%  ~A~%" si sj
              (post-fuel "219" (unparse-circuit (add-loop s-key-fuel `(,si x) `(,sj x))))))))

(let ((s-key-fuel (parse-circuit *key-fuel*)))
  (dotimes (i 8)
    (dotimes (j 8)
      (dolist (si '(:r :l))
        (dolist (sj '(:r :l))
          (format t "(~A ~A) (~A ~A)~%  ~A~%" si i sj j
                  (post-fuel "219" (unparse-circuit (add-loop s-key-fuel `(,si ,i) `(,sj ,j))))))))))

(unparse-circuit (add-loop (parse-circuit *key-fuel*) (:l 1) (:l 1)))
