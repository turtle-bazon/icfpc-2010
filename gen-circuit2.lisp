;;;
;;; gen-fule2.lisp
;;;

(in-package :icfpc)

#|
factory: 
'(((.) (.))  == external gate
  ((. .) (. .)) == gate 0
  ((. .) (. .)) == gate 1
)

external gate == X
'((A) (B)):
A == откуда инфа поступает в X
B == куда инфа уходит из X

internal gate:
'((A B) (C D))
'(A B) - два входа гейта 
A - левый вход, B - правый вход

'(C D) - два выхода гейта (левый, правый)

пример гейта:

0: '((1L X) (1R X))
гейт 0 имеет два входа:

* 1L == левый выход гейта 1, соединен с левым входом гейта 0.
т.е. гейт 1 имеет левый выход со значением 0L: '((? ?) (0L ?))

* X: правый вход гейта 0 соединен с внешним гейтом X: '((?) (0R))

теперь фабрика имеет такой вид:

?:
1LX0#1RX,
??0#0L?:
0R

гейт 0 имеет два выхода:

* 1R: левый выход гейта 0 идет на правый вход гейта 1,
т.е. правый вход гейта 1 имеет значение 0L 

* X: в правый выход гейта 0 идет на вход внешнего гейта,
т.е. вход внешнего гейта имеет значение 0R

теперь такая фабрика:

0R:
1LX0#1RX,
?0L0#0L?:
0R

остаются несвязанными левый вход гейта 1 и правый выход гейта 1.
их надо вязать:

0R:
1LX0#1RX,
1R0L0#0L1L:
0R

получаем illegal prefix, то есть архитектура кольца правильная.

теперь надо попытаться генерить много таких представлений
файл gen-fuel1.lisp пытается это делать, но там много быдлокода, и поэтому ашипки
gen-fuel2.lisp comming soon
|#

(defun make-factory (size)
  (make-array (list size 8)))

(defun permutations (l)
  (if (null l) '(())
  (mapcan #'(lambda (x)
    (mapcar #'(lambda (y) (cons x y))
      (permutations (remove x l :count 1)))) l)))

(defun list-of-bits (size)
  (append (make-list size :initial-element 0)
	  (make-list size :initial-element 1)))

(defun list-of-gates (size)
  (let ((lst nil))
    (dotimes (i size)
      (push i lst) (push i lst))
    (nreverse lst)))

(defun possible-permutations (list)
  (remove-duplicates (permutations list) :test #'equal))

(defun possible-bit-permutations (size)
  (remove-duplicates (permutations (list-of-bits size)) :test #'equal))

(defun fill-outputs (factory)
  (let ((size (array-dimension factory 0)))
    (dotimes (i size)
      (let ((i0n (aref factory i 0))
	    (i0s (aref factory i 1)))
	(if (char= i0s #\L)
	    (progn (setf (aref factory i0n 4) i)
		   (setf (aref factory i0n 5) #\L))
	    (progn (setf (aref factory i0n 6) i)
		   (setf (aref factory i0n 7) #\L))))
      (let ((i1n (aref factory i 2))
	    (i1s (aref factory i 3)))
	(if (char= i1s #\L)
	    (progn (setf (aref factory i1n 4) i)
		   (setf (aref factory i1n 5) #\R))
	    (progn (setf (aref factory i1n 6) i)
		   (setf (aref factory i1n 7) #\R))))))
  factory)

(defun test-factory (factory)
  (let ((ret t))
    (dotimes (i (array-dimension factory 0) factory)
      (if ret
	  (dotimes (j 4)
	    (when (not (characterp (aref factory i (1+ (* j 2)))))
	      (setf ret nil)
	      (return)))
	  (return nil)))))



(defun copy-array1 (array)
  (let ((copy (make-array (array-dimensions array))))
    (dotimes (i (array-dimension array 0))
      (dotimes (j (array-dimension array 1))
	(setf (aref copy i j) (aref array i j))))
    copy))

(defun possible-factories (size)
  (let ((gate-perms (list (random-elt (possible-permutations (list-of-gates size)))))
	(sym-perms (possible-bit-permutations size))
	(tmp-facts nil)
	(facts nil))
    (dotimes (i (length gate-perms))
      (let ((f (make-array (list size 8)))
	    (perm (elt gate-perms i)))
	(dotimes (gate size)
	  (setf (aref f gate 0) (pop perm))
	  (setf (aref f gate 2) (pop perm)))
	(push f tmp-facts)))
;    (dolist (fact tmp-facts)
    (let ((fact (random-elt tmp-facts)))
      (dotimes (i (length sym-perms))
	(let ((perm (elt sym-perms i))
	      (f (copy-array1 fact)))
	  (dotimes (gate size)
	    (setf (aref f gate 1) (if (= (pop perm) 0) #\R #\L))
	    (setf (aref f gate 3) (if (= (pop perm) 0) #\R #\L)))
	  (push f facts))))
    (map nil #'fill-outputs facts)
    (remove nil (mapcar #'test-factory facts))))

(defun insert-external-gate (factory in out)
  (let ((source (list #\X #\Space)) (dest (list #\X #\Space))
	(in-pos (first in)) (out-pos (first out)))
    (if (char= (second in) #\L)
	(progn (rotatef (first source)
			(aref factory in-pos 0))
	       (rotatef (second source)
			(aref factory in-pos 1)))
	(progn (rotatef (first source)
			(aref factory in-pos 2))
	       (rotatef (second source)
			(aref factory in-pos 3))))
    (if (char= (second out) #\L)
	(progn (rotatef (first dest)
			(aref factory out-pos 4))
	       (rotatef (second dest)
			(aref factory out-pos 5)))
	(progn (rotatef (first dest)
			(aref factory out-pos 6))
	       (rotatef (second dest)
			(aref factory out-pos 7))))
    (list source dest)))

(defun format-factory (fact)
  (let ((str nil))
    (dotimes (i (array-dimension fact 0))
      (push (format nil "~A~A~A~A0#~A~A~A~A~A"
		    (aref fact i 0) (aref fact i 1) (aref fact i 2) (aref fact i 3)
		    (aref fact i 4) (aref fact i 5) (aref fact i 6) (aref fact i 7)
		    (if (= i (1- (array-dimension fact 0))) #\: #\,))
	    str))
    (remove #\Space (apply #'strings (nreverse str)) :test #'char=)))

(defun format-factory-with-ext-gate (factory in out)
  (let* ((str nil)
	 (fact (copy-array factory))
	 (ext-gate (insert-external-gate fact in out)))
    (push (strings (write-to-string (first (first ext-gate))) (string (second (first ext-gate)))) str)
    (push ":" str)
    (dotimes (i (array-dimension factory 0))
      (push (format nil "~A~A~A~A0#~A~A~A~A~A"
		    (aref fact i 0) (aref fact i 1) (aref fact i 2) (aref fact i 3)
		    (aref fact i 4) (aref fact i 5) (aref fact i 6) (aref fact i 7)
		    (if (= i (1- (array-dimension factory 0))) #\: #\,))
	    str))
    (push (strings (write-to-string (first (second ext-gate))) (string (second (second ext-gate)))) str)
    (remove #\Space (apply #'strings (nreverse str)) :test #'char=)))

(defun post-factory (factory)
  (post-fuel "219" factory))
