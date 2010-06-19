
(defun strings (&rest strings)
  (apply #'concatenate 'string strings))

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
  (make-array (list size 4)))

(defun possible-inputs (size)
  (let ((lst nil))
    (dotimes (i size)
      (push i lst))
    (nreverse lst)))

(defun fill-inputs (size)
  (let ((possible-input (possible-inputs size))
	(factories nil))
    (dotimes (i0 size)
      (dotimes (i1 size)
	(let ((fact (make-factory size)))
	  (dotimes (i size)
	    (setf (aref fact i 0) (elt possible-input i0))
	    (setf (aref fact i 1) (elt possible-input i1)))
	  (push fact factories))))
    (nreverse factories)))
