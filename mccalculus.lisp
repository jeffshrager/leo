; (load (compile-file "mccalculus.lisp"))

(defun int1 (n) 
  (loop for i from 1 to n 
	with sum = 0
	as add = i
	do (incf sum add)
	(print (list i add sum (+ n (* (* n n) 0.5))))))
(defun int2 (n) 
  (loop for i from 1 to n 
	with sum = 0
	as add = (* i i)
	do (incf sum add)
	(print (list i add sum (* 0.3 (expt n 3))))))
(int1 10)
(int2 10)