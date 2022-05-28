; (load (compile-file "ladders.lisp"))

(defun down-ladder (l)
  (cond ((null (cdr l)) (car l))
	(t (expt (car l) (up-ladder (cdr l))))))

(defun up-ladder (l)
  (cond ((null (cdr l)) (car l))
	(t (expt  (down-ladder (cdr l)) (car l)))))

(defun ladder-search (&key (len 4) (lim 5))
  (loop for l in (all-ladders len lim)
	as up = (up-ladder l)
	as down = (down-ladder l)
	when (= up down)
	do (print l)))

(defun all-ladders (len lim)
  (mapcan #'identity (all-ladders2 len lim)))

(defun all-ladders2 (len lim)
  (cond ((= len 0) (list (list nil)))
	(t (loop for i from 2 to lim
		 collect (insert-all i (all-ladders2 (1- len) lim))))))

(defun insert-all (what in)
  (loop for elt in in
	append (loop for subelt in elt
		     collect (cons what subelt))))
  
(ladder-search :len 3 :lim 7)