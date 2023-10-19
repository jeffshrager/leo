; (load (compile-file "facts.lisp"))

(defun fact (n)
  (if (= 0 n) 1 (* n (fact (1- n)))))

(defun in-fact (n)
  (search-all (format nil "~a" n) 
	      (format nil "~a" (fact n))))

(defun in-fact-range (low high)
  (loop for n from low to high
	as all = (in-fact n)
	when (< 1 (length all))
	do (print (list n all))
	collect (cons n (length all))))

(defun search-all (s1 s2)
  (loop for p = (search s1 s2 :start2 (if p (1+ p) 0))
	until (null p)
	collect p))

(defvar *r* nil)

(defun run ()
  (loop for i in
	(sort (setq *r* (in-fact-range 2 10000))
	      #'>
	      :key #'cdr)
	until (zerop (cdr i))
	do (print i)))

(defun s! (n)
  (if (> n 32768)
      (error "Too large!")
  (let* ((r (format nil "~a" (fact n)))
	 (lr (length r)))
    (loop for p from 0 to lr by 100
	  do (format t "~a~%" (ignore-errors (subseq r p (+ 100 p))))))))

(s! 10000)

(defun ap2 (&optional (low 1) (high 200))
  (loop as p from low to high 
	as p2 = (expt 2 p)
	as p2s = (format nil "~a" p2)
	as 666pos = (search "666" p2s)
	when 666pos
	do (print (list p p2 666pos))))
