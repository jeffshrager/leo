; (load (compile-file "seqs.lisp"))

(defun seqn (rr &key (n 1000) (tracen nil))
  (loop for n from 1 to n
	with sum = 0
	as v = (funcall rr n)
	do 
	(incf sum v)
	(when (and tracen (zerop (mod n tracen)))
	  (print (list n v sum)))
	finally (return sum)))

(defun run ()
  (loop for m from 1.0 to 10.0
	do
	(format t "~%--> ~a <--~%" m)
	(ignore-errors
	 (seqn (lambda (n) 
		 (/ (expt -1 (1+ n))
		    (- (* m n) 1)))
	       :tracen 100 :n 1000))))
(run)
