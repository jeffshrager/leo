; (load (compile-file "seqs.lisp"))

(defvar r 0)

(defun run (&key (to 100000) (+- +1) (up #'(lambda (n) (if (oddp n) -1.0 1.0))) (down (lambda (n) 
											(+ 1
											   (* 3 n)))))
  (loop for n below to
	with sum = 0
	do (setq r (incf sum (/ (funcall up n) (funcall down n)))))
  r)

(defun fact (n)
  (if (zerop n) 1
    (* n (fact (1- n)))))

(print (* 4 (run)))
