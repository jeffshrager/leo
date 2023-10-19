; (load (compile-file "p2digs.lisp"))

(defun p2digs (n)
  (let* ((s (format nil "~a" (expt 2 n)))
	 (l (length s)))
    (subseq s (- l 3))))
    
(defun run ()
  (loop for n from 10 below 10000
	as r = (p2digs n)
	if (string-equal "888" r)
	do (print (list n r))))

(run)
