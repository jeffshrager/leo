;; (load (compile-file "numbersentences.lisp"))

(defun all-combinations (len lim)
  (mapcan #'identity (all-combos2 len lim)))

(defun all-combos2 (len lim)
  (cond ((= len 0) (list (list nil)))
	(t (loop for i from 1 to lim
		 collect (insert-all i (all-combos2 (1- len) lim))))))

(defun insert-all (what in)
  (loop for elt in in
	append (loop for subelt in elt
		     collect (cons what subelt))))

(defun run (n)
  (loop for sol in (all-combinations n n)
	when (right? sol)
	collect sol))

;;; All we get here is one combination, as (3 2 4 1) for the n=4
;;; set. The number of the sentence is assumed to go as: (1 2 3 4).

(defun right? (s)
  ;; First we build the complete list.
  (let ((cl (loop for c in s
		  as n from 1 by 1
		  append (list c n))))
    ;;(print (list s cl))
    (loop for c in s
	  as n from 1 by 1
	  if (not (= c (count n cl)))
	  do (return nil)
	  finally (return t))))

(loop for n from 1 to 10
      do (format t "~%=== ~a ===~%" n)
      (mapcar #'print (run n))
      )
