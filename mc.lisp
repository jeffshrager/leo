;;; (load (compile-file "mc.lisp"))

(defparameter *tp* ;; xition probs
  '((w 0.5 i)
    (w 0.2 d)
    (w 0.3 w)
    (i 0.5 i)
    (i 0.5 d)
    ))

(defun crush-tp (tp)
  (loop for node in (print (loop with nodes = nil for node in tp do (pushnew (car node) nodes) finally (return nodes)))
	as nodes = (print (sort (loop for elt in tp when (eq node (car elt)) collect elt) #'> :key #'second))
	collect (cons node (loop with i = 0.0 for (from v to) in nodes collect `(,(incf i v) ,to)))))

(defparameter *ts* (crush-tp *tp*)) ;; Transition sums
;;; Crushing makes the transition probs into xition sums, as:
;;;  '((w (0.3 w) (0.8 i) (1.0 d))
;;;    (i (0.5 i) (1.0 d))))
;;; which is more convenient for running.

(defun run1 (&key (start-at 'w))
  (loop with state = start-at
	with trace = (list start-at)
	for new-state = (next-from state)
	until (eq 'd new-state)
	do 
	(setf state new-state)
	(push state trace)
	finally (return trace)))

(defun next-from (state)
  (let* ((from-tss (cdr (assoc state *ts*)))
	 (r (/ (random 1000) 1000)))
    (loop for (v nw) in from-tss
	  when (>= v r)
	  do (return nw))))

(defun runn (&key (n 1000) (start-at 'w))
  (loop for r below n
	with v = (list 'w (list) 'i (list))
	as ss = (run1 :start-at start-at)
	do
	(push (count 'w ss) (second v))
	(push (count 'i ss) (fourth v))
	finally
	(prog nil
	   (setf (second v) (mean (second v))
		 (fourth v) (mean (fourth v)))
	   (print v)
	   (return v))))

(defun mean (l)
  (float (/ (reduce #'+ l) (length l))))

(untrace)
;(trace run1)
(print (runn :n 10000 :start-at 'w))	
(print (runn :n 10000 :start-at 'i))	
