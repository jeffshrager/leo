;; (load (compile-file "chains2.lisp"))
;; dot -Tpdf <file>.dot -o <file>.pdf
(setf *print-level* nil)

;;; Pillars of expt 3: 3 12 36 

(defun factor (n)
  (too-big-break? n)
  ;; This minor ugliness is because first-factor-tree returns primes
  ;; as a number and we always want the factor to be a list
  (let ((fs (factors-from (first-factor-tree n))))
    (if (atom fs) (list fs) fs)))

(defun all-flat-factors (n)
  (too-big-break? n)
  ;; This minor ugliness is because first-factor-tree returns primes
  ;; as a number and we always want the factor to be a list
  (flatten (first-factor-tree n)))

(defun flatten (l)
  (cond ((null l) ())
	((atom l) (list l))
	(t (append (flatten (car l))
		   (flatten (cdr l))))))
	
;;; This will always return the first left-atomic factor tree it comes
;;; to. You have to pass the result through factors-from to get the
;;; actual factors out of that.

(defvar *n->factors* (make-hash-table :test #'equal))
(clrhash *n->factors*)

(defun first-factor-tree (n)
  (too-big-break? n)
  (or (gethash n *n->factors*)
      (let ((fft 
	     (if (prime? n)
		 n
		 ;; Find the first number that is an even divisor of n
		 (loop for s from 2 to (1+ (truncate (sqrt n)))
		       as q = (/ n s)
		       if (= 1 q)
		       do (return n)
		       if (int? q)
		       do (return (list n (first-factor-tree s) (first-factor-tree q)))))))
	(setf (gethash n *n->factors*) fft)
	fft)))

(defun too-big-break? (n)
  (when (> n 10000000)
    (print n)
    (throw 'too-big-to-prime-test :too-big-to-prime-test)))

(defun ftd ()
  (loop for n being the hash-keys of *n->factors*
	using (hash-value fs)
	collect (print (list n fs))))

(defun int? (n)
  (= (truncate n) n))

;;; Turns the result of first-factor-tree into the actual factors, for
;;; example: (1234324 2 (617162 2 (308581 7 (44083 13 3391)))) =>
;;; (2 2 7 13 3391)

(defun factors-from (l &optional c)
  (cond ((atom l) l) ;; Handles prime case
	;; Stop at the leaves before hitting the prime case
	((atom (third l)) (append c (cdr l)))
	(t (factors-from (third l) (cons (second l) c)))))

;;; Testing for primes. (This is similar to factoring but doesn't have
;;; to recurse.)

(defun prime? (n)
  (too-big-break? n)
  (if (member n '(1 2 3 5 7 11)) t
      (if (< n 12) nil
	  (or (let ((fs (gethash n *n->factors*)))
		;; UUU Short (in time, I hope) for (= 1 (length ...))
		(or (numberp fs) (and (not (null fs)) (null (cdr fs)))))
	      ;; Find the first number that is an even divisor of n. If this gets
	      ;; to the end, it's prime!
	      (loop for s from 2 to (1+ (truncate (sqrt n)))
		    as q = (/ n s)
		    if (= 1 q)
		    do
		    (return nil)
		    if (int? q) 
		    do
		    (return nil)
		    finally (return (progn (setf (gethash n *n->factors*) n) t)))))))

;;; Chain runner for x'=2*(Sum of Prime Factors(x))+floor(0.5*x), x'=2*(Sum of Prime Factors(x))+ceil(0.5*x)

(defvar *from->to* (make-hash-table :test #'equal))
(defvar *to<-from* (make-hash-table :test #'equal)) ;; For finding f-numbers
(defvar *loops* nil)

(defun run-chain (n chain l limit top &key (trace? nil)) ;; Top is just passed along for loop recording purposes
  (if (or (= l limit))
      chain
      (catch 'too-big-to-prime-test ;; ??? Should this go outside someplace?
	(loop for next in (let* ((factors (factor n))
				 (sum-of-factors (reduce #'+ factors))
				 (n/2 (* 0.5 n))
				 (floor (floor n/2))
				 (ciel (floor (+ n/2 0.99)))
				 (a (+ floor (* 2 sum-of-factors)))
				 (b (+ ciel (* 2 sum-of-factors))))
			    (when (eq :full trace?) (format t "~%n= ~a, factors=~a, next-pair = (~a ~a)~%" n factors a b))
			    (if (= a b) (list a) (list a b)))
	      collect
	      (let ((new-chain (cons next chain)))
		(if (member next chain)
		    (progn (pushnew `(,next ,@(reverse (loop for elt in chain
						  until (= elt next)
						  collect elt))
					    ,next)
				    *loops* :test #'equal) new-chain)
		    (run-chain next new-chain (+ 1 l) limit top :trace? trace?)))))))

(defun record-chains (chains)
  (cond ((null chains) nil)
	((numberp (car chains))
	 (loop for (from to) on (reverse chains)
	       unless (null to)
	       do
	       (pushnew to (gethash from *from->to*))
	       (pushnew from (gethash to *to<-from*))))
	(t (record-chains (car chains))
	   (record-chains (cdr chains)))))

(defun run-chains (&key (start 1) (end 5) (step 1) (depth-limit 5) (trace? nil) &aux f-numbers loop-edges)
  ;; Trace can be t, nil, or :full. In :full it prints out all the chains!
  (when trace? (setf end 100))
  ;; This will just store the loop "key" (where the
  ;; loop was detected) and the top nodes (where the
  ;; chain started that when to that loop)
  (setf *loops* nil) 
  (clrhash *from->to*)
  (clrhash *to<-from*)
  (loop for n from start to end by step
	do (record-chains (run-chain n (list n) 1 depth-limit n :trace? nil)))
  ;; Finding f-numbers that have no input. These are *from->to* keys
  ;; that have no *to<-from* entry.
  (loop for from being the hash-keys of *from->to*
	unless (gethash from *to<-from*)
	do
	(pushnew from f-numbers))
  ;; Loop edges get colored red. We red them out of the *loops*
  ;; collector.
  (setf loop-edges nil)
  (loop for loop in *loops*
	do (loop for (from to) on loop
		 until (null to)
		 do (pushnew (cons from to) loop-edges)))
  (with-open-file
      (o (format nil "chains2.dot") :direction :output :if-exists :supersede)
    (format o "digraph G {~%")
    (labels ((dotedge (from to) (format o "~a -> ~a ~a~%" from to
					(if (member (cons from to) loop-edges :test #'equal)
					    "[color=red];" ";"))))
	    (loop for from being the hash-keys of *from->to*
		  using (hash-value to)
		  do
		  (dotedge from (car to))
		  (when (cadr to) (dotedge from (cadr to)))
		  ))
    (loop for f-number in f-numbers
	  do (format o "~a [style=filled, fillcolor=orange];~%" f-number))
    (format o "}~%")
    )
  )

;;; The idea here is to return the longest lists that aren't subsets
;;; of one another. For example, if l* = ((a b) (w x) (b a c) (x w z
;;; y)) -> ((a b c) (w x y z)) [order doesn't matter in either the top
;;; or sublists

(defun maximal-supersets (l*)
  (loop for l1 in l*
	unless (loop for l2 in l*
		     if (proper-subsetp l1 l2)
		     do (return t))
	collect l1))
		     
(defun proper-subsetp (a b)
  (and (subsetp a b)                       ; every element of a is found in b
       (not (endp (set-difference b a))))) ; b has another element

(untrace)
;(trace run-chain)
(run-chains :end 50 :depth-limit 25)
