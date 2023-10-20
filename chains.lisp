;; (load (compile-file "chains.lisp"))
;; dot -Tpdf <file>.dot -o <file>.pdf

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

;;; Chain runner

(defvar *from->to* (make-hash-table :test #'equal))
(defvar *loops* (make-hash-table :test #'equal))
(defvar *delta->stacks* (make-hash-table :test #'equal))

(defun run-chain (fn n &key (trace? nil) &aux chain low high)
  (when trace? (format t "~%--- Running chain for ~a: " n))
  (catch 'too-big-to-prime-test 
    (setf chain (list n)) 
    (let ((chain 
	   (loop as next = (progn (if (prime? n)
				      (progn
					(setf high n low 1)
					(when (eq :full trace?)
					  (format t "~%For prime ~a, low=~a, high=~a ==> " n low high))
					)
				      (let* ((factors (factor n))
					     (lowest-factor (apply #'min factors))
					     (all-but-lowest (delete lowest-factor factors :count 1)))
		      			(setf high (reduce #'* all-but-lowest) low lowest-factor)
					(when (eq :full trace?)
					  (format t "~%factors=~a, lowest-factor=~a, all-but-lowest=~a, high (* highs)=~a ==> "
						  factors lowest-factor all-but-lowest high))))
				  (apply fn (list low high)))
		 do (when trace? (format t "  ~a, " next))
		 (when (member next chain)
		   (when (> next 1) (push chain (gethash next *loops*)))
		   (return chain))
		 (push next chain)
		 (setf n next))))
      (when (eq trace? :full) (format t "~%Chain: ~a~%" chain))
      chain)))

;;; A stack is a series of values where the delta between then keeps
;;; doubling.  This example starts at 49 (d=41):

;;; (stack? '(11 15 13 21 49 90 172 336 664 1320))

;;; Stacks could start anywhere, so we need to check from each
;;; starting point. And the last two don't count as the start of a
;;; stack (so this is actually a little tricky!)

(defun stack? (l)
  (loop for potential-stack on l
	until (< (length potential-stack) 3) ;; Don't check stacks of 2!
	as d/l = (stack2? potential-stack) ;; Get the initial delta, if this actually is a stack.
	if d/l ;; And if so, return it.
	do (return d/l)
	finally (return nil) ;; Finally, if there aren't any stacks, return nil.
	))

(defun stack2? (l)
  (let ((d (- (second l) (first l)))) ;; Set the delta between first two
    (loop for e from 0 by 1 ;; This will be our exponent
	  as (a b) on l ;; The too-short case should be blocked by our caller
	  until (null b)
	  if (not (= (- b a) (* d (expt 2 e))))
	  do (return nil)
	  finally (return (list d l))
	  )))

(defun run-chains (fn name &key (start 3) (end 100) (step 2) (trace? nil) &aux (report-every (/ end 20)))
  ;; Trace can be t, nil, or :full. In :full it prints out all the chains!
  (when trace? (setf end 100))
  (clrhash *from->to*)
  (clrhash *loops*)
  (clrhash *delta->stacks*)
  (loop for n from start to end by step
	do
	(when (and (not trace?) (zerop (mod n report-every)) (print n)))
	(loop for (to from) on (let ((chain (run-chain fn n :trace? trace?)))
				 (unless (eq :too-big-to-prime-test chain)
				   (let ((d/l (stack? chain)))
				     (when d/l
				       (let ((d (first d/l))
					     (l (second d/l)))
					 (push l (gethash d *delta->stacks*))
					 (when trace? (format t "** Stack: ~a (delta = ~a)~%" l d)))))
				   chain))
	      until (null from)
	      do (setf (gethash from *from->to*) to)))
  (with-open-file
      (o (format nil "chains_for_~a_to_~a.dot" name end) :direction :output :if-exists :supersede)

    (format o "digraph G {~%")
    (loop for from being the hash-keys of *from->to*
	  using (hash-value to)
	  do (format o "~a -> ~a;~%" from to))
    (format o "}~%")
    )
  (format t "~%Loops for ~a:~%" name)
  (loop for base being the hash-keys of *loops*
	using (hash-value chains)
	do
	(cond ((null trace?)
	       (format t "  ~a chains land at ~a~%" (length chains) base))
	      ((eq :full trace?)
	       (format t "  ~a chains land at ~a:: ~a~%" (length chains) base chains))
	      (t (format t "  ~a chains land at ~a from: ~{~a~^, ~}~%" (length chains) base (mapcar #'(lambda (c) (car (last c))) chains)))
	      ))
  (format t "~%Stacks for ~a:~%" name)
  (loop for delta being the hash-keys of *delta->stacks*
	using (hash-value stacks)
	do
	(cond ((null trace?)
	       (format t "  ~a stacks have delta=~a:   ~a~%" (length stacks) delta
		       (maximal-supersets stacks)))
	      ((eq :full trace?)
	       (format t "  ~a stacks have delta=~a:   ~a~%" (length stacks) delta stacks))
	      (t (format t "  ~a stacks have delta=~a from: ~{~a~^, ~}~%"
			 (length stacks) delta (mapcar #'(lambda (c) (car (last c))) stacks)))
	      ))
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
;(trace run-chain stack? stack2?)
;(run-chains #'(lambda (low high) (- (* 2 high) low)) "2h-l" :end 1000 :trace? nil)
;(run-chains #'(lambda (low high) (+ (* 2 high) low)) "2h+l" :end 1000 :step 2 :trace? :full)
(run-chains #'(lambda (low high) (+ (* 2 low) high)) "2l+h" :end 2000 :step 1 :trace? nil)
