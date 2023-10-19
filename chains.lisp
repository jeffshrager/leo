;; (load (compile-file "chains.lisp"))
;; dot -Tpdf chains.dot -o chains.pdf
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
  (when (> n 10000000000000)
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

(defun run-chains (&key (start 3) (end 1000000) (step 2) &aux (report-every (/ end 20)))
  (clrhash *from->to*)
  (clrhash *loops*)
  (loop for n from start to end by step
	do
	(when (zerop (mod n report-every)) (print n))
	(loop for (to from) on (let ((chain (run-chain n)))
				    (unless (eq :too-big-to-prime-test chain) chain))
		 until (null from)
		 do (setf (gethash from *from->to*) to)))
  (with-open-file
      (o "chains.dot" :direction :output :if-exists :supersede)

    (format o "digraph G {~%")
    (loop for from being the hash-keys of *from->to*
	  using (hash-value to)
	  do (format o "~a -> ~a;~%" from to))
    (format o "}~%")
    )
  (format t "Loops:~%")
  (loop for base being the hash-keys of *loops*
	using (hash-value chains)
	do
	(format t "  ~a chains land at ~a~%" (length chains) base)
	;;(format t "  ~a chains land at ~a from: ~{~a~^, ~}~%" (length chains) base (mapcar #'(lambda (c) (car (last c))) chains))
	;;(format t "  ~a chains land at ~a:: ~a~%" (length chains) base chains)
	)
  )
    
(defun run-chain (n &aux chain high low)
  (catch 'too-big-to-prime-test 
    (setf chain (list n)) 
    (loop as next = (progn (if (prime? n) (setf high n low 1)
			       (let* ((factors (factor n))
				      (lowest-factor (apply #'min factors))
				      (all-but-lowest (delete lowest-factor factors :count 1)))
				 ;;(format t "~%factors=~a, lowest-factor=~a, all-but-lowest=~a~%"
				 ;;	 factors lowest-factor all-but-lowest)
		      		 (setf high (reduce #'* all-but-lowest) low lowest-factor)))
			   (- (* 2 high) low))
	  do
	  (when (member next chain)
	    (when (> next 1) (push chain (gethash next *loops*)))
	    (return chain))
	  (push next chain)
	  (setf n next))))

(untrace)
;(trace run-chain)
(run-chains :end 1000000)
