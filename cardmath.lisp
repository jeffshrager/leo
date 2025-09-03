;; (load "cardmath.lisp")

;;; This plays a card game that I invented to play with Leo, but
;;; actually it was way too hard. You deal each player four cards
;;; (after removing face cards from the deck), and they word TOGETHER
;;; to make the same number by any combination of algebraic
;;; manipulation, including using the cards as digits. For example, if
;;; you get 1 2 3 4, you can make 1+2+3+4, 1*2+3*4, 12+34, etc. The
;;; idea is to end up with the same final value, created by each of
;;; the four cards. It turns out actually be the quite challanging!

(defvar *h1* nil)
(defvar *h2* nil)
(defvar *total->exprs* (make-hash-table :test #'equal))

(defun play ()
  (setq *h1* nil *h2* nil)
  (loop for i below 4 
	do 
	(push (1+ (random 10)) *h1*)
	(push (1+ (random 10)) *h2*))
  (format t "Hand 1 = ~a~%" *h1*)
  (format t "Hand 2 = ~a~%" *h2*)
  ;; Load up the solutions table.
  (clrhash *total->exprs*)
  (create-all-exprs *h1* :h1)
  (create-all-exprs *h2* :h2)
  ;; Find matching solutions (entries that have both :h1 and :h2 components
  (loop for total being the hash-keys of *total->exprs*
	using (hash-value exprs)
	as h1s = (find-all :h1 exprs)
	as h2s = (find-all :h2 exprs)
	when (and h1s h2s)
	do (format t "~a: ~a h1s, ~a h2s~%" total (length h1s) (length h2s))
	(format t "  h1: ~a~%  h2: ~a~%" (car h1s) (car h2s))))

(defun find-all (key alist)
  (loop for elt in alist
	when (eq key (car elt))
	collect elt))

(defun dht (table &optional (n 10))
  (maphash #'(lambda (key value)
	              (when (zerop (decf n)) (return-from dht))
		      (format t "~s: ~s~%" key value)       
		             )
	      table))

(defun create-all-exprs (ns key)
  (loop for (total expr) in (all-permuted-totals+exprs ns)
	unless (eq :nan total)
	do (push `(,key ,expr) (gethash total *total->exprs*))))

(defun permutations (bag) ;; Borrowed from PN
  "Return a list of all the permutations of the input."
  (if (null bag)
      '(())
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
                            (remove e bag :count 1 :test #'eq))))
              bag)))

(defun all-combinations (n l)
  "Each way to make a list of n elts from the list l, with repeats."
  (cond ((= n 1)
	 (mapcar #'list l))
	(t (loop for i in l
		 append (loop for nl in (all-combinations (1- n) l)
			      collect (cons i nl))))))

(defparameter *all-ops-combinations*
  (all-combinations 3 '(* + - /!)))

(defun /! (a b)
  (if (zerop b) 
      (throw 'nan-break :nan) 
    (/ a b)))

(defun all-single-totals+exprs (ns)
  (loop for (o1 o2 o3) in *all-ops-combinations*
	as expr = `(,o1 ,(first ns)
			(,o2 ,(second ns)
			     (,o3 ,(third ns) ,(fourth ns))))
	collect `(,(catch 'nan-break (eval expr))
		  ,expr)))

(defun all-permuted-totals+exprs (ns)
  (loop for perm in (permutations ns)
	append (all-single-totals+exprs perm)))

(pprint (setq r (play)))