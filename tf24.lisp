;;; (load (compile-file "tf24.lisp"))

(defparameter *ops* '(* - + / expt))
(defparameter *lops* (length *ops*))
(defparameter *ns* '(2 4 9 12))
;;(defparameter *ns* '(1 1 1 1))
(defparameter *lns* (length *ns*))
(defparameter *depth-limit* 4)
(defvar *value->expr* (make-hash-table :test #'equal))
(defvar *expr->value* (make-hash-table :test #'equal))

(defun run ()
  (loop for target from 20 to 25
	do (find-expr target)))

(defun run24 ()
  (clrhash *expr->value*)
  (loop for i below 100
	as x = (find-expr 24)
	unless (gethash x *expr->value*)
	do
	(print x)
	(setf (gethash x *expr->value*) t)
	))

(defun find-expr (target)
  ;;(format t "~% ======= ~a~%" target)
  (loop as power from 3 to 7 by 1
	as ntries = (expt 10 power)
	as (shortest . minlength) = (find-expr-in-n-tries target ntries)
	until shortest
	finally (progn (format t "~a (~a) ~a [~a]~%" target minlength shortest power)
		       (return shortest))))

(defun find-expr-in-n-tries (target ntries)
  (loop as try below ntries
	as expr = (an-expr *depth-limit*)
	with shortest = nil
	with minlength = 100
	as lexpr = (length (flatten expr))
	when (and (< lexpr minlength) (allowed-expr? expr) (ignore-errors (= target (eval expr))))
	do
	;;(print expr) (terpri) 	
	(setf shortest expr minlength lexpr)
	finally (return (if shortest (cons shortest minlength)))))

(defun flatten (l)
  (cond ((null l) ())
	((atom l) (list l))
	(t (append (flatten (car l))
		   (flatten (cdr l))))))

(defun allowed-expr? (expr)
  ;; Must have exactly the 4 numbers and the four operators (in any order)
  (let ((flat-expr (flatten expr)))
    (and 
     (<= (count 'expt flat-expr) 1) ;; or it'll loop with huge numbers!
     (uses-all-only-once flat-expr *ns*)
     ;;(uses-all-only-once flat-expr *ops*)
     )))

(defun uses-all-only-once (flat-expr items)
  (set-equal items (loop for e in flat-expr if (member e items) collect e)))

(defun set-equal (a b)
  (and (= (length a) (length b))
       (null (set-difference a b))
       (null (set-difference b a))))

(defun an-expr (depth-limit)
  (cond ((zerop depth-limit) (nth (random *lns*) *ns*))
	((zerop (random 2)) (nth (random *lns*) *ns*))
	(t (list (nth (random *lops*) *ops*)
		 (an-expr (1- depth-limit))
		 (an-expr (1- depth-limit))))))

#|

(defun exprs (d)
  (cond ((zerop d) nil)
	(t (append (copy-seq *ns*)
		   (let ((xs (exprs (1- d))))
 		     (xspread xs xs))))))
(defun xspread (x1 x2)
  (loop for e1 in x1
	append (loop for e2 in x2
		     append (loop for op in *ops*
				  collect `(,op ,e1 ,e2)))))

(defvar *value->expr* (make-hash-table :tests #'equal))
(defvar *expr->value* (make-hash-table :tests #'equal))

(defun run (&aux expers (exprs *depth-limit*))
  (clrhash *value->expr*)
  (clrhash *expr->value*)
  (loop for v below 100
	as x = (gethash v *value->expr*)
	if x
	do (return x)
	else
	do (loop for x in (exprs *depth-limit*)
		 as v2 = (gethash x *expr->value*)
		 if (= v2 v)
		 do (return x)
		 else (let ((v (ignore-errors (eval x))))
			(when v
			    (progn
			      (setf (gethash x *expr->value*) v
				    (gethash v *value->expr*) x)))))))
			  
|#
(run24)

