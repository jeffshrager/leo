;;; (load (compile-file "tf24.lisp"))

(defparameter *ops* '(* - + / expt))
(defparameter *lops* (length *ops*))
;;(defparameter *ns* '(1 2 3 4))
(defparameter *ns* '(1 1 1 1))
(defparameter *lns* (length *ns*))
(defparameter *depth-limit* 5)

(defun run ()
  (loop for target below 100
	do (find-expr target)))

(defun find-expr (target)
  ;;(format t "~% ======= ~a~%" target)
  (loop as power from 3 to 7 by 1
	as ntries = (expt 10 power)
	as (shortest . minlength) = (find-expr-in-n-tries target ntries)
	until shortest
	finally (format t "~a (~a) ~a [~a]~%" target minlength shortest power)))

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
     ;;(uses-all-only-once flat-expr *ns*)
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

(run)
