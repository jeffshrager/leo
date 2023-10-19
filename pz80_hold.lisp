;;; (load (compile-file "pz80.lisp"))

(defvar *all-n* nil) ;; Efficiency hack so we don't have to create this every time.
(defvar *n* nil)

(defun run ()
  (loop for n from 2 to 10
	do (print (run-n n))))

(defun run-n (&optional (n 10) (reps 10))
  (setf *n* n *all-n* (loop for m below n collect m))
  (/ 
   (loop for rep below reps
	 sum (tryn))
   (float reps)))

(defun tryn (&optional (ncustomers 100))
  (/ (float 
      (loop for try below ncustomers
	    sum (1- (length (getashirt (random *n*))))))
     ncustomers))

(defun getashirt (want)
  (let ((mapping (randomly-order))
	tries)
    ;;(print mapping)
    (loop as next-try = (let ((nt (new-try tries)))
			  (push nt tries) nt)
	  as got = (nth next-try mapping)
	  until (= want got)
	  finally (return tries))))

(defun new-try (tries)
  (let ((remainder (set-difference *all-n* tries)))
    (nth (random (length remainder)) remainder)))

(defun randomly-order ()
  (loop for m below *n*
	with can = (copy-list *all-n*)
	as x = (nth (random (length can)) can)
	do (setf can (remove x can))
	collect x))

(untrace)
;(trace tryn getashirt)
(run)
