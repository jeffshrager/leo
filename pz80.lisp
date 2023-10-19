;;; (load (compile-file "pz80.lisp"))

;; https://fivethirtyeight.com/features/can-you-crack-the-safe/
;; SOLVED CORRECTLY! N=9

(defvar *all-n* nil) ;; Efficiency hack so we don't have to create this every time.
(defvar *n* nil)

(defun run (&optional (reps 100) (max-n 10))
  (loop for n from 2 to max-n
	do (print (list n (run-n n reps)))))

(defun run-n (n reps &aux (freps (float reps)))
  (setf *n* n *all-n* (loop for m below n collect m))
  (/ (loop for rep below reps sum (tryn)) freps))

;;; 80% returns means that for every buy they have 4 returns, so the
;;; number coming out of this function should be 4 (I guess)

(defun tryn (&optional (ncustomers 100))
  (/ (float (loop for try below ncustomers sum (length (getashirt))))
     ncustomers ;; This is the same as the number of buys
     ))

(defun getashirt (&aux tries)
  ;; Just random numbers w/o replacement until you get the one you want.
  (loop as next-try = (new-try tries)
	until (zerop next-try) ;; Makes no difference which shirt we want, so always just try to get #0
	do (push next-try tries) 
	finally (return tries)))

(defun new-try (seen)
  "Get a random number below *n* that's not in the seen list (i.e., w/o replacement)."
  (let ((remainder (set-difference *all-n* seen)))
    (nth (random (length remainder)) remainder)))

(untrace)
;(trace tryn getashirt)
(run)
