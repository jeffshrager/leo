;;; (load (compile-file "moe.lisp"))

(defparameter *population-size* 1000)
(defparameter *target-number-of-greens* 60)
(defparameter *survey-sample-size* 50)
(defparameter *sample-cache-size* 5000)
(defparameter *pswmoe-reps* 5000)

#|

The sampling protocol is to take 50 from 1000. The sim runs around 6%,
so that's 60 added to the 1000. Since it's uniform sampling, we only
really need to make 1 set of these, and then sample. The sampling
takes 50 from the 1000, for 500 reps (or however many reps we ask for
by sending in the n arg).

|#

;;; Since we're choosing a random position in the urn, just making the
;;; first 60 of 1000 green should give us a good estiamte of the
;;; number of greens.

(defvar *urn*
  (loop for i below *population-size*
	collect (if (< i *target-number-of-greens*) :green :blue)))

;;; This confusing thing with *sample-cache* is just an efficiency
;;; hack so that I don't have to replicate the draw zillions of times,
;;; which takes too long. So we pre-compute the results of 1000
;;; surveys and then just pick one when asked to run a survey.

(defvar *sample-cache* (make-hash-table :test #'equal))

;;; Random numbers w/o replacement the hard (read:stupid) way

(defvar *p->seen* (make-hash-table :test #'equal))

(defun sample ()
  (clrhash *p->seen*)
  (loop with i = 0
	with count = 0
	until (= *survey-sample-size* i)
	as p = (random *population-size*)
	do (when (null (gethash p *p->seen*))
	     (incf i)
	     (if (eq :green (nth p *urn*))
		 (incf count)))
	finally (return (float (/ count *survey-sample-size*)))))

;;; This actually simulates doing the survey by randomly choosing of
;;; the 1000 pre-computed surveys.

(defun sample-*survey-sample-size* ()
  (gethash (random *sample-cache-size*) *sample-cache*))

(defun rmotwc () ;; report mean of the whole cache
  (mean 
   (loop for v being the hash-values of *sample-cache*
	 collect v)))

;;; Simple stats utils.

(defun mean (l)
  (/ (reduce #'+ l) (float (length l))))

(defun sd (l)
  (if (null (cdr l))
      0.0
    (sqrt
     (let ((m (mean l)))
       (* (/ 1.0 (1- (length l)))
	  (reduce #'+ (mapcar #'(lambda (x) (expt (- x m) 2)) l))
	  )))))

(setf *print-pretty* nil)

(defun run-surveys (n)
  (let ((samples (loop for i below n collect (sample-*survey-sample-size*))))
    (values (mean samples) (sd samples))))

(defun run (n)
  (when (zerop (hash-table-count *sample-cache*))
    (loop for i below *sample-cache-size* do (setf (gethash i *sample-cache*) (sample))))
  (multiple-value-bind (mean sd) (run-surveys n)
    (let* ((moe (* 1.96 (/ sd (sqrt n))))
	   ;; Note that if we didn't know that we'd set the data to
	   ;; exactly 6%, this would have to work off the actual mean,
	   ;; but since we did force it to be exactly 6%, in order to
	   ;; actually get 95% of the observations to fall withtin
	   ;; +-MOE, we need to force it here too, but we need the sd
	   ;; from the simulations.
	   (mean 0.06) 
	   (ci-low (- mean moe))
	   (ci-high (+ mean moe))
	   ;; 95% of the estimates of the mean should be in this range. NOTE:
	   ;; NOT 95% of the samples! (Turns out this works well for very large
	   ;; numbers of reps, like 2500, but at the same as as the sd was
	   ;; computed, it doesn't always work, which is a little concerning.
	   (pswmoe (/ (loop for i below *pswmoe-reps*
			    as estm = (run-surveys n)
			    if (and (>= estm ci-low) (<= estm ci-high)) 
			    sum 1.0)
		      *pswmoe-reps*)))
      (list :n n :mean mean :sd sd :moe moe :ci-low ci-low :ci-high ci-high :pswmoe pswmoe))))

(format t "*population-size*=~a, *target-number-of-greens*=~a, *survey-sample-size*=~a, *sample-cache-size*=~a, *pswmoe-reps*=~a~%"
	  *population-size*
	  *target-number-of-greens*
	  *survey-sample-size*
	  *sample-cache-size*
	  *pswmoe-reps*
	  )
(loop for n from 250 to 3000 by 250
      do (print (run n)))
