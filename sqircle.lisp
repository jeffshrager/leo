;;; (load (compile-file "sqircle.lisp"))

;;; Based on a challenge from Leo to estimate the number of hits that
;;; will land in the "corners" if you randomly fire points at a circle
;;; enscribed in a square, and only 0.775 of them actually hit the
;;; square. This is the same as the way you estimate pi by
;;; sampling. My estimate was about 15% -- we use a standard r=1
;;; circle.

(defun oneshot ()
  (let* ((x (/ (1+ (random 1000)) 1000.0))
	 (y (/ (1+ (random 1000)) 1000.0))
	 (x (if (zerop (random 2)) (* -1 x) x))
	 (y (if (zerop (random 2)) (* -1 y) y))
	 )
    (< 1.0 (sqrt (+ (* x x) (* y y))))))

(defun run (&optional (n 10000) &aux (corners 0) (circles 0))
  (loop for i below n
	when (oneshot)
	do (incf corners)
	else do (incf circles))
  (* 0.775 (/ corners n)))

(pprint (run))
