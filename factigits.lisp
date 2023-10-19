; (load (compile-file "factigits.lisp"))

(defun fact (n)
  (if (= n 0) 
      1 
    (* n (fact (- n 1)))))

(defun fd (n) 
  (let ((r (fact n)))
    (list (length (format nil "~a" r)) (end-zeros r) r)))

(defun end-zeros (n)
  (loop for c across (reverse (format nil "~a" n))
	as i from 0 by 1
	until (not (char-equal c #\0))
	finally (return i)))

(defvar ip "314159265358979323846264338327950288419716939937510582")

(defun find-pi (n)
  (let ((s (format nil "~a" n)))
    (loop for p from 0 to (length ip)
	  with len = nil
	  as ipsub = (subseq ip 0 p)
	  as loc = (search ipsub s)
	  do (if loc (setf len p)
	       (return len)))))

(defvar *longest-n* nil)
(defvar *longest* nil)

(defun pi-fact (f n)
  (if (zerop (mod n 1000)) (print n))
  (let ((len (find-pi f)))
    (if (> len *longest*)
	(setf *longest* (print len) *longest-n* (print n))))
  f)
	     
(defun run (&optional (top 1000))
  (setf *longest* 0 *longest-n* 0)
  (loop for n from 1 to top
	with fact = 1
	do
	(setf fact (* fact n))
	(pi-fact fact n)
	finally (return (list *longest* *longest-n*))
	))

(run 100000)
