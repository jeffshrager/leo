OUT OF DATE -- SEE JS CODE!

;;; (load (compile-file "gjdle.lisp"))

;;; Creates sytems of linear equation problems that can be solved
;;; through gauss-jordan elimination using only integers.

(defparameter *n* 5)

(defun random-row+target (&optional (n *n*))
  (loop for i below (1+ n) collect (random 10)))

(defun init-mat (&optional (n *n*))
  (loop for i below n collect (random-row+target n)))

;;; We pick a random row and either multiply or divide it by 1-5 and
;;; then add or subtract is to/from another random row, but we need to
;;; use every row at least once, so we do this by creating a random
;;; sequence from the contatenated list of values. (Can only divide if
;;; the division results in integers)

(defun random-permute (sequence)
  (let ((copy (copy-seq sequence)))
    (loop for i downfrom (length copy) to 2 do
          (rotatef (elt copy (random i))
                   (elt copy (1- i))))
    copy))

(defun row-selection-sequence (&optional (n *n*))
  (loop with set = (loop for i from 1 to n collect i)
	for i below 5
	append (random-permute set)))

(defvar *rss* (row-selection-sequence))

(defun operate (m)
  (let* ((n (length (car m)))
	 (from (pop *rss*))
	 ;; Make sure we don't accidentally get the same row twice at the fold
	 (to (loop for to = (pop *rss*) until (not (= to from)) finally (return to)))
	 (*/op (nth (random 2) (list #'* #'*))) ;; FFF Allow /
	 (+-op (nth (random 5) (list :swap #'+ #'- #'+ #'-)))
	 (mult (1+ (random 5)))
	 )
    (push (list from to */op +-op mult) *seq*)
    (let ((fromrow (nth (1- from) m))
	  (torow (nth (1- to) m)))
      (if (eq :swap +-op)
	  (progn
	    (setf (nth (1- from) m) torow)
	    (setf (nth (1- to) m) fromrow))
	  (loop as p from 0 below n
		do
		(rplaca (nthcdr p torow)
			(funcall +-op (nth p torow) (funcall */op mult (nth p fromrow)))))))
     m))
    
(defun test (&aux m)
  (setf *seq* nil)
  (setf *rss* (row-selection-sequence))
  (setq m (loop for row below *n*
	        as i = (append (loop for i below *n* collect 0) (list (random 10)))
		do (setf (nth row i) 1)
		collect i))
  (loop for i below (+ 3 (random 10)) do (operate m))
  (print m)
  (print *seq*)
  )

(untrace)
(test)
