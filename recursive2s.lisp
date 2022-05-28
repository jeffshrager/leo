; (load (compile-file "recursive2s.lisp"))

(defvar *n->2^n* (make-hash-table :test #'equal))
(defvar *n->nsubs* (make-hash-table :test #'equal))
(defvar *digit->count* (make-hash-table :test #'equal))

(defun rec2s (top)
  (clear-tables)
  (compute-recursive-powers-of-2 top)
  (report-digit-counts)
  (report-recursive-powers-of-2)
  )

(defun clear-tables ()
  (clrhash *digit->count*)
  (clrhash *n->2^n*)
  (clrhash *n->nsubs*)
  )

(defun compute-recursive-powers-of-2 (top)
  (loop for n below top
	as n2 = (expt 2 n)
	as n2s = (format nil "~a" n2)
	do 
	(record-digit-counts n2s)
	(when (zerop (mod n 100)) (print n))
	(setf (gethash n *n->2^n*) n2s)
	(loop for i from 7 below n
	      as is = (gethash i *n->2^n*)
	      as p = (when is (search is n2s))
	      when p
	      do (push (list i is p) (gethash n *n->nsubs*)))))

(defun report-recursive-powers-of-2 ()
  (loop for entry in 
	(sort 
	 (loop for n being the hash-keys of *n->nsubs*
	       using (hash-value is)
	       collect (list (length is) n (expt 2 n) is))
	 #'> :key #'car)
	do (print entry)))

(defun record-digit-counts (s)
  (loop for c across s
	do (incf (gethash c *digit->count* 0))))

(defun report-digit-counts ()
  (loop for c being the hash-keys of *digit->count*
	using (hash-value k)
	do (print (cons c k))))

(rec2s 1000)
