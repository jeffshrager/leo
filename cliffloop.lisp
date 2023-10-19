;;; (load (compile-file "cliffloop.lisp"))

;;; Memoization should help significantly!

(defvar *n->factors* (make-hash-table))

;;; This can be be sped up in various ways:
;;;   factorp (used under prime?) doesn't have to recurse
;;;   the whole thing can memoize previous results

(defun factor (n)
  ;; This minor ugliness is because first-factor-tree returns primes
  ;; as a number and we always want the factor to be a list
  (let ((fs (factors-from (first-factor-tree n))))
    (if (atom fs) (list fs) fs)))
  
;;; This will always return the first left-atomic factor tree it comes
;;; to. You have to pass the result through factors-from to get the
;;; actual factors out of that.

(defun first-factor-tree (n)
  (or (gethash n *n->factors*)
      (let ((fft 
	     (if (prime? n)
		 n
		 ;; Find the first number that is an even divisor of n
		 (loop for s from 2 to (1+ (truncate (sqrt n)))
		       as q = (/ n s)
		       if (= 1 q)
		       do (return n)
		       if (int? q)
		       do (return (list n (first-factor-tree s) (first-factor-tree q)))))))
	(setf (gethash n *n->factors*) fft)
	fft)))

(defun int? (n)
  (= (truncate n) n))

;;; Turns the result of first-factor-tree into the actual factors, for
;;; example: (1234324 2 (617162 2 (308581 7 (44083 13 3391)))) =>
;;; (2 2 7 13 3391)

(defun factors-from (l &optional c)
  (cond ((atom l) l) ;; Handles prime case
	;; Stop at the leaves before hitting the prime case
	((atom (third l)) (append c (cdr l)))
	(t (factors-from (third l) (cons (second l) c)))))

;;; Testing for primes. (This is similar to factoring but doesn't have
;;; to recurse.)

(defun prime? (n)
  (or (let ((fs (gethash n *n->factors*)))
	;; UUU Short (in time, I hope) for (= 1 (length ...))
	(and (not (null fs)) (null (cdr fs)))) 
      ;; Find the first number that is an even divisor of n. If this gets
      ;; to the end, it's prime!
      (loop for s from 2 to (1+ (truncate (sqrt n)))
	    as q = (/ n s)
	    if (= 1 q)
	    do (return nil)
	    if (int? q) 
	    return nil
	    finally (return t))))

;;; Loop finder

(defun find-path-a (n &optional h)
  (if (member n h) h
      (let ((new-n (+ 1 (reduce #'+ (factor n)))))
	(find-path-a new-n (cons n h)))))

(defun find-path-b (n &optional h)
  (if (member n h) h
      (let* ((factors (factor n))
	     (new-n (+ 1 (length factors) (reduce #'+ factors))))
	(find-path-b new-n (cons n h)))))

(defun find-path-c (n &optional h)
  (if (member n h) h
      (let* ((factors (factor n))
	     (new-n (+ 2 (length factors) (reduce #'+ factors))))
	(find-path-c new-n (cons n h)))))

;;; Testing and execution

;;; dot -Grankdir=LR -Tpng g.gv > g.png

(defun find-paths (fn break-list &key (limit 100000) (filename "ppaths"))
  (let ((gvfile (format nil "~a.gv" filename)))
    (with-open-file
	(o gvfile  :direction :output :if-exists :supersede)
      (format o "strict digraph mygraph {~%")
      (clrhash *n->factors*)
      (format t "Testing paths up to ~a...~%" limit)
      (loop for n from 2 to limit
	    as path = (funcall fn n)
	    unless (member (first path) break-list)
	    do
	    ;;(print path)
	    (loop for (from to) on (reverse path)
		  do (format o "~a -> ~a~%" from to))
	    )
      (format o "}~%"))
    (sleep 1) ;; Let the gv file close
    (let ((pngfile (format nil "~a.png" filename)))
      (if (probe-file pngfile) (delete-file pngfile))
      (sb-ext:run-program "/usr/local/bin/dot" (list "-Grankdir=LR" "-Tpng")
			  :input gvfile
			  :output pngfile :if-output-exists :supersede
			  :wait t))
    ))

(find-paths #'find-path-a '() :limit 1000 :filename "a1000")
(find-paths #'find-path-b '() :limit 1000 :filename "b1000")
(find-paths #'find-path-c '() :limit 1000 :filename "c1000")
