;;; (load (compile-file "chem.lisp"))

(defparameter *tables*
  '((;;table1
     (;;down 
      (al (cl 3))
      (ba (cl 2))
      (k i)
      ((k 3) po4)
      (mg (cl 2))
      (na oh)
      (na br)
      ((nh4 2) co3)
      ((na 2) so4)
      ((na 2) c2o4)
      (na c2h3o2)
      (sn (cl 2))
      ((cr 2) (so4 3))
      (co (cl 2))
      ((k 2) cro4)
      (ni (cl 2))
      )
     (;;across
      (ag no3)
      (ca (no3 2))
      (cu (no3 2))
      (fe (no3 3))
      (sr (no3 2))
      (zn (no3 2))
      (pb (no3 2))
      (mn (no3 2))
      )
     (;;obs
      (p - - p - - - - )
      (p - - - - - - p )
      (p - p p - - p p )
      (p p p p p p p p )
      (p - p p - - p p )
      (p p p p - p p p )
      (p - p p - - - p )
      (p - p p p p p p )
      (- - - - p - - - )
      (p - p p p - - p )
      (- - - p - - - p )
      (p - - p - - - - )
      (- - - - - - - - )
      (p - - p - - p p )
      (p p - p - p p p )
      (p - - p - p p p )
      )
     )
    (;;table2
     (;;down
      ((na 2) so4)
      (na oh)
      (na br)
      ((na 2) c2o4)
      (na c2h3o2)
      )
     (;;across
      (k i)
      ((k 3) po4)
      ((nh4 2) co3)
      ((k 2) cro4)
      )
     (;;obs
      (p - p - )
      (- - - - )
      (- - - - )
      (- - - - )
      (- - - - )
      )
     )
    (;;table3
     (;;down
      (al (cl 3))
      (ba (cl 2))
      (mg (cl 2))
      (sn (cl 2))
      (co (cl 2))
      (ni (cl 2))
      ((cr 2) (so4 3))
      )
     (;;across
      (k i)
      ((k 3) po4)
      ((na 2) so4)
      (na oh)
      (na br)
      ((na 2) c2o4)
      (na c2h3o2)
      ((nh4 2) co3)
      ((k 2) cro4)
      )
     (;;obs
      (- p - p - - - - p )
      (- p p - - p - p p )
      (- p - p - - - - - )
      (- p p - - - p p p )
      (p p - p - p p p p )
      (- p - p - p p p - )
      (- p x p p - - p - )
      )
     )
    ))

(defvar *pair->obs* (make-hash-table :test #'equal))
(defvar *reactions* nil)
(defvar *products* (make-hash-table :test #'equal))
(defvar *ion->p-** (make-hash-table :test #'equal))
(defvar *product->other-products-and-obs* (make-hash-table :test #'equal))

(defun crecord ()
  (clrhash *ion->p-**)
  (clrhash *pair->obs*)
  (clrhash *products*)
  (clrhash *product->other-products-and-obs*)
  (setf *reactions* nil)
  (loop for (down across obs) in *tables*
	do 
	(loop for (c1 a1) in down
	      as dobs in obs
	      do (loop for (c2 a2) in across
		       as ob in dobs
		       do
		       (let* ((c1 (clear c1))
			      (a1 (clear a1))
			      (c2 (clear c2))
			      (a2 (clear a2))
			      (r1 (cons c1 a1))
			      (r2 (cons c2 a2))
			      (p1 (cons c1 a2))
			      (p2 (cons c2 a1))
			      (r (list (list r1 r2) (list p1 p2))))
			 (push ob (gethash c1 *ion->p-**))
			 (push ob (gethash c2 *ion->p-**))
			 (push ob (gethash a1 *ion->p-**))
			 (push ob (gethash a2 *ion->p-**))
			 (push (list r ob) *reactions*)
			 (push ob (gethash p1 *pair->obs*))
			 (push ob (gethash p2 *pair->obs*))
			 (pushnew ob (gethash (list p1 p2) *products*))
			 (pushnew (list p2 ob) (gethash p1 *product->other-products-and-obs*))
			 (pushnew (list p1 ob) (gethash p2 *product->other-products-and-obs*))
			 )))))

(defun fr (p1 &optional p2)
  (loop for ((l r) ob) in *reactions*
	do (if (or (member p1 l :test #'equal) (member p1 r :test #'equal))
	       (if p2
		   (if (or (member p2 l :test #'equal) (member p2 r :test #'equal))
		       (print (list l r ob)))
		   (print (list l r ob))))))

(defun frp (p1 &optional p2)
  (loop for ot in '(P -)
	do
	(loop for ((l r) ob) in *reactions*
	      when (eq ob ot)
	      do (if (or (member p1 r :test #'equal))
		     (if p2
			 (if (or (member p2 r :test #'equal))
			     (print (list l r ob)))
			 (print (list l r ob)))))))
		       
(defvar *ion->an+sol* (make-hash-table :test #'equal))

(defun creport ()
  (clrhash *ion->an+sol*)
  (format t "~%~%================ Independent product apparent solubilities:~%")
  (loop for pair being the hash-keys of *pair->obs*
	using (hash-value obs)
	do (print (list pair obs))
	(push (cons (cdr pair) obs) (gethash (car pair) *ion->an+sol*))
	(push (cons (car pair) obs) (gethash (cdr pair) *ion->an+sol*))
	)
  (loop for ion being the hash-keys of *ion->an+sol*
	using (hash-value v)
	do
	(print (list ion v)))
  )

(defun rreport ()
  (format t "~%~%================ Reactions (paired products) observations:~%")
  (loop for pair being the hash-keys of *products*
	using (hash-value obs)
	do (print (list pair obs))))

(defun rxreport ()
  (format t "~%~%================ Pairs in the reactions table that are listed both ways:~%")
  (loop for pair being the hash-keys of *products*
	using (hash-value obs)
	if (gethash (list (second pair) (first pair)) *products*)
	do (print pair)))
	
(defun preport ()
  (format t "~%~%================ Pairs -> all other products:~%")
  (loop for pair being the hash-keys of *product->other-products-and-obs*
	using (hash-value op+ob)
	do (print (list pair op+ob)))
  (format t "~%~%         ================ Only -:~%")
  (loop for pair being the hash-keys of *product->other-products-and-obs*
	using (hash-value op+ob)
	unless (member 'p (mapcar #'second op+ob))
	do (print (list pair op+ob)))
  (format t "~%~%         ================ Only p:~%")
  (loop for pair being the hash-keys of *product->other-products-and-obs*
	using (hash-value op+ob)
	unless (member '- (mapcar #'second op+ob))
	do (print (list pair op+ob)))
  (format t "~%~%         ================ Both p and -:~%")
  (loop for pair being the hash-keys of *product->other-products-and-obs*
	using (hash-value op+ob)
	when (and (member '- (mapcar #'second op+ob)) (member 'p (mapcar #'second op+ob)))
	do (print (list pair op+ob)))
  )
  
(defun clear (ion)
  (cond ((atom ion) ion)
	(t (car ion))))

(defun find-first-solubles ()
  (format t "~%~%================ First soluables:~%")
  (loop for pair being the hash-keys of *pair->obs*
	using (hash-value obs)
	if (member '- obs)
	collect pair))

(defvar *first-solubles* nil)
(defvar *second-solubles* nil)

(setf *print-pretty* nil)
(setf *print-right-margin* nil)

(defun ireport ()
  (format t "~%~%================ Ion solubilities:~%")
  (mapcar #'print
	  (sort (loop for ion being the hash-keys of *ion->p-**
		      using (hash-value obs)
		      do (print (list ion obs))
		      collect (list ion :p%= (* 100.0 (/ (count 'p obs) (length obs)))))
		#'> :key #'third)))

;; (defun irreport ()
;;   (format t "~%~%================ Pair solubilities:~%")
;;   (clrhash 
;;   (loop for (l r ob) in *reactions*
	
;;   (mapcar #'print
;; 	  (sort 

;; 		      collect (list ion :p%= (* 100.0 (/ (count 'p obs) (length obs)))))
;; 		#'> :key #'third)))


(defun find-second-solubles (first-solubles)
  (format t "~%~%================ Second soluables:~%")
  (loop for reaction being the hash-keys of *products*
	using (hash-value ob)
	with ilist = nil
	with clist = nil
	do
	(if (and (equal '(p) ob)
		 (or (member (first reaction) first-solubles :test #'equal)
		     (member (second reaction) first-solubles :test #'equal)))
	    (let ((r (set-difference reaction first-solubles :test #'equal)))
	      (if r (pushnew r ilist :test #'equal)
		  (pushnew reaction clist :test #'equal))))
	finally
	(print "----------------INSOL:")
	(mapcar #'print ilist)
	(print "----------------CONF:")
	(mapcar #'print clist)
	))
	 
(defun run ()
  (crecord)
  (creport)
  (rreport)
  (rxreport)
  (preport)
  (mapcar #'print (setf *first-solubles* (find-first-solubles)))
  (mapcar #'print (setf *second-solubles* (find-second-solubles *first-solubles*)))
  (ireport)
  )

(run)
