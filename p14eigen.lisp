;;; (load (compile-file "p14eigen.lisp"))
;;; ((7/3 -2/3) (-1/3 8/3)) <- correct!
;;; {{7/3,-2/3},{-1/3,8/3}}{{3},{0}}
;;; eigenvectors{{7/3,-2/3},{-1/3,8/3}} <- These are correct
;;; eigenvectors{{4/3,2/3},{1,2}} <- These are crazy
;;; {{2,1},{1,-1}}({{2,0},{0,3}}*({{2,1},{1,-1}}^-1)) <- But this creates the WRONG matrix!
;;; eigenvectors({{2,1},{1,-1}}({{2,0},{0,3}}*({{2,1},{1,-1}}^-1)))
;;; YOU HAVE TO DO THE MULTIPLICATION IN THE OTHER ORDER!!!!!!????????????
;;; BUT MM IS ASSOCIATIVE!!!!!!!!!!!?????????????
;;; {{2,1},{1,-1}}.{{2,0},{0,3}}.{{2,1},{1,-1}}^-1
;;; {{2,1},{1,-1}}({{2,0},{0,3}}{{2,1},{1,-1}}^-1)
;;; ({{2,1},{1,-1}}{{2,0},{0,3}}){{2,1},{1,-1}}^-1
;;; ({{2,1},{1,-1}}{{2,0},{0,3}})({{2,1},{1,-1}}^-1)
;;; ({{2,1},{1,-1}}*{{2,0},{0,3}})*({{2,1},{1,-1}}^-1)

;;; {{7/3,-2/3},{-1/3,8/3}}={{4/3,-2/3},{-1,2}}

(defun mvx (a v)
  (list (+ (* (car (first a)) (car v))
	   (* (cadr (first a)) (cadr v)))
	(+ (* (car (second a)) (car v))
	   (* (cadr (second a)) (cadr v)))))

(defun right? (a)
  (and
    (equal (mvx a '(3 0)) '(7 -1))
    (equal (mvx a '(4 -1)) '(10 -4))
    (equal (mvx a '(2 1)) '(4 2))
  ))

(defparameter *nl* -10)
(defparameter *nh* 10)
(defparameter *dl* 1)
(defparameter *dh* 5)

(defun run ()
  (loop for an from *nl* to *nh*
	do (loop for ad from *dl* to *dh*
		 do (loop for bn from *nl* to *nh*
		       do (loop for bd from *dl* to *dh*
				do (loop for cn from *nl* to *nh*
				      do (loop for cd from *dl* to *dh*
					       do (loop for dn from *nl* to *nh*
						     do (loop for dd from *dl* to *dh*
							      as a = (/ an ad)
							      as b = (/ bn bd)
							      as c = (/ cn cd)
							      as d = (/ dn dd)
							      as m = (list (list a b) (list c d))
							      if (right? m)
							      do (print m))))))))))

(untrace)
;(trace mvx)
(run)
