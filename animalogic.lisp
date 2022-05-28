; (load (compile-file "animalogic.lisp"))

;;; This plays a game that Leo got that has four rows of
;;; animals. There are four types of animals in four colors. The
;;; constraint is that each row is a stack, so you have to clear the
;;; board by only pops, and of course, you can only pop the same color
;;; or the same type of animal.

;;; First is the color, second is the animal. Only the top animal can
;;; be removed (popped). (Row order turns out not to matter.)

(defparameter *maps* 
  '(
    (1
     ((g g) (y g) (b g) (r g))
     ((g l) (y l) (b l) (r l))
     ((g h) (y h) (b h) (r h))
     ((g c) (y c) (b c) (r c)))
    (49 
     ((y c) (y g) (r g) (r l))
     ((g g) (y l) (g c) (y h))
     ((r c) (b g) (b c) (b h))
     ((r h) (g h) (g l) (b l)))
    (60
     ((b c) (r g) (b g) (y g))
     ((g l) (y l) (g g) (r l))
     ((b l) (b h) (y h) (y c))
     ((g h) (r h) (r c) (g c)))
    ))

;;; Take an animal, and the map, and return a list of all maps that
;;; are possible with a compatible animal popped, along with the
;;; animal that gets popped. If there are not possibilities, this will
;;; return nil.

(defun all-compatible-new-maps (animal map)
  (loop for row in map
	as top-animal = (first row)
	if (compatible? animal top-animal)
	collect (list top-animal (remove-animal-from-map top-animal map))))

(defun compatible? (a1 a2)
  (or (eq (first a1) (first a2))
      (eq (second a1) (second a2))))

(defun remove-animal-from-map (animal map)
  (loop for row in map
	collect (if (equal (car row) animal)
		    (cdr row)
		  row)))

(defun null-map? (map) (equal '(()()()()) map))

(defvar *s* 0)
(defvar *f* 0)
(defvar *ss* nil)

(defun play (mapno)
  (format t "~%--------- ~a ---------~%" mapno)
  (setf *s* 0 *f* 0 *ss* nil)
  (let ((map (cdr (assoc mapno *maps*))))
    (loop for row in map ;; Start out trying each top animal
	  as first-animal = (first row)
	  collect (play-with-first-animal first-animal (remove-animal-from-map first-animal map) (list first-animal))))
  (format t "~a successes, ~a failures, s/f ratio: ~a~%" *s* *f* (if (zerop *f*) "nan" (/ (float *s*) *f*)))
  (mapcar #'pprint *ss*)
  )

(defun play-with-first-animal (animal map stack)
  (cond ((null-map? map)
	 (let ((stack (reverse stack)))
	   (push stack *ss*)
	   (incf *s*)))
	(t (let ((next-steps (all-compatible-new-maps animal map)))
	     (if (null next-steps)
		 (progn (incf *f*)) ;; Fail
	       (loop for (next-animal new-map) in next-steps
		     collect (play-with-first-animal next-animal new-map (cons next-animal stack))))))))

(play 1)
(play 49)
(play 60)
