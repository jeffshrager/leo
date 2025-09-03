;;; (load (compile-file "earth.lisp"))

;;; Computes the weight of a simplified 2d earth where there are 5
;;; layers and each layer is broken into 12 30degree sectors beginning
;;; at 0deg and moving around in the usual way, so from logical due
;;; East to North -> West -> South -> and back to East. For each point
;;; we calculate the distance and direction of force from each other
;;; sector in the entire (simplified) earth, using a mass of 1 for
;;; each segment and a thickness of 1. Only the center of mass is
;;; computed (and we could improve the estimate, of course, but
;;; reducing the angular delta so that the CoM better and better
;;; approximated the piece.

(defparameter *n-elements* 12)
(defparameter *angular-delta* (/ (* pi 2) *n-elements*))

;; WWW There's going to be a real number problem here testing whether
;; we've reached the max angle because of pi. So I add a little slop
;; in the test to ensure that we don't go around again. This won't
;; matter at very high densities (numbers of elements).

(defparameter *max-angle* (+ 0.05 (- (* 2 pi) *angular-delta*))) ;; The slop is about 3 degrees (in rads) 
(defparameter *g* 6.7) ;; Actually 6.67E-11 M m2 kg-2
(defparameter *ring-thinkness* 1)
(defparameter *n-rings* 5) ;; Max radius of the earth will be this times ring-thickness
(defparameter *unit-mass* 1.0)
;;; FFF For the moment we assume that all units are the same arc size
;;; (point mass simplification) which is wrong except at the
;;; infinitesmial limit.
(defparameter *mass-squared-times-g* (* *unit-mass* *unit-mass* *g*))

;;; We weigh each element in two nested loops: One that goes around
;;; the ring, and one that moves from ring to ring. But the inner code
;;; has to run through the whole thing again in order to compute the
;;; weight of a singe element.

(defun weigh-the-earth () 
  (loop for ring from 1 to *n-rings*
	as radius = (* ring *ring-thinkness*)
	do (format t "~%----------------------- Ring: ~a (Radius: ~a) -----------------------~%" ring radius)
	sum (loop for angle from 0.0 to *max-angle* by *angular-delta*
		 sum (weigh-element radius angle))))

(defun weigh-element (from-radius from-angle)
  (loop for ring from 1 to *n-rings*
	as to-radius = (* ring *ring-thinkness*)
	do (format t "Ring ~a:~%" ring)
	sum (loop for to-angle from 0 to *max-angle* by *angular-delta*
		  sum (elt-to-elt-weight from-radius from-angle to-radius to-angle))))

;;; This is the meat of the computation. It return the length of the
;;; component of the graviational force vector between the target and
;;; the vector to the center of the earth (since our definitely of
;;; "weight" is that it's what a scale will read, and the scale is
;;; always pointing towards the center of the earth). There are
;;; various tricky parts to this, esp. the vector projection, and it's
;;; easy to get wrong, so there's also a test set.

;;; From- and To- are (respectively) the element we're trying to weigh
;;; (from-) and the element that it is being pulled to (to- ... of
;;; course they're technically being pulled toward one another, but
;;; each component of that will be separately computed). 

(defparameter *pi/180* (/ pi 180))

(defun nearly-zerop (n)
  (< (abs n) 0.0001))

(defun elt-to-elt-weight (from-radius from-angle to-radius to-angle)
  (let* ((dist (rdist from-angle from-radius to-angle to-radius))
	 (dang (abs (- to-angle from-angle))) ;; Angle between the points
	 ;; Note the projection should just be the length of the force
	 ;; vector times the cos of the angle between them, because
	 ;; ... right triangles, right?
	 (proj (* dist (cos dang))) ;; Can it really be this simple?
	 )
    ;; Here's the actual weight calculation. Recall that we're making
    ;; the point mass simplification.
    (let ((result (if (nearly-zerop proj) 0.0
		      (/ *mass-squared-times-g* (* proj proj)))))
      (format t "  FR= ~,2f, FA= ~,2f, TR= ~,2f, TA= ~,2f, dist= ~,2f, dang= ~,2f, proj = ~,2f, r= ~,2f~%"
	      from-radius from-angle to-radius to-angle dist dang proj result)
      result
      )))

;;; Distance is the square root of r1 squared plus r2 two squared
;;; minus two times r1 one r2 two times cos of theta-1 minus
;;; theta-two: d = sqrt(r1^2 + r2^2 - 2 * r1 * r2 * cos(θ2 - θ1))

(defun rdist (far from-radius tar to-radius)
  (sqrt (+ (* to-radius to-radius) (* from-radius from-radius)
		 (- (* 2 (* to-radius from-radius)
		       (cos (- tar far)))))))

;;; If this is working correctly, all the elements of a particular
;;; ring should have the exact same weight, and the weight should
;;; become less (approaching zero) as we get to the center. (Note that
;;; weigh-earth starts from the center and goes outward, so the inner
;;; elements should have very small weights, and increase as you go
;;; outward.

(untrace)
(trace weigh-element)
(weigh-the-earth)
