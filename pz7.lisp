;;; (load (compile-file "pz7.lisp"))

;;; https://fivethirtyeight.com/features/can-you-crack-the-safe/
;;; SOLVED CORRECTLY!! (In fact, I guessed the answer before even
;;; writing the program, but that's beacuse there's only one solution!

#|
    
     0
   5   1    
     6
   4   2
     3

Note that the 7-segment code is ambiguous! See:

   https://en.wikipedia.org/wiki/Seven-segment_display

"Alternate patterns: The numeral 1 may be represented with the left
segments, the numerals 6 and 9 may be represented without a 'tail',
and the numeral 7 represented with a 'tail'."

The version below matches the solution that the Riddler gave, although
you get a different answer when you make different choices foe the
display. 

|#


(defvar *models*
  '((0 0 1 2 3 4 5)
    (1 1 2)
    (2 0 1 6 4 3)
    (3 0 1 2 3 6)
    (4 1 2 5 6)
    (5 0 5 6 2 3)
    (6  2 3 4 5 6)
    (7 0 1 2 5)
    (8 0 1 2 3 4 5 6)
    (9 0 1 2 5 6)))

(defun encode (a b)
  (let* ((codes (append (cdr (assoc a *models*)) (cdr (assoc b *models*)))))
    (loop for n below 8
	  with sum = 0
	  do (incf sum (* (count n codes) (expt 3 n)))
	  finally (return sum))))

(defvar *code->pair* (make-hash-table :test #'equal))

(defun run ()
  (clrhash *code->pair*)
  (loop for a below 10
	do (loop for b below 10
		 do (push (cons a b) (gethash (encode a b) *code->pair*))))
  (loop for code being the hash-keys of *code->pair*
	using (hash-value pairs)
	when (> (length pairs) 2)
	do (print (list code pairs)))
  )

(run)
