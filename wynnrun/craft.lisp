;;; (load "craft.lisp")

;;; To Do:
;;; Change all strings to symbols

(defparameter *n-tries* 100000)

;;; To trace change (setf *trace?* nil)
(defparameter *trace?* nil)

(defparameter *accessoryTypes* '("ring" "bracelet" "necklace"))
(defparameter *atkSpds* '("SLOW" "NORMAL" "FAST"))
(defparameter *skp_order* '("str" "dex" "int" "def" "agi"))
(defparameter *skp_elements* '("e" "t" "w" "f" "a"))
(defparameter *weaponTypes* '("wand" "spear" "bow" "dagger" "relik"))
(defparameter *armorTypes* '("helmet" "chestplate" "leggings" "boots"))
(defparameter *consumableTypes* '("potion" "scroll" "food"))
(defparameter *rolledIDs*
  '("hprPct" "mr" "sdPct" "mdPct" "ls" "ms" "xpb" "lb" "ref" "thorns" "expd"
    "spd" "atkTier" "poison" "hpBonus" "spRegen" "eSteal" "hprRaw" "sdRaw" "mdRaw"
    "fDamPct" "wDamPct" "aDamPct" "tDamPct" "eDamPct" "fDefPct" "wDefPct" "aDefPct" "tDefPct"
    "eDefPct" "spPct1" "spRaw1" "spPct2" "spRaw2" "spPct3" "spRaw3" "spPct4"
    "spRaw4" "rainbowRaw" "sprint" "sprintReg" "jh" "lq" "gXp" "gSpd"))

(defparameter *powderstats*
  '(
    (3 6 17 2 1)  (5 8 21 4 2)  (6 10 25 8 3)  (7 10 31 14 5)  (9 11 38 22 9)  (11 13 46 30 13) 
    (1 8 9 3 1)  (1 12 11 5 1)  (2 15 13 9 2)  (3 15 17 14 4)  (4 17 22 20 7)  (5 20 28 28 10) 
    (3 4 13 3 1)  (4 6 15 6 1)  (5 8 17 11 2)  (6 8 21 18 4)  (7 10 26 28 7)  (9 11 32 40 10) 
    (2 5 14 3 1)  (4 8 16 5 2)  (5 9 19 9 3)  (6 9 24 16 5)  (8 10 30 25 9)  (10 12 37 36 13) 
    (2 6 11 3 1)  (3 10 14 6 2)  (4 11 17 10 3)  (5 11 22 16 5)  (7 12 28 24 9)  (8 14 35 34 13)
    ))

(defparameter *recipetypes*
  '("helmet" "chestplate" "leggings" "boots" "relik" "wand" "spear"
    "dagger" "bow" "ring" "necklace" "bracelet" "scroll" "food" "potion"))

(defun s+ (&rest args)
  (apply #'concatenate 'string (mapcar #'(lambda (a) (format nil "~a" a)) args)))

(defparameter *leveltypes*
  '("1-3" "3-5" "5-7" "7-9" "10-13" "13-15" "15-17" "17-19" "20-23" "23-25" "25-27"
    "27-29" "30-33" "33-35" "35-37" "37-39" "40-43" "43-45" "45-47" "47-49" "50-53"
    "53-55" "55-57" "57-59" "60-63" "63-65" "65-67" "67-69" "70-73" "73-75" "75-77"
    "77-79" "80-83" "83-85" "85-87" "87-89" "90-93" "93-95" "95-97" "97-99" "100-103" "103-105"))

;;; creates a crafted item object.

(defstruct craft
  recipe
  mat_tiers
  ingreds
  statmap
  atkspd
  flat-eff-mult
  )

(defun new-alist () (list (list :alist)))

(defun gget (map key &key (allow-nil? t) (default :no-default))
  "Works on either our headed alists or standard plists"
  (let ((v 
	 (let ((v 
		(if (atom (car map))
		    ;;(getf map key) Unfort. getf doesn't allow :test
		    (loop for (ikey val) on map by #'cddr
			  when (string-equal key ikey)
			  do (return val))
		    (cdr (assoc key map :test #'string-equal)))))
	   (or v (if allow-nil? nil
		     (if (eq default :no-default)
			 (error "(gget ~a ~a) got a nil, but allow-nil? was nil, and no default was provided!" map key)
			 default))))))
    (when *trace?* (format t "~s <- ~s~%" v key))
    v))

(defun do-crafting (recipe mat_tiers ingreds &optional atkspd)
 "constructs a craft.
    @param recipe: helmet-1-3 (id) etc a recipe object.
    @param mat_tiers: (1->3 1->3) an array with 2 numbers.
    @param ingreds: (). an array with 6 entries each with an ingredient map."
 (let* ((statmap (new-alist)) ;; statmap is an alist this is the fixed header
	(craft 
	 (make-craft
	  :recipe recipe
	  :mat_tiers mat_tiers
	  :ingreds ingreds
	  :statmap statmap
	  :atkspd atkspd)))
   (initcraftstats craft)
   craft))

(defun aset (alist key newval)
  "Setf or adds values to our spealized headed alists"
  (let ((curval (assoc key alist :test #'string-equal))) ;; looks for strings or keys
    (if curval (setf (cdr curval) newval)
	(setf (cdr alist) (cons (cons key newval) (cdr alist)))))
  (when *trace?* (format t "~a -> ~a~%" newval key))
  newval)

(defun member? (what in)
  (member what in :test #'string-equal))

(defmacro stenth (key nth newval) ;; Set STATMAP key entry value nth subvalue
  `(let ((**vs (gget statmap ,key)))
     (setf (nth ,nth **vs) ,newval)))

;;; This is very confusing because of the double indexing. The
;;; incoming array is ((a b) (c d) (e f)) and you get an index where
;;; the first (j) is the top level and the second (k) is within the
;;; sublevel, so 1 0 is c and 0 1 is a

(defmacro inceff (j k value)
  `(let ((eff (nth ,j eff)))
     (incf (nth ,k eff) ,value)))

(defun join- (words)
  (format nil "~{~a~^-~}" words))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defmacro setf! (&rest args) ;; Reporting version of setf
  `(setf 
    ,@(loop for (var val) on args by #'cddr
	    append `(,var (let ((**val ,val))
			    (when *trace?* (format t "~a := ~a~%" ',var **val))
			       **val)))))

(defun chapter (s) (when *trace?* (format t "~%~%========== ~a ==========~%~%" s)))

(defun initcraftstats (craft
		       &aux
			 (recipe (craft-recipe craft))
			 (mat_tiers (craft-mat_tiers craft))
			 (ingreds (craft-ingreds craft))
			 (statmap (craft-statmap craft))
			 (atkspd (craft-atkspd craft))
			 allnone
			 ratio
			 matmult
			 tiertomult
			 tiers
			 amounts
			 low low1 low2
			 high high1 high2
			 ndambaselow
			 ndambasehigh
			 elemdambaselow
			 elemdambasehigh
			 eff eff_flat
			 maxrolls minrolls
			 skillpoints reqs
			 )

  (chapter "A")

  ;;; WWW WATCH OUT FOR CONSTANTS THAT WILL RETAIN SIDE-EFFECTS !!!
  
  "get all stats for this build. stores in this.statmap.
    @pre the craft itself should be valid. no checking of validity of pieces is done here."
  (aset statmap "minrolls" (setf minrolls (new-alist)))
  (aset statmap "maxrolls" (setf maxrolls (new-alist)))
  (aset statmap "name" "hash???????????????")
  (aset statmap "displayname" "hash????????????????")
  (aset statmap "tier" "crafted")
  (aset statmap "type" (string-downcase (gget recipe "type")))
  ;; WWW "dura" can mean EITHER durability or duration!  There's
  ;; special handling for dura throughout !!!
  (aset statmap "duration" (list (or (first (gget recipe "duration")) 0) (or (second (gget recipe "duration")) 0))) ;;(low high)
  (aset statmap "durability" (list (or (first (gget recipe "durability")) 0) (or (second (gget recipe "durability")) 0)))
  ;; ??? The recipe objects seem to have "level" but not "level" so I
  ;; fixed the following 2 lines!!!  Also, the structure expectation
  ;; of level seems to be messed up. It thinks it's a list whereas it
  ;; a plist, so I fixed it in the recipe to be what the code seems to
  ;; expect
  (aset statmap "level" (second (gget recipe "level")))
  (aset statmap "levellow" (first (gget recipe "level")))
  (aset statmap "ndam" 0)
  (aset statmap "hp" 0)
  (aset statmap "hplow" 0)

  (chapter "B")

  (loop for e in *skp_elements*
	do
	(aset statmap (s+ e "dam") "0-0")
	(aset statmap (s+ e "def") 0))
  (loop for e in *skp_order*
	do
	(aset statmap (s+ e "req") 0)
	(aset statmap e 0))
  (setf allnone t)
  (if (or (member? (gget statmap "type") *armortypes*)
	  (member? (gget statmap "type") *weapontypes*))
      (progn 
	(aset statmap "category" "weapon")
	(if (< (first (gget recipe "level")) 30)
            (aset statmap "slots" 1)
	    (if (< (first (gget recipe "level")) 70)
		(aset statmap "slots" 2)
		(aset statmap "slots" 3))))
      (aset statmap "slots" 0))

  (chapter "C")

  (if (member? (gget statmap "type") *consumabletypes*)
      (progn
	(aset statmap "category" "consumable")
	(if (< (first (gget recipe "level")) 30)
            (aset statmap "charges" 1)
	    (if (< (first (gget recipe "level")) 70)
		(aset statmap "charges" 2)
		(aset statmap "charges" 3)
		))
      
	(chapter "D")
      
	;; Check that there are 6 ingredients -- they can be null
      
	(unless (= (length ingreds) 6)
	  (error "There must be 6 ingredients. If one is no included it must be nil."))

	;;no ingredient consumables always have 3 charges.
      
	(loop for ingred in ingreds
	      if (not (null ingred))
              do (setf allnone nil))

	(when *trace?* (format t "@A:Allnone = ~a~%" allnone))

	(if allnone
	    (progn 
	      (aset statmap "charges" 3)
	      (aset statmap "hp" (join- (gget recipe "healthordamage")))
              (aset statmap "duration" (gget recipe "basicduration")))
	    (aset statmap "category" "consumable"))
	)
      (aset statmap "charges" 0)
      )

  (chapter "E")

  (if (member? (gget statmap "type") *armortypes*)
      (progn 
	(aset statmap "hp" (join- (gget recipe "healthordamage")))
	(aset statmap "category" "armor"))
      (if (member? (gget statmap "type") *weapontypes*)
	  (progn 
	    (aset statmap "ndam" (join- (gget recipe "healthordamage")))
	    (loop for e in *skp_elements*
		  do
		  (aset statmap (s+ e "dam") "0-0")
		  (aset statmap (s+ e "damlow") "0-0")
		  )
	    (aset statmap "category" "weapon")
	    (aset statmap "atkspd" atkspd) ;; ??? Need to pull this from structure ???
	    )))
  (if (member? (gget statmap "type") *accessorytypes*)
      (aset statmap "category" "accessory"))
  (aset statmap "powders" ())
    
  (chapter "F")

  #| change certain ids based on material tier. 
  healthordamage changes.		;
  duration and durability change. (but not basicduration) ;
  |#
  
  ;;(print '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>) (terpri) (setf *trace?* t)

  (setf! matmult 1
	 tiertomult '(0 1 1.25 1.4)
	 tiers (copy-list mat_tiers)
	 amounts (loop for x in (gget recipe "materials") collect (gget x "amount"))
	 low (first (gget recipe "healthordamage"))
	 high (second (gget recipe "healthordamage"))
	 ;;mat multipliers - should work!
	 matmult (/ (+ (* (nth (first tiers) tiertomult) (first amounts))
		       (* (nth (second tiers) tiertomult) (second amounts)))
		    (+ (first amounts) (second amounts)))
	 )

  ;;(print '<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<) (terpri) (setf *trace?* nil)

  (chapter "CATEGORY ANALYSIS")

  ;; CATEGORY ANALYSIS

  (if (string-equal (gget statmap "category") "consumable")
      (if allnone
          (aset statmap "hp" (s+ (floor (* low matmult)) "-" (floor (* high matmult))))
	  (aset statmap "duration" (list (round (* (first (gget statmap "duration")) matmult))
					 (round (* (second (gget statmap "duration")) matmult)))))
      ;;durability modifier
      (aset statmap "durability" (list (round (* (first (gget statmap "durability")) matmult))
				       (round (* (second (gget statmap "durability")) matmult))))
      )

  (chapter "CA-1")

  (if (string-equal (gget statmap "category") "weapon")
      (progn 
	;;attack damages oh boy
	(setf ratio 2.05)
	(case (read-from-string atkspd)
	  (slow (setf ratio (/ ratio 1.5)))
	  (normal (setf ratio 1))
	  (fast (setf ratio (/ ratio 2.5))))

	(setf! ndambaselow (floor (* low matmult))
	       ndambasehigh (floor (* high matmult))
	       ndambaselow (floor (* ndambaselow ratio))
	       ndambasehigh (floor (* ndambasehigh ratio))
	       elemdambaselow (list 0 0 0 0 0)
	       elemdambasehigh (list 0 0 0 0 0))

	;; the powders part seems never to have been completed in the
	;; original code. it looks like it was just stubbed.

	(chapter "CA-2")

	(let ((powders nil))
	  (loop for ingred in ingreds
		if (and ingred (gget ingred "ispowder"))
		do (push (gget ingred "pid") powders))
	  (aset statmap "ingredpowders" powders))
      
	;; i create a separate variable for each low damage range because we
	;; need one damage range to calculate damage with and it's custom to
	;; use the maximum range of the range range.

	(setf low1 (floor (* ndambaselow 0.9)))
	(setf low2 (floor (* ndambaselow 1.1)))
	(setf high1 (floor (* ndambasehigh 0.9)))
	(setf high2 (floor (* ndambasehigh 1.1)))
	(aset statmap "ndambaselow" ndambaselow)
	(aset statmap "ndambasehigh" ndambasehigh)
	(aset statmap "ndamlow" (s+ low1 "-" low2))
	(aset statmap "ndam" (s+ high1 "-" high2))
	(loop for elt in *skp_elements*
	      as e from 0 by 1
	      do 
              (aset statmap (s+ (nth e *skp_elements*) "dambaselow") (nth e elemdambaselow))
              (aset statmap (S+ (nth e *skp_elements*) "dambasehigh") (nth e elemdambasehigh))
              (setf low1 (floor (* (nth e elemdambaselow) 0.9)))
              (setf low2 (floor (* (nth e elemdambaselow) 1.1)))
              (setf high1 (floor (* (nth e elemdambasehigh) 0.9)))
              (setf high2 (floor (* (nth e elemdambasehigh) 1.1)))
              (aset statmap (s+ (nth e *skp_elements*) "damlow") (s+ low1 "-" low2))
              (aset statmap (s+ (nth e *skp_elements*) "dam") (s+ high1 "-" high2))
	      )
	))

  (chapter "CA-3")

  (if (string-equal (gget statmap "category") "armor")
      (progn 
	(setf low (floor (* low matmult)))
	(setf high (floor (* high matmult)))
	(aset statmap "hp" high)
	(aset statmap "hplow" low)
	))

  #|
  ;; ??????????????????????????? POWDER MESS ??????????????????????????????? ;
  (if (or (string-equal (gget statmap "category") "armor") ;
  (string-equal (gget statmap "category") "accessory")) ;
  (loop for ingred in ingreds		;
  do (if (gget ingred "ispowder")	;
  (progn				;
  (setf powder (gget powderstats (getget ingred "pid"))) ;
  (setf name (gget powdernames (gget ingred "pid"))) ;
  (aset statmap (s+ (first name.charat) "def") ;
  (list (s+ (gget statmap (or (s+ (first name.charat) "def") 0) + powder("defplus"))))) ;
  (aset statmap *skp_elements*((skp_elements.indexof((first name.charat)) + 4 )% 5) + "def" ((gget statmap skp_elements((skp_elements.indexof((first name.charat)) + 4 )% 5)+"def") || 0) - powder("defminus")))))) ;
  |#
		     
  (chapter "END CATEGORY ANALYSIS")
  ;; END CATEGORY ANALYSIS
    
  (chapter "APPLY INGREDIENTS")
  (chapter "AI-0: Position Mods for all ingreds")

  ;; Adjust the ingredient effectiveness by position (posmods) (This
  ;; is SOOOOOOO OBSCURE! I had to check this by pulling out the js
  ;; code and running it separately!)

  (setf eff (copy-tree `((100 100) (100 100) (100 100))))
  (loop for ingred in ingreds
	as n from 0 by 1
	;;i and j will refer to the eff matrix.
	as i = (floor (/ n 2))
	as j = (mod n 2)
	when ingred ;; ??? This isn't in the original code but what happens with null ingreds there ???
	do
	;;(format t "Computing eff mods for ~a~%  posmods = ~a~%  ingoing eff = ~a~%"
	;;(gget ingred "name")  (gget ingred "posmods") eff)
	;;(format t "  n = ~a, i = ~a, j = ~a~%" n i j)
	(loop for (key value) on (gget ingred "posmods") by #'cddr
	      unless (zerop value)
	      do
	      ;; Remember: If the incoming array is ((a b) (c d) (e
	      ;; f)) and you get an index where the first index is the
	      ;; top level and the second is within the sublevel, so 1
	      ;; 0 is c and 0 1 is a. In js this would be eff[top][below]
	      (case key
		(above (loop for k from (1- i) downto 0 do (inceff k j value)))
		(under (loop for k from (1+ i) to 2 by 1 do (inceff k j value)))
		(left (if (= j 1) (inceff i (1- j) value)))
		(right (if (= j 0) (inceff i (1+ j) value)))
		(touching (loop for ktheff in eff
				as k from 0 by 1
				do (loop for lthsubeff in ktheff
					 as l from 0 by 1
					 do (if (or (and (= (abs (- k i)) 1)
							 (= (abs (- l j)) 0))
						    (and (= (abs (- k i)) 0)
							 (= (abs (- l j)) 1)))
						(inceff k l value)))))
		(nottouching (loop for ktheff in eff
				   as k from 0 by 1
				   do (loop for lthsubeff in ktheff
					    as l from 0 by 1
					    do (if (or (> (abs(- k i)) 1)
						       (and (= (abs (- k i)) 1)
							    (= (abs (- l j)) 1)))
						   (inceff k l value)))))
		(t (break "Fell through above, under, ... key stack!")))
	      ;;(print eff) (terpri)
	      ))

  (when *trace?* (format t "Finally eff = ~a~%" eff))

  ;;apply ingredient effectivness - on ids and reqs (itemids). not on durability duration or charges.

  (setf eff_flat (flatten eff))
  (aset statmap "ingredeffectiveness" eff_flat)
  (setf (craft-flat-eff-mult craft) eff_flat)
  ;;apply ingredient ids

  (chapter "AI Main Ingredient Loop")

  (loop for ingred in ingreds
	as n from 0 by 1
	as eff_mult = (/ (nth n eff_flat) 100.0) ;; tofixed(2) FFF
	when ingred
	do

	(when *trace?* (format t "~%************************************
* Processing ingred: ~a (eff_mult=~a)
************************************~%" (gget ingred "name") eff_mult))

	;;; WWW "dura" can mean EITHER durability or duration! WWW !!!

	(chapter "AI-1: Durability (dura meaning #1)")

	(loop for (key value) on (gget ingred "itemids") by #'cddr
	      do (if (and (not (string-equal key "dura"))
			  (not (member? (gget statmap "type") *consumabletypes*))) ;;consumables never get reqs
		     (if (not (gget ingred "ispowder"))
			 (aset statmap key (round (+ (gget statmap key :allow-nil? nil :default 0) (* value eff_mult))))
			 (aset statmap key (round (+ (gget statmap key :allow-nil? nil :default 0) value))))
		     (aset statmap "durability"
			   (loop for x in (gget statmap "durability")
				 collect (+ x value))
			   )))

	(chapter "AI-2: Duration (also dura (meaning #2)!)")

	(loop for (key value) on (gget ingred "consumableids") by #'cddr
	      ;;neither duration nor charges are affected by effectiveness
	      do
	      (gget statmap "duration" :allow-nil? nil :default '(0 0))
	      (if (string-equal key "dura")
		     (aset statmap "duration" (loop for x in (gget statmap "duration" :allow-nil? nil :default '(0 0))
						    collect (+ x value)))
		     (aset statmap key (+ (gget statmap "charges") value))))

	(chapter "AI-3: Assigning minrolls and maxrolls")

	;; The original code here was for what seems like it was an
	;; old version of the data structure where the maxrolls and
	;; minrolls were separate, but now they are combined in the
	;; ids in a much more efficient manner, so this doesn't look
	;; much like the js code. The ids looks like this: (wDefPct
	;; (-10 -8) poison ( 90 100)), where for example poison is
	;; the roll, and 90 is the min and 100 is the max for that
	;; roll. What we want is for maxrolls and minrolls to be
	;; alists where the max and min as above are set for each
	;; roll, and where they are not set, they are set each to
	;; zero. If there's a previous value, you add them.

	(loop for (roll (newmin newmax)) on (gget ingred "ids") by #'cddr
	      as oldmin = (gget minrolls roll)
	      if (null oldmin) ;; Maybe FFF error check various fups
	      do 
	      (aset minrolls roll (floor (* eff_mult newmin)))
	      (aset maxrolls roll (floor (* eff_mult newmax))) ;; This doesn't look like it's in the .js code???
	      else
	      do 
	      (aset minrolls roll (+ oldmin (* eff_mult newmin)))
	      (aset maxrolls roll (+ (gget maxrolls roll) (* eff_mult newmax)))
	      )

	(when *trace?* (format t "~%~%---~%Minrolls = ~a~%Maxrolls = ~a~%---~%~%" minrolls maxrolls))

	(chapter "AI-4")

	(loop for dur in (gget statmap "durability")
	      as d from 0 by 1
	      do (if (< dur 1)
		     (stenth "durability" d 1)
		     (stenth "durability" d dur)))

	(chapter "AI-5")

	(loop for curdur in (gget statmap "duration")
	      as d from 0 by 1
	      do (if (and (not allnone) curdur)
		     (stenth "duration" d (if (< curdur 10) 10 curdur))))

	(if (and (gget statmap "charges")
		 (< (gget statmap "charges") 1))
	    (aset statmap "charges" 1))
	  
	(chapter "AI-6: Skill points")

	;; This is really hard to understand (and I'm sure that it's
	;; being done in the most complex possible way!)

	(setf reqs (list 0 0 0 0 0))
	(aset statmap "reqs" reqs)
	(setf skillpoints (list 0 0 0 0 0))
	(aset statmap "skillpoints" skillpoints)

	(loop for order in *skp_order*
	      as e from 0 by 1
	      with maxrolls = (gget statmap "maxrolls")
	      as mrskoval = (gget maxrolls order)
	      do 
	      (aset statmap order (or mrskoval 0))
	      (setf (nth e skillpoints) (or mrskoval 0))
	      (setf (nth e reqs)
		    (let* ((soreq (s+ order "req"))
			   (oreq (gget statmap soreq)))
		      (if (and oreq (not (member? (gget statmap "type") *consumabletypes*)))
			  (gget statmap soreq)
			  0)))
	      ))

  (chapter "AI-7: Set the value for any roll not already set above to 0.")

  (loop for roll in *rolledids*
	as min = (gget minrolls roll) ;; FFF maybe check both
	if (null min) 
	do
	(aset minrolls roll 0)
	(aset maxrolls roll 0)
	)



  ;; Mark done

  (chapter "DONE")
  (aset statmap "crafted" t)
  statmap
  )

(defun report-craft (craft)
  (let* ((recipe (craft-recipe craft))
	 (mat_tiers (craft-mat_tiers craft))
	 (ingreds (craft-ingreds craft))
	 (flat-eff-mult (craft-flat-eff-mult craft))
	 (statmap (craft-statmap craft))
	 (atkspd (craft-atkspd craft)))
    (format t "Recipe: ~s~%~%" Recipe)
    (format t "Mat_tiers: ~s~%~%" Mat_tiers)
    (format t "Ingreds: ~s~%~%" Ingreds)
    (format t "Atkspd: ~s~%~%" Atkspd)
    (format t "Flat-eff-mult: ~s~%~%" flat-eff-mult)
    (format t "Statmap:~%")
    (loop for (key . value)
	  in statmap
	  unless (or (member key '("maxrolls" "minrolls") :test #'string-equal)
		     (null value)
		     (and (numberp value) (zerop value))
		     (and (stringp value) (string-equal value "0-0")))
	  do (format t "  ~a = ~a~%" key value))
    (loop with minrolls = (gget statmap "minrolls")
	  for (key . max) in (gget statmap "maxrolls")
	  as min = (gget minrolls key)
	  when (and min max (not (and (zerop min) (zerop max))))
	  do (format t "  ~a : ~a - ~a~%" key min max))
    ))

;;; WWW The old format for min/max (for example in the ids) is
;;; (minimum ### maximum ###) which I convert everywhere to just (###
;;; ###). This needs to be either preprocessed out, or both
;;; possibliites must be embedded into the code.

;;; WWW I use ingredient position 

'(report-craft (do-crafting
		  '(level (80 83) type "DAGGER" skill "WEAPONSMITHING"
		    materials ((item "Refined Diamond Ingot" amount 10)
			       (item "Refined Pine Wood" amount 5))
		    healthOrDamage (192 198) durability (452 462) id "Dagger-80-83")
		'(2 2)
		'(
		  (name "Ancient Metal" tier 0 skills ("WEAPONSMITHING" "ARMOURING" "TAILORING")
		   lvl 64 ids (mdRaw (20 21))
		   itemids (dura -40 strReq 0 dexReq 0 intReq 0 defReq 0 agiReq 0) 
		   consumableIDs (charges 0 dura 0)
		   posMods (left 0 right 0 above 0 under 0 touching 0 notTouching 0) id 3)
		  (name "Elemental Crystal" tier 1 skills ("ARMOURING" "TAILORING" "WEAPONSMITHING" "WOODWORKING")
		   lvl 70 ids (ref (4 5) thorns (4 5) hpBonus (200 230) sdRaw (18 25) mdRaw (14 20))
		   itemids (dura -101 strReq 0 dexReq 0 intReq 0 defReq 0 agiReq 0) 
		   consumableIDs (charges 0 dura 0)
		   posMods (left 0 right 0 above 0 under 0 touching 0 notTouching 0) id 234)
		  (name "Aspect of the Void" tier 3 skills ("WEAPONSMITHING" "SCRIBING")
		   level 100 ids ()
		   itemids (dura -160 strReq 0 dexReq 0 intReq 0 defReq 0 agiReq 0 atkTier 0 slotMod 0) 
		   consumableIDs (charges 0 dura -275)
		   posMods (left 110 right 110 above 110 under 110 touching -110 notTouching 15) id 563)
		  (name "Expelled Shrapnel" tier 0 skills ("WEAPONSMITHING" "WOODWORKING")
		   lvl 105 ids (hpBonus (-275 -250) mdRaw (35 40))
		   itemids (dura -54 strReq 8 dexReq 8 intReq 0 defReq 0 agiReq 8) 
		   consumableIDs (charges 0 dura 0)
		   posMods (left 0 right 0 above 0 under 0 touching 0 notTouching 0) id 50)
		  nil
		  nil
		  )
		"normal"))

'(report-craft (do-crafting
		  '(level (80 83) type "DAGGER" skill "WEAPONSMITHING"
		    materials ((item "Refined Diamond Ingot" amount 10)
			       (item "Refined Pine Wood" amount 5))
		    healthOrDamage (192 198) durability (452 462) id "Dagger-80-83")
		'(2 2)
		'(
		  (name "Ancient Metal" tier 0 skills ("WEAPONSMITHING" "ARMOURING" "TAILORING")
		   lvl 64 ids (mdRaw (20 21))
		   itemids (dura -40 strReq 0 dexReq 0 intReq 0 defReq 0 agiReq 0) 
		   consumableIDs (charges 0 dura 0)
		   posMods (left 0 right 0 above 0 under 0 touching 0 notTouching 0) id 3)
		  (name "Elemental Crystal" tier 1 skills ("ARMOURING" "TAILORING" "WEAPONSMITHING" "WOODWORKING")
		   lvl 70 ids (ref (4 5) thorns (4 5) hpBonus (200 230) sdRaw (18 25) mdRaw (14 20))
		   itemids (dura -101 strReq 0 dexReq 0 intReq 0 defReq 0 agiReq 0) 
		   consumableIDs (charges 0 dura 0)
		   posMods (left 0 right 0 above 0 under 0 touching 0 notTouching 0) id 234)
		  (name "Expelled Shrapnel" tier 0 skills ("WEAPONSMITHING" "WOODWORKING")
		   lvl 105 ids (hpBonus (-275 -250) mdRaw (35 40))
		   itemids (dura -54 strReq 8 dexReq 8 intReq 0 defReq 0 agiReq 8) 
		   consumableIDs (charges 0 dura 0)
		   posMods (left 0 right 0 above 0 under 0 touching 0 notTouching 0) id 50)
		  nil
		  nil
		  (name "Aspect of the Void" tier 3 skills ("WEAPONSMITHING" "SCRIBING")
		   level 100 ids ()
		   itemids (dura -160 strReq 0 dexReq 0 intReq 0 defReq 0 agiReq 0 atkTier 0 slotMod 0) 
		   consumableIDs (charges 0 dura -275)
		   posMods (left 110 right 110 above 110 under 110 touching -110 notTouching 15) id 563)
		  )
		"normal"))

'(report-craft (do-crafting
		  '(level (80 83) type "DAGGER" skill "WEAPONSMITHING"
		    materials ((item "Refined Diamond Ingot" amount 10)
			       (item "Refined Pine Wood" amount 5))
		    healthOrDamage (192 198) durability (452 462) id "Dagger-80-83")
		'(3 1)
		'(
		  (name "Ancient Metal" tier 0 skills ("WEAPONSMITHING" "ARMOURING" "TAILORING")
		   lvl 64 ids (mdRaw (20 21))
		   itemids (dura -40 strReq 0 dexReq 0 intReq 0 defReq 0 agiReq 0) 
		   consumableIDs (charges 0 dura 0)
		   posMods (left 0 right 0 above 0 under 0 touching 0 notTouching 0) id 3)
		  (name "Elemental Crystal" tier 1 skills ("ARMOURING" "TAILORING" "WEAPONSMITHING" "WOODWORKING")
		   lvl 70 ids (ref (4 5) thorns (4 5) hpBonus (200 230) sdRaw (18 25) mdRaw (14 20))
		   itemids (dura -101 strReq 0 dexReq 0 intReq 0 defReq 0 agiReq 0) 
		   consumableIDs (charges 0 dura 0)
		   posMods (left 0 right 0 above 0 under 0 touching 0 notTouching 0) id 234)
		  (name "Expelled Shrapnel" tier 0 skills ("WEAPONSMITHING" "WOODWORKING")
		   lvl 105 ids (hpBonus (-275 -250) mdRaw (35 40))
		   itemids (dura -54 strReq 8 dexReq 8 intReq 0 defReq 0 agiReq 8) 
		   consumableIDs (charges 0 dura 0)
		   posMods (left 0 right 0 above 0 under 0 touching 0 notTouching 0) id 50)
		  nil
		  nil
		  (name "Aspect of the Void" tier 3 skills ("WEAPONSMITHING" "SCRIBING")
		   level 100 ids ()
		   itemids (dura -160 strReq 0 dexReq 0 intReq 0 defReq 0 agiReq 0 atkTier 0 slotMod 0) 
		   consumableIDs (charges 0 dura -275)
		   posMods (left 110 right 110 above 110 under 110 touching -110 notTouching 15) id 563)
		  )
		"fast"))

'(report-craft (do-crafting
		  '(LEVEL (100 103) TYPE "BOOTS" SKILL "TAILORING" MATERIALS
		    ((ITEM "Refined Voidstone Ingot" AMOUNT 6)
		     (ITEM "Refined Hemp String" AMOUNT 12))
		    HEALTHORDAMAGE (3250 3300) DURABILITY (522 525) ID "Boots-100-103")
		'(3 3)
		'((NAME "Horse Mane" TIER 1 SKILLS ("TAILORING" "SCRIBING") LEVEL 22
		   IDS (AGI (1 1) SPD (2 4)) ITEMIDS
		   (DURA -35 STRREQ 0 DEXREQ 0 INTREQ 0 DEFREQ 0 AGIREQ 0)
		   CONSUMABLEIDS (CHARGES 0 DURA -69) POSMODS
		   (LEFT 0 RIGHT 0 ABOVE 0 UNDER 0 TOUCHING 0 NOTTOUCHING 0) ID 255)
		  NIL
		  (NAME "Doom Stone" TIER 3 SKILLS ("TAILORING" "JEWELING") LEVEL 103
		   IDS NIL ITEMIDS
		   (DURA -160 STRREQ 0 DEXREQ 0 INTREQ 0 DEFREQ 0 AGIREQ 0)
		   CONSUMABLEIDS (CHARGES 0 DURA 0) POSMODS
		   (LEFT -100 RIGHT -100 ABOVE 60 UNDER 60 TOUCHING 0 NOTTOUCHING 0) ID
		   588)
		  (NAME "Incremental Mapping Module" TIER 3 SKILLS
		   ("ARMOURING" "TAILORING") LEVEL 90 IDS
		   (MR (6 6) SPD (16 20) HPBONUS (-750 -650)) ITEMIDS
		   (DURA -149 STRREQ 0 DEXREQ 0 INTREQ 0 DEFREQ 0 AGIREQ 60)
		   CONSUMABLEIDS (CHARGES 0 DURA 0) POSMODS
		   (LEFT 0 RIGHT 0 ABOVE 0 UNDER 0 TOUCHING 0 NOTTOUCHING 0) ID 605)
		  NIL NIL)
		"normal"))



(defparameter *recipes* nil)
(defparameter *ingreds* nil)
(defvar *n-ingreds* nil)

;;; These are VERY unhygienic macros...largely for the sake of
;;; efficiency they refer prolifically to terms defined in the LET* of
;;; score-craft. In order to localize that a bit, we create the
;;; stat-context macro which just does the let*, and this can be
;;; modified in a slightly localized way.

;;; Simplifies certain long scoring functions by replacing NaNs with 0s.

(defmacro stat-math (expr)
  (stat-math-tree-convert expr))

(defun stat-math-tree-convert (expr)
  (cond ((null expr) expr)
        ((numberp expr) expr) 
	((member expr '(+ - / * expt sqrt)) expr)
	((atom expr) `(if (numberp ,expr) ,expr 0))
	(:else (mapcar #'stat-math-tree-convert expr))))
  
(defmacro with-stat-context (craft skill score-expr)
  `(let* ((statmap (craft-statmap ,craft))
	  (maxrolls (gget statmap "maxrolls"))
	  (reqs (gget statmap "reqs"))
	  (walk-speed (gget maxrolls "spd"))
	  (spell-damage (gget maxrolls "sdam")) ;; There doesn't appear to be a property called "sdam" (or any other version thereof)???
	  (cube-root-durability (expt (second (gget statmap "durability")) 1/3))
	  (eDamPct (gget maxrolls "edampct"))
	  (stat-math-result (stat-math (+ spell-damage walk-speed)))
	  ;;(edam+wlkspd (+ (if (numberp edampct) edampct 0) (if (numberp walk-speed) walk-speed 0)))
	  )
     ,score-expr))
  
;;; So now these macros can refer to anything that's defined in
;;; WITH-STAT-CONTEXT as if these are globally-defined.

(defmacro neg-or-0 (thing)
  `(if (> ,thing 0) 0 ,thing))

(defmacro reqsums ()
  `(loop for req in reqs
	if (< req 0) sum 0
	else sum req))

;;; Finally, here are the top-level scoring functions. This can call
;;; on any of the above macros, and any simple math.

(defmacro stat-math+edampct*cube-root-durability-nomralized-by-one-tenth-sqrt-reqsums ()
  `(/ (* (+ stat-math-result (neg-or-0 edampct)) cube-root-durability)
      (+ 1.0 (/ (sqrt (reqsums)) 10.0))))

;;; So finally here's the scoring function.

(defun score-craft (craft skill)
  (with-stat-context craft skill (stat-math+edampct*cube-root-durability-nomralized-by-one-tenth-sqrt-reqsums)))

(defun find-a-random-compatible-ingredient (skill)
  (loop as i = (nth (random *n-ingreds*) *ingreds*)
	if (member skill (gget i "skills") :test #'string-equal)
	do (return i)))

(defun filter-ingreds (ingreds)
  (loop for ingred in ingreds
	if (acceptable-ingred ingred)
	collect ingred))

;; (or (> durability 0)
;;     (includes one or more of the stats being optimized) [spd]
;;     (has any effect on positional effectiveness (non-zero in any position))
;;     ))

(defun acceptable-ingred (ingred)
  (and (member? "jeweling" (gget ingred "skills"))
       (let ((ids (gget ingred "ids")))
	 (or (gget ids "sdam")
	     (gget ids "spd")
	     (> (gget (gget ingred "itemids") "dura") 0)
	     (loop for (pos value) on (gget ingred "posMods") by #'cddr
		   if (> value 0)
		   do (return t))))))

(defun run (&optional (ntries 10000))
  (load "recipes.lisp")
  (load "ingreds.lisp")
  (print (length (setf *ingreds* (filter-ingreds *ingreds*))))
  (setf *n-ingreds* (length *ingreds*))
  (loop for recipe+ in *recipes*
	as recipe = (car (gget recipe+ 'data))
	as skill = (read-from-string (gget recipe "skill"))
	if (and (equal '(100 103) (gget recipe "level"))
		(or (and (listp skill) (member? "jeweling" skill))
		    (string-equal skill "jeweling")))
	do (format t "~%~%==================================~%~s~%==================================~%~%" recipe)
	(loop for try below ntries
	      with top-score = -999
	      with top = nil
	      as mat_tiers = '(3 3) ;; (list (1+ (random 3)) (1+ (random 3)))
	      as ingreds = (loop for i below 6
			      collect (if (not (zerop (random 3)))
					  (find-a-random-compatible-ingredient skill)))
	      as atkspd = (nth (random 3) '("slow" "normal" "fast"))
	      as craft = (do-crafting recipe mat_tiers ingreds atkspd)
	      as score = (score-craft craft skill)
	      if (> score top-score)
	      do (setf top-score score top craft)
	      (format t "~a " score)
	      finally (progn (report-craft top)
			     (format t "Score = ~a~%" top-score)))))

(untrace)
;;;(trace score-craft)

(run *n-tries*)

