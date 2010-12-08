(defconstant help-file "startrek.hlp")
(defconstant pict-file "startrek.pic")
(defconstant map-file "startrek.map")

(defconstant region-names
  (list "ANTARTES" "SIRIUS"
	"RIGEL" "DENEB"
	"POCYON" "CAPELLA"
	"VEGA" "BETELGEUSE"
	"CANAPUS" "ALDEBARAN"
	"ALTAIR" "REGULUS"
	"SAGITATTARIUS" "ARCTURUS"
	"POLLOX" "SPICA")
  "Names of the quadrants regions")

(defconstant quadrant-names (list "I" "II" "III" "IV"))

(defstruct sector
  "Structure describing the datat of each sector."
  (scannedp)
  (klingon-list)
  (starbase-list)
  (star-list))

(defvar quadrants (make-array '(8 8)))

(defstruct location
  (quad-x 0)
  (quad-y 0)
  (sect-x 0)
  (sect-y 0))

(defvar enterprise-location (make-location))

(defvar start-klingons 0)
(defvar total-klingons 0)
(defvar total-starbases 0)

(defconstant start-energy 3000)
(defconstant start-photon-torpedoes 10)
(defconstant min-klingon-shield-energy 100)
(defconstant max-klingon-shield-energy 300)
(defconstant min-klingon-phaser-fire 50)
(defconstant max-klingon-phaser-fire 200)
(defconstant max-stars-sector 9)
(defconstant min-klingons-galaxy 7)
(defconstant max-klingons-galaxy 28)
(defconstant max-starbases-galaxy 5)
(defconstant min-days-mission 15)
(defconstant max-days-mission 45)
(defconstant start-star-date 3000)

(defvar energy start-energy)
(defvar shield-energy 0)
(defvar remaining-days nil)
(defvar end-date 0)
(defvar photon-torpedoes start-photon-torpedoes)
(defvar lost-game nil)
(defvar won-game nil)

(defvar warp-engine-status 0)
(defvar srs-sensor-status 0)
(defvar lrs-sensor-status 0)
(defvar phaser-status 0)
(defvar photon-torpedo-status 0)
(defvar damage-control-status 0)
(defvar shield-control-status 0)
(defvar library-computer-status 0)

(defun parse-number (number-string)
  (let ((number 0))
    (setf number (read-from-string number-string))
    (if (numberp number) number nil)))

(defun get-historical-name (x y)
  (concatenate 'string (nth (+ (* y 2) (if (> x 3) 1 0)) region-names)
	       " "
	       (nth (mod x 4) quadrant-names)))

(defun display-file (file-name)
  (let ((p (parse-namestring file-name)))
    (with-open-file (s p :direction :input)
      (do ((l (read-line s) (read-line s nil 'eof)))
	  ((eq l 'eof) nil)
      (write-line l)))))

(defun setup-sectors ()
  (dotimes (j 8)
    (dotimes (i 8)
      (setf (aref quadrants i j) (make-sector)))))

(defun fill-stars ()
  (dotimes (j 8)
    (dotimes (i 8)
      (dotimes (k (1+ (random max-stars-sector)))
	(let ((x (random 8)) (y (random 8)))
	  (setf (sector-star-list (aref quadrants i j))
		(adjoin (list x y)
			(sector-star-list (aref quadrants i j)))))))))

(defun fill-klingons ()
  (progn
    (setq total-klingons (+ min-klingons-galaxy
			    (random (1+ (- max-klingons-galaxy min-klingons-galaxy)))))
    (setq start-klingons total-klingons)
    (dotimes (k total-klingons)
      (let ((quad-x (random 8)) (quad-y (random 8))
	    (sect-x (random 8)) (sect-y (random 8))
	    (shield-strength (+ min-klingon-shield-energy
				(random (- max-klingon-shield-energy 
					   min-klingon-shield-energy)))))
	(setf (sector-klingon-list (aref quadrants quad-x quad-y))
	      (cons (list sect-x sect-y shield-strength)
		    (sector-klingon-list (aref quadrants quad-x quad-y))))))))

(defun fill-starbases ()
  (progn
    (setq total-starbases (1+ (random max-starbases-galaxy)))
    (dotimes (k total-starbases)
      (let ((quad-x (random 8)) (quad-y (random 8))
	    (sect-x (random 8)) (sect-y (random 8)))
	(setf (sector-starbase-list (aref quadrants quad-x quad-y))
	      (adjoin (list sect-x sect-y)
		      (sector-starbase-list (aref quadrants quad-x quad-y))))))))

(defun setup-galaxy ()
  (progn
    (setup-sectors)
    (fill-stars)
    (fill-klingons)
    (fill-starbases)
    (setf remaining-days (+ (random (- max-days-mission min-days-mission))
			    min-days-mission))
    (setf end-date (+ start-star-date remaining-days))))

(defun setup-enterprise-location ()
  (setf enterprise-location
	(make-location
	 :quad-x (random 8)
	 :quad-y (random 8)
	 :sect-x (random 8)
	 :sect-y (random 8))))

(defun quadrant-matrix (quad-x quad-y)
  (let ((scan-array (make-array '(8 8) :element-type 'byte :initial-element 0))
	(klingon-list (sector-klingon-list (aref quadrants quad-x quad-y)))
	(starbase-list (sector-starbase-list (aref quadrants quad-x quad-y)))
	(star-list (sector-star-list (aref quadrants quad-x quad-y))))
    (loop for (i j) in star-list do
	 (setf (aref scan-array i j) 1))
    (loop for (i j) in starbase-list do
	 (setf (aref scan-array i j) 2))
    (loop for (i j) in klingon-list do
	 (setf (aref scan-array i j) 3))
    (when (and (= quad-x (location-quad-x enterprise-location))
	       (= quad-y (location-quad-y enterprise-location)))
      (setf (aref scan-array
		  (location-sect-x enterprise-location)
		  (location-sect-y enterprise-location))
	    4))
    scan-array))

(defun report-condition (quad-x quad-y)
  (if (> (length (sector-klingon-list (aref quadrants quad-x quad-y))) 0)
      "*RED*"
      "GREEN"))

(defun get-symbol (number)
  (case number
    (0 "   ")
    (1 " * ")
    (2 ">!<")
    (3 "+K+")
    (4 "<*>")))

(defun report-status (quad-x quad-y line)
  (let ((here? (and (= quad-x (location-quad-x enterprise-location))
		    (= quad-y (location-quad-y enterprise-location)))))
    (case line
      (0 (format nil "Stardate             ~D"
		 (- end-date remaining-days)))
      (1 (format nil "Condition            ~A" (report-condition quad-x quad-y)))
      (2 (if here?
	     (format nil "Quadrant             ~D , ~D"
		      (1+ (location-quad-x enterprise-location))
		      (1+ (location-quad-y enterprise-location)))
	      "Quadrant          N/A"))
      (3 (if here?
	     (format nil "Sector               ~D, ~D"
		     (1+ (location-sect-x enterprise-location))
		     (1+ (location-sect-y enterprise-location)))
	     "Sector          N/A"))
      (4 (format nil "Photon torpedoes     ~D"
		 photon-torpedoes))
      (5 (format nil "Total energy         ~D"
		 (+ shield-energy energy)))
      (6 (format nil "Shields              ~D"
		 shield-energy))
      (7 (format nil "Klingons remaining   ~D"
		 total-klingons)))))

(defun short-range-sensor-scan ()
  (let* ((quad-x (location-quad-x enterprise-location))
	 (quad-y (location-quad-y enterprise-location))
	 (grid (quadrant-matrix quad-x quad-y)))
    (setf (sector-scannedp (aref quadrants quad-x quad-y)) t)
    (when (string= (report-condition quad-x quad-y) "*RED*")
      (write-line "COMBAT AREA  CONDITION RED"))
    (write-line  "----------------------------")
    (dotimes (j 8)
      (write-string (get-symbol 0))
      (dotimes (i 8)
	(write-string (get-symbol (aref grid i j))))
      (write-string (get-symbol 0))
      (write-line (report-status quad-x quad-y j)))
    (write-line  "----------------------------")
    nil))
	 
(defun scan-quadrant (quad-x quad-y)
  (if (sector-scannedp (aref quadrants quad-x quad-y))
      (let ((klingons (length (sector-klingon-list
			       (aref quadrants quad-x quad-y))))
	    (starbases (length (sector-starbase-list
				(aref quadrants quad-x quad-y))))
	    (stars (length (sector-star-list
			    (aref quadrants quad-x quad-y)))))
	(format nil "~1D~1D~1D" klingons starbases stars))
      "***"))

(defun long-range-sensor-scan ()
  (let ((quad-x (location-quad-x enterprise-location))
	(quad-y (location-quad-y enterprise-location)))
    (write-line "--------------------")
    (loop for j from (1- quad-y) to (1+ quad-y) do
	 (loop for i from (1- quad-x) to (1+ quad-x) do
	      (if (and (>= j 0) (< j 8) (>= i 0) (< i 8))
		  (progn
		    (setf (sector-scannedp (aref quadrants i j)) t)
		    (format t ": ~A " (scan-quadrant i j)))
		  (write-string ": *** ")))
	 (format *standard-output* ":~%" nil))
    (write-line "--------------------")))
	  
(defun cumulative-galactic-record ()
  (progn
    (terpri)
    (write-line "           COMPUTER RECORD FOR GALAXY          ")
    (format *standard-output*
	    "~&           Enterprise position: ~D , ~D~%"
	    (1+ (location-quad-x enterprise-location))
	    (1+ (location-quad-y enterprise-location)))
    (write-line "     1     2     3     4     5     6     7     8  ")
    (write-line "   ----- ----- ----- ----- ----- ----- ----- -----")
    (dotimes (j 8)
      (format *standard-output* "~&~1D  " (1+ j))
      (dotimes (i 8)
	(format *standard-output* " ~3A  " (scan-quadrant i j)))
      (terpri)
      (write-line "   ----- ----- ----- ----- ----- ----- ----- -----"))
    ))


(defun calculate-relative-location (course warp-factor)
  (progn
    (assert (or (> course 0) (< course 9)
		(> warp-factor 0) (< warp-factor 9)))
    (let* ((ratio (* (1- course) (/ pi 4)))
	   (rel-x (* (cos ratio) warp-factor))
	   (rel-y (* (sin ratio) warp-factor)))
      (multiple-value-bind (den-x rem-x) (truncate rel-x)
	(multiple-value-bind (den-y rem-y) (truncate rel-y)
	  (make-location
	   :quad-x den-x
	   :quad-y (- den-y)
	   :sect-x (truncate (* rem-x 8))
	   :sect-y (- (truncate (* rem-y 8)))
	   ))))))

(defun calculate-relative-course (pos)
  (let* ((fact-x (+ (* (location-quad-x pos) 8) (location-sect-x pos)))
	 (fact-y (- (+ (* (location-quad-y pos) 8) (location-sect-y pos))))
	 (-x? (if (< fact-x 0) t nil)) (-y? (if (< fact-y 0) t nil))
	 (cource! (if (= fact-x 0)
		      (if -y? 7 3)
		      (if (= fact-y 0)
			  (if -x? 5 1)
			  (let ((rho (atan (/ (abs fact-y) (abs fact-x))))
				(theta (if -x?
					   (if -y? pi (* 1/2 pi))
					   (if -y? (* 3/2 pi) 0))))
			    (1+ (* (+ rho theta) (/ 4 pi)))))))
	   (warp-factor (/ (sqrt (+ (* fact-x fact-x) (* fact-y fact-y))) 8)))
	 (list cource! warp-factor)))

(defun add-locations (first-pos second-pos)
  (let ((fact-x (-
		 (+ (* 8 (location-quad-x first-pos)) (location-sect-x first-pos))
		 (+ (* 8 (location-quad-x second-pos)) (location-sect-y second-pos))))
	(fact-y (-
		 (+ (* 8 (location-quad-y first-pos)) (location-sect-y first-pos))
		 (+ (* 8 (location-quad-y second-pos)) (location-sect-y second-pos)))))
    (multiple-value-bind (quad-x sect-x) (truncate (/ fact-x 8))
      (multiple-value-bind (quad-y sect-y) (truncate (/ fact-y 8))
	(make-location :quad-x quad-x :quad-y quad-y
		       :sect-x (* sect-x 8) :sect-y (* sect-y 8))))))

(defun sub-locations (first-pos second-pos)
  (let ((fact-x (-
		 (+ (* 8 (location-quad-x first-pos)) (location-sect-x first-pos))
		 (+ (* 8 (location-quad-x second-pos)) (location-sect-x second-pos))))
	(fact-y (-
		 (+ (* 8 (location-quad-y first-pos)) (location-sect-y first-pos))
		 (+ (* * (location-quad-y second-pos)) (location-sect-y second-pos)))))
    (multiple-value-bind (quad-x sect-x) (truncate (/ fact-x 8))
      (multiple-value-bind (quad-y sect-y) (truncate (/ fact-y 8))
	(make-location :quad-x quad-x :quad-y quad-y
		       :sect-x (* sect-x 8) :sect-y (* sect-y 8))))))
		 
(defun location= (loc1 loc2)
  (let ((loc1-quad-x (location-quad-x loc1))
	(loc1-quad-y (location-quad-y loc1))
	(loc1-sect-x (location-sect-x loc1))
	(loc1-sect-y (location-sect-y loc1))
	(loc2-quad-x (location-quad-x loc2))
	(loc2-quad-y (location-quad-y loc2))
	(loc2-sect-x (location-sect-x loc2))
	(loc2-sect-y (location-sect-y loc2)))
    (and (= loc1-quad-x loc2-quad-x)
	 (= loc1-quad-y loc2-quad-y)
	 (= loc1-sect-x loc2-sect-x)
	 (= loc1-sect-y loc2-sect-y))))
	
(defun warp-engine-control ()
  (block warp-control
    (let ((course)
	  (warp-factor)
	  (energy-consumption)
	  (new-location))
      
      (write-string "COURSE (1 - 9)? ")
      (force-output)
      (setq course (ignore-errors (parse-number (read-line))))

      (unless (numberp course)
	(write-line "What??")
	(return-from warp-control nil))
      
      (unless (and (>= course 1) (<= course 9))
	     (terpri)
	     (write-line "Lt. Sulu reports 'incorrect course data sir!'")
	     (return-from warp-control nil))
      
      (when (and (? warp-engine-status 0.85) (> warp-factor 0.2))
	(write-line "Warp-engines are damaged.  Maximum speed warp 0.2")
	(terpri)
	(setq warp-factor 0.2))
     
      (setq energy-consumption (round (* warp-factor 8 2)))
      (if (< energy energy-consumption)
	  (progn
	    (format *standard-output*
		    "Engineering reports 'Insufficient energy for manuvering at warp ~D'" warp-factor)
	    (terpri)
	    (return-from warp-control nil))
	  (setq energy (- energy energy-consumption)))

      (setq new-location
	    (add-locations enterprise-location
			   (calculate-relative-location course warp-factor)))

      (if (or (< (location-quad-x new-location) 0)
	      (> (location-quad-y new-location) 7)
	      (< (location-sect-x new-location) 0)
	      (> (location-sect-y new-location) 7))
	  (progn
	    (write-line
	     "Lt.  Uhura reports message from starfleed command:")
	    (write-line
	     "   'Permission to attempt crossing of galactic perimeter")
	    (write-line "   is hereby *DENIED*.  Shut down your engines.'")
	    (terpri)
	    (write-line
	     "Chief engineer Scott reports 'Warp engines shut down.'")
	    (terpri)
	    (return-from warp-control nil))
	  (setq enterprise-location new-location)))))

(defun calculate-phaser-hit (energy first-pos second-pos)
  (let* ((first-sect-x (first first-pos))
	 (second-sect-x (first second-pos))
	 (first-sect-y (second first-pos))
	 (second-sect-y (second second-pos))
	 (rel-x (- second-sect-x first-sect-x))
	 (rel-y (- second-sect-y first-sect-y))
	 (distance-sqr (+ (* rel-x rel-x) (* rel-y rel-y))))
    (if (= distance-sqr 0) (setf distance-sqr 1))
    (truncate (* (/ energy distance-sqr) (+ (random 2) 2)))))

(defun reduce-klingons (number)
  (progn
    (decf total-klingons number)
    (when (<= total-klingons 0)
      (terpri)
      (write-line
       "Congratulations captain!  The last Klingon battleship that has been")
      (write-line "menacing the galaxy has been destroyed.")
      (format *standard-output*
	      "~%Your efficiency rating is ~3,2F.~2%"
	      (* 1000 (/ start-klingons remaining-days)))
      (setf won-game t))))
		      
(defun phaser-control ()
  (block phaser
    (let ((klingon-list (sector-klingon-list
			 (aref quadrants
			       (location-quad-x enterprise-location)
			       (location-quad-y enterprise-location))))
	  (alotted-energy 0)
	  (selection))
      (when (not klingon-list)
	(terpri)
	(write-line
	 "Science officer Spock reports: no enemy ships in this quadrant")
	(terpri)
	(return-from phaser nil))

      (when (> phaser-status .85)
	(terpri)
	(write-line "Phasers inoperative.")
	(terpri))

      (if (= (length klingon-list) 1)
	  (progn
	    (terpri)
	    (write-line "Phasers locked on target.")
	    (setq selection 0))
	  (progn
	    (terpri)
	    (write-line "Select one of the following targets:")
	    
	    (let ((number 0))
	      (loop for (i j) in klingon-list do
		   (format *standard-output*
			   "~&Klingon ~D at ~D , ~D~%" number (1+ i) (1+ j))
		   (incf number))
	      (decf number)
	      (terpri)
	      (write-string "Selection? ")
	      (force-output)
	      (setq selection (ignore-errors (parse-number (read-line))))
	      
	      (unless (numberp selection)
		(write-line "What??")
		(return-from phaser nil))
	      
	      (when (or (< selection 0) (> selection number))
		(terpri)
		(write-line
		 "Ensign Checkof reports 'No ship with that number'")
		(return-from phaser nil))
	      )))

      (format *standard-output*
	      "~&Energy available = ~D units~%" energy)
      (write-string "Number of units to fire? ")
      (force-output)
      (setq alotted-energy (ignore-errors (parse-integer (read-line))))

      (when (or (not (numberp alotted-energy)) (< alotted-energy 0))
	(write-line "What??")
	(return-from phaser nil))

      (when (> alotted-energy energy)
	(terpri)
	(write-line
	 "Ensign Checkof reports: 'Insufficient energy.'")
	(return-from phaser nil))

      (setf energy (- energy alotted-energy))
      
      (let* ((klingon (nth selection klingon-list))
	     (enterprise-pos (list (location-sect-x enterprise-location)
				   (location-sect-y enterprise-location)))
	     (klingon-pos-x (first klingon))
	     (klingon-pos-y (second klingon))
	     (klingon-shield (third klingon))
	     (hit (calculate-phaser-hit
		   alotted-energy enterprise-pos klingon)))
	(if (> hit (* .15 klingon-shield))
	    (progn
	      (terpri)
	      (format *standard-output*
		      "~&~D units hit Klingon at ~D , ~D~%"
		      (ceiling hit) (first klingon) (second klingon))
	      (setq klingon-shield (- klingon-shield hit))
	      (if (< klingon-shield 0)
		  (progn
		    (write-line "*** KLINGON DESTROYED ***")
		    (setq klingon-list (remove klingon klingon-list))
		    (reduce-klingons 1))
		  (progn
		    (format *standard-output*
			    "    (Sensor show ~D units remaining.)~%"
			    (ceiling klingon-shield))
		    (setf (third (nth selection klingon-list))
			  klingon-shield)))

	      (setf (sector-klingon-list
		     (aref quadrants
			   (location-quad-x enterprise-location)
			   (location-quad-y enterprise-location)))
		    klingon-list))
	    (format *standard-output*
		    "~&Sensors show no damage at ~D , ~D~%"
		    (1+ klingon-pos-x) (1+ klingon-pos-y)))
	))))

(defun enterprise-destroyed ()
  (progn
    (terpri)
    (write-line "The Enterprise has been destroyed!")
    (write-line "Soon the federation will be conquered.")
    (write-line "The game is over.")
    (format *standard-output*
	    "~&At the end of the game there were ~D klingon~P remaining.~2%"
	    total-klingons total-klingons)
    (setf lost-game t)))

(defun calculate-enterprise-damage (energy-hit)
  (progn
    (when (< (- shield-energy energy-hit) 0)
      (enterprise-destroyed)
      (return-from calculate-enterprise-damage))
    
    (when (> energy-hit (* shield-energy .15))
      (let ((damage (/ (random 100) 100))
	    (system (random 8))
	    (sensor-name "None"))
	(macrolet ((update (variable name)
		     `(progn (incf damage ,variable)
			     (if (> damage 1.0) (setf damage 1.0))
			     (setf ,variable damage)
			     (setf sensor-name ,name))
		     ))
	  (case system
	    (0 (update warp-engine-status "warp engines"))
	    (1 (update srs-sensor-status "short range sensor"))
	    (2 (update lrs-sensor-status "long range sensor"))
	    (3 (update phaser-status "phaser control"))
	    (4 (update photon-torpedo-status "photon torpedo control"))
	    (5 (update damage-control-status "damage control"))
	    (6 (update shield-control-status "shield control"))
	    (7 (update library-computer-status "library computer")))
	  (let ((damage-extent "damaged"))
	    (when (> damage .85)
	      (setf damage-extent "disabled"))
	    (format *standard-output*
		    "~&Damage control reports '~A ~A by the hit'~%"
		    sensor-name damage-extent))
	  ))
      (decf shield-energy energy-hit))))

(defun complete-repair-enterprise-damage ()
  (progn
    (setf warp-engine-status 0)
    (setf srs-sensor-status 0)
    (setf lrs-sensor-status 0)
    (setf phaser-status 0)
    (setf photon-torpedo-status 0)
    (setf damage-control-status 0)
    (setf shield-control-status 0)
    (setf library-computer-status 0)
    ))

(defun partial-repair-enterprise-damage ()
  (let ((repair (/ (random 100) 100)))
    (macrolet ((update (variable)
		 `(if (> ,variable 0)
		      (let ((damage (- ,variable repair)))
			(if (< damage 0) (setf damage 0))
			(setf ,variable damage)
			(return-from partial-repair-enterprise-damage)))
		 ))
      (update warp-engine-status)
      (update shield-control-status)
      (update srs-sensor-status)
      (update lrs-sensor-status)
      (update phaser-status)
      (update photon-torpedo-status)
      (update damage-control-status)
      (update library-computer-status)
      )))

(defun found-klingons-quadrant ()
  (let ((klingon-list
	(sector-klingon-list
	 (aref quadrants
	       (location-quad-x enterprise-location)
	       (location-quad-y enterprise-location)))))

    (if (> (list-length klingon-list) 0)
	t nil)))


;	(enterprise-sect-x (location-sect-x enterprise-location))
;	(enterprise-sect-y (location-sect-y enterprise-location)))
;    
; ;   (loop for (sect-x sect-y) in klingon-list do
;	 (let ((klingon-shoots (if (= (random 2) 0) nil t)))
;	   (when klingon-shoots
;	     (let* ((energy-fired (+ min-klingon-phaser-fire
;				     (random (1+
;					      (- max-klingon-phaser-fire
;						 min-klingon-phaser-fire)))))
;		    (energy-hit (calculate-phaser-hit
;				 energy-fired
;				 (list enterprise-sect-x enterprise-sect-y)
;				 (list sect-x sect-y))))
;	       (format *standard-output*
;		       "~%~D unit hit on Enterprise from ~D, ~D~%"
;		       energy-hit (1+ sect-x) (1+ sect-y))
;	       (when (> (- shield-energy energy-hit) 0)
;		 (format *standard-output*
;			 "~&      ~%"
;;			 (- shield-energy energy-hit)))
;;	       (calculate-enterprise-damage energy-hit)
;;	       ))))
 ;;   ))

(defun klingon-phaser-fire ()
  (block phaser
    (let ((klingon-list 
	   (sector-klingon-list
	    (aref quadrants 
		  (location-quad-x enterprise-location)
		  (location-quad-y enterprise-location))))
	  (enterprise-sect-x (location-sect-x enterprise-location))
	  (enterprise-sect-y (location-sect-y enterprise-location))
	  )
    
      (loop for (sect-x sect-y) in klingon-list do
	    (let ((klingon-shoots (if (= (random 2) 0) nil t)))
	      (when klingon-shoots
		(let* ((energy-fired (+ min-klingon-phaser-fire
					(random (1+
						 (- max-klingon-phaser-fire
						    min-klingon-phaser-fire)))))
		       (energy-hit (calculate-phaser-hit 
				    energy-fired
				    (list enterprise-sect-x enterprise-sect-y)
				    (list sect-x sect-y))))
		  (format *standard-output*
			  "~%~D unit hit on Enterprise from ~D, ~D~%"
			  energy-hit (1+ sect-x) (1+ sect-y))
		  (when (> (- shield-energy  energy-hit) 0)
		    (format *standard-output*
			    "~&    ~%"
			    (- shield-energy energy-hit)))
		  (calculate-enterprise-damage  energy-hit)		  
		  ))))
      )))


(defun photon-torpedo-control ()
  (block torpedo
    (unless (> photon-torpedoes 0)
      (write-line "All photon torpedos expended.")
      (terpri)
      (return-from torpedo nil))
    
    (when (> photon-torpedo-status .85)
      (write-line "Photon tubes are not operational")
      (terpri)
      (return-from torpedo nil))
    
    (let ((course))
      (write-string "COURSE (1-9)? ")
      (force-output)
      (setq course (ignore-errors (parse-number (read-line))))

      (unless (numberp course)
	(write-line "What??")
	(return-from torpedo nil))
      
      (unless (and (>= course 1) (<= course 9))
	(terpri)
	(write-line "Ensign Checkov reports 'incorrect course data, sir!")
	(return-from torpedo nil))
      
      (decf photon-torpedoes)
      (terpri)
      (write-line "Torpedo track:")
      
      (let* ((ratio (* (/ (1- course) 4) pi))
	     (step-x (cos ratio))
	     (step-y (- (sin ratio)))
	     (cur-x (location-sect-x enterprise-location))
	     (cur-y (location-sect-y enterprise-location))
	     (sector (aref quadrants
			   (location-quad-x enterprise-location)
			   (location-quad-y enterprise-location)))
	     (star-list (sector-star-list sector))
	     (starbase-list (sector-starbase-list sector))
	     (klingon-list (sector-klingon-list sector)))
	(loop
	   (setq cur-x (+ cur-x step-x))
	   (setq cur-y (+ cur-y step-y))

	   (dolist (s star-list)
	     (when (and (= (first s) (round cur-x))
			(= (second s) (round cur-y)))
	       (format *standard-output*
		       "~&Star at ~D , ~D absorbed torpedo energy.~%"
		       (1+ (first s)) (1+ (second s)))
	       (terpri)
	       (return-from torpedo nil)))

	   (dolist (k klingon-list)
	     (when (and (= (first k) (round cur-x))
			(= (second k) (round cur-y)))
	       (write-line "*** KLINGON DESTROYED ***")
	       (terpri)
	       (setf (sector-klingon-list
		      (aref quadrants
			    (location-quad-x enterprise-location)
			    (location-quad-y enterprise-location)))
		     (remove k klingon-list))
	       (reduce-klingons 1)
	       (return-from torpedo nil)))

	   (dolist (s starbase-list)
	     (when (and (= (first s) (round cur-x))
			(= (second s) (round cur-y)))
	       (write-line "*** STARBASE DESTROYED ***")
	       (setf (sector-starbase-list
		      (aref quadrants
			    (location-quad-x enterprise-location)
			    (location-quad-y enterprise-location)))
		     (remove s starbase-list))
	       (decf total-starbases)
	       (if (= total-starbases 0)
		   (progn
		     (write-line "That does it, captain!! You are hereby relieved of command")
		     (write-line "and sentenced to 99 days hard labor at Cygnus 12!")
		     (setf lost-game t)
		     (return-from torpedo nil))
		   (progn 
		     (write-line "Starfleet is revieing your record")
		     (write-line "to consider you for court martial.")
		     (terpri)
		     (return-from torpedo nil)))))

	   (when (or (< cur-x 0) (> cur-x 7)
		     (< cur-y 0) (> cur-y 7))
	     (write-line "Torpedo missed.")
	     (terpri)
	     (return-from torpedo nil))

	   (format *standard-output* "               ~D , ~D~%"
		   (1+ (round cur-x)) (1+ (round cur-y))))))))

(defun shield-control ()
  (block shield
    (let ((allotted-energy 0))
      
      (when (> shield-control-status .85)
	(write-line "Shields inoperable.")
	(return-from shield nil))
      
      (format *standard-output*
	      "~&Energy currently allotted to shields = ~D units~%"
	      shield-energy)
      (format *standard-output* "~&Energy available = ~D units~%" energy)
      (write-string "Number of units to shields? ")
      (force-output)
      (setq allotted-energy (ignore-errors (parse-integer (read-line))))

      (when (or (not (numberp allotted-energy)) (< allotted-energy 0))
	(write-line "What??")
	(return-from shield nil))

      (when (= allotted-energy shield-energy)
	(write-line "")
	(return-from shield nil))

      (when (> allotted-energy (+ energy shield-energy))
	(write-line
	 "Deflector control reports 'This is not the federation treasury'")
	(return-from shield nil))

      (decf energy (- allotted-energy shield-energy))
      (setf shield-energy allotted-energy)
     
      (write-line "Deflector control room reports:")
      (format *standard-output*
	      "'Shields are now at ~D units at your command.'~%"
	      shield-energy)
      shield-energy)))

(defun damage-control-report ()
  (progn
    (terpri)
    (write-line "Device                State of repair")
    (write-line "-------------------------------------")
    (format *standard-output*
	    "Warp engines              ~3,2F~%" warp-engine-status)
    (format *standard-output*
	    "Short range sensor        ~3,2F~%" srs-sensor-status)
    (format *standard-output*
	    "Long range sensor         ~3,2F~%" lrs-sensor-status)
    (format *standard-output*
	    "Phaser control            ~3,2F~%" phaser-status)
    (format *standard-output*
	    "Photon tubes              ~3,2F~%" photon-torpedo-status)
    (format *standard-output*
	    "Shield control            ~3,2F~%" shield-control-status)
    (format *standard-output*
	    "Damage control            ~3,2F~%" damage-control-status)
    (format *standard-output*
	    "Library computer          ~3,2F~%" library-computer-status)
    (terpri)))
	   

(defun photon-torpedo-data () 
  (block data
    (let ((klingon-list (sector-klingon-list
			 (aref quadrants
			       (location-quad-x enterprise-location)
			       (location-quad-y enterprise-location)))))
      (when (= (length klingon-list) 0)
	(terpri)
	(write-line "Sensors report no klingons in this quadrant")
	(terpri)
	(return-from data nil))
      
      (format *standard-output*
	      "~%Distance to klingon battle cruiser~P~2%"
	      (length klingon-list))

      (loop for (i j) in klingon-list do
	   (let ((data (calculate-relative-course
			(sub-locations
			 (make-location
			  :quad-x (location-quad-x enterprise-location)
			  :quad-y (location-quad-y enterprise-location)
			  :sect-x i :sect-y j)
			 enterprise-location))))

	     (format *standard-output*
		     "~&Klingon in sector ~D, ~D~%Course = ~ 3,2F~%Distance= ~3,2F~2%" (1+ i) (1+ j) (first data) (second data)))))))

(defun starbase-nav-data () 
  (block data
    (let ((starbase-list (sector-starbase-list
			  (aref quadrants
				(location-quad-x enterprise-location)
				(location-quad-y enterprise-location)))))
      (when (= (length starbase-list) 0)
	(terpri)
	(write-line "Sensors report no starbase in this quadrant")
	(terpri)
	(return-from data nil))
      
      (format *standard-output*
	      "~%Distance to starbase~P~2%"
	      (length starbase-list))

      (loop for (i j) in starbase-list do
	   (let ((data (calculate-relative-course
			(sub-locations
			 (make-location
			  :quad-x (location-quad-x enterprise-location)
			  :quad-y (location-quad-y enterprise-location)
			  :sect-x i :sect-y j)
			 enterprise-location))))

	     (format *standard-output*
		     (concatenate 'string
				  "~&starbase in sector ~D, ~D"
				  "~%Course = ~3,2F~%Distance= ~3,2F~2%")
		     (1+ i) (1+ j) (first data) (second data)))))))

(defun navigation-calculator () 
  (block navigation
    (terpri)
    (write-line "Navigation calculator")
    (format t "~%Current position in quadrant is ~D, ~D sector ~Dm ~D~2%"
	    (1+ (location-quad-x enterprise-location))
	    (1+ (location-quad-y enterprise-location))
	    (1+ (location-sect-x enterprise-location))
	    (1+ (location-sect-y enterprise-location)))

    (write-line "Enter quadrant coordinates")
    (let ((quad-x (progn
		    (write-string "X coordinate (1..8): ")
		    (force-output)
		    (ignore-errors (parse-integer (read-line)))))
	  (quad-y (progn
		    (write-string "Y coordinate (1..8): ")
		    (force-output)
		    (ignore-errors (parse-integer (read-line))))))
      
      (unless (and (numberp quad-x) (numberp quad-y))
	(write-line "What??")
	(return-from navigation nil))
      
      (when (or (< quad-x 1) (> quad-x 8)
		(< quad-y 1) (> quad-y 8))
	(terpri)
	(write-line "Not a valid quadrant.")
	(terpri)
	(return-from navigation nil))
      
      (let ((data (calculate-relative-course
		   (sub-locations
		    (make-location :quad-x (1- quad-x) :quad-y (1- quad-y)
				   :sect-x 
				   (location-sect-x enterprise-location)
				   :sect-y
				   (location-sect-y enterprise-location))
		    enterprise-location))))

	(format t "~%Navigation~%Course = ~3,2F~%Distance= ~3,2F~2%"
		(first data) (second data))))))

(defun status-report () 
  (progn
    (format *standard-output* "~%Status report~%")
    (format *standard-output* "There are ~D Klingon~P left.~%"
	    total-klingons total-klingons)
    (format *standard-output* "Mission must be completed in ~D day~P.~%"
	    remaining-days remaining-days)
    (format *standard-output*
	    "The federation has ~D starbase~P in the galaxy.~%"
	    total-starbases total-starbases)
    nil))

(defun galactic-region-name-map () 
  (display-file map-file))

(defun library-computer () 
  (block computer
    (when (> library-computer-status .85)
      (write-line "Library computer disabled.")
      (return-from computer nil))
    (let ((command
	   (progn
	     (write-string "Computer active and awaiting command? ")
	     (force-output)
	     (ignore-errors (parse-integer (read-line))))))
      (case command
	(0 (cumulative-galactic-record))
	(1 (status-report))
	(2 (photon-torpedo-data))
	(3 (starbase-nav-data))
	(4 (navigation-calculator))
	(5 (galactic-region-name-map))
	(otherwise
	 (terpri)
	 (write-line "The library computer accepts the following commands:")
	 (write-line "  0  -- Cumulative galactic record")
	 (write-line "  1  -- Status report")
	 (write-line "  2  -- Photon torpedo data")
	 (write-line "  3  -- Starbase navigation data")
	 (write-line "  4  -- Navigation calculator")
	 (write-line "  5  -- Galactic region name map")
	 (terpri)))
      nil)))

(defun select-command () "...")

(defun describe-settings () "...")

(defun found-starbase-quadrant () "...")

(defun dock-to-starbase () "...")

(defun setup-parameters () "...")

(defun update-date (number) "...")

(defun play-again-p () "...")

(defun startrek () "...")

