(load "read-csv.lisp")

;;a team struct to hold all of the pertinent info for a given team

(defstruct team
  ;;an integer for games played by the team
  games
  ;;integer for team at-bats
  ABs
  ;;integer for team hits
  hits
  ;;integer for team RBIs
  RBIs
  ;;integer for team hits
  errors
  ;;array for the last 10 games run differential
  run-diffs
  ;;pointer for the next array index to be altered
  next-altered
  ;;integer for the total bases
  total-bases
  )

;;a pitcher struct that will keep information on the games that a
;;certain pitcher pitches

(defstruct pitcher
  ;;an integer to track the amount of outs recorder
  outs
  ;;an integer to track the amount of earned runs allowed
  ERs
  ;;an integer for hits allowed
  hits
  ;;an integer for the at-bats against the pitcher
  ABs
  ;;integer for the total bases
  total-bases
  )




;;functions to get inputs to the neural network

;;pitcher function

;; get-ERA
;; ------------------------------
;; Input: pitcher a pitcher struct
;; Output: the ERA in the game the pitcher throws
;;         divided by 9 to make it a value between 0 and 1
;;         if the value is >1 return 1.0

(defun get-ERA
    (pitcher)
  (let* ((ER-per-out (coerce (/ (pitcher-ERs pitcher)
				(pitcher-outs pitcher))
			     'float))
	 (our-ERA (* ER-per-out 3)))
    (if (> our-ERA 1.0)
	1.0
      our-ERA)))

;; get-BAA
;;--------------------------------
;; Input: pitcher a pitcher struct
;; Output: the BAA in the games the pitcher throws

(defun get-BAA
    (pitcher)
  (coerce (/ (pitcher-hits pitcher)
	     (pitcher-ABs pitcher))
	  'float))


;;team functions

;; get-RBIs
;;-------------------------------
;; Input: team a team struct
;; Output: the RBIs per innning or 1.0 if this value
;;         is larger than 1.0

(defun get-RBIs
    (team)
  (let* ((RBI-game (coerce (/ (team-RBIs team)
			      (team-games team))
			   'float))
	 (RBI-inning (/ RBI-game 9)))
    (if (> RBI-inning 1.0)
	1.0
      RBI-inning)))

;; get-BA
;;-------------------------------
;; Input: team a team struct
;; Output: the teams batting average

(defun get-BA
    (team)
  (coerce (/ (team-hits team)
	     (team-ABs team))
	  'float))

;; get-errors
;;------------------------------
;; Input: team, a team struct
;; Output: the teams errors per inning or 1
;;         if it is greater than 1 somehow

(defun get-errors
    (team)
  (let* ((errors-game (coerce (/ (team-errors team)
				 (team-games team))
			      'float))
	 (errors-inning (/ errors-game 9)))
    (if (> errors-inning 1.0)
	1.0
      errors-inning)))


;; get-slugging
;; --------------------------
;; Input: team, a team struct
;; Output: the teams slugging%

(defun get-slugging
    (team)
  (let ((slugging (coerce (/ (team-total-bases team)
			     (team-ABs team))
			  'float)))
    (if (> slugging 1.0)
	1.0
      slugging)))

;; get-slugging-against
;; -----------------------------
;; Input: pitcher, a pitcher struct
;; Output: the pitchers slugging% against

(defun get-slugging-against
    (team)
  (let ((slugging (coerce (/ (pitcher-total-bases pitcher)
                             (pitcher-ABs pitcher))
                          'float)))
    (if (> slugging 1.0)
        1.0
      slugging)))

;; get-run-diff
;; ------------------------------
;; Input: team, a team struct
;; Output: the teams average run diff over
;;         the last 10 games

(defun get-run-diff
    (team)
  (let* ((arr (team-run-diffs team))
	 (diff (+ (aref arr 0) (aref arr 1)
		  (aref arr 2) (aref arr 3)
		  (aref arr 4) (aref arr 5)
                  (aref arr 6) (aref arr 7)
		  (are farr 8) (aref arr 9)))
	 (avg-diff (coerce (/ diff 90)
			   'float)))
    (when (> avg-diff 1.0)
      1.0)
    (when (< avg-diff 0.0)
      0.0)
    avg-diff))





;;Home team info
;;4 team
;;6 game #
;;10 runs scored
;;22-27 ABs,hits,2B,3B,HR,RBI
;;41 ER
;;44 putouts
;;46 errors
;;50 opposing ABs
;;51 opposing hits
;;102 starters id
;;103 starters name

(defconstant HOME-TEAM 4)
(defconstant HOME-GAME 6)
(defconstant HOME-RUNS 10)
(defconstant HOME-AB 22)
(defconstant HOME-HITS 23)
(defconstant HOME-2B 24)
(defconstant HOME-3B 25)
(defconstant HOME-HR 26)
(defconstant HOME-RBI 27)
(defconstant HOME-ER 41)
(defconstant HOME-PUTOUTS 44)
(defconstant HOME-ERRORS 46)
(defconstant HOME-STARTER 102)
(defconstant VISITOR-TEAM 4)
(defconstant VISITOR-GAME 6)
(defconstant VISITOR-RUNS 10)
(defconstant VISITOR-AB 22)
(defconstant VISITOR-HITS 23)
(defconstant VISITOR-2B 24)
(defconstant VISITOR-3B 25)
(defconstant VISITOR-HR 26)
(defconstant VISITOR-RBI 27)
(defconstant VISITOR-ER 41)
(defconstant VISITOR-PUTOUTS 44)
(defconstant VISITOR-ERRORS 46)
(defconstant VISITOR-STARTER 102)

;;visiting team info
;;7 team   
;;9 game # 
;;11 runs scored 
;;50-55 ABs,hits,2B,3B,HR,RBI     
;;69 ER 
;;72 putouts  
;;74 errors 
;;22 opposing ABs                    
;;23 opposing hits                 
;;104 starters id                    
;;105 starters name


;;incrementing functions

(defun inc-games
    (team)
  (setf (team-games team) (+ 1 (team-games team))))

(defun inc-ABs
    (team ABs)
  (setf (team-ABs team) 
    (+ (team-ABs team) ABs)))

(defun inc-hits
    (team hits)
  (setf (team-hits team)
    (+ (team-hits team) hits)))

(defun inc-RBIs
    (team RBIs)
  (setf (team-RBIs team)
    (+ (team-RBIs team) RBIs)))

(defun inc-errors
    (team errors)
  (setf (team-errors team) 
    (+ (team-errors team) errors)))

(defun inc-total-bases
    (team hits doubl tripl hr)
  (setf (team-total-bases team)
     (+ (team-total-bases team)
	hits doubl tripl tripl hr hr hr)))

(defun inc-run-diffs
    (team runs opp-runs)
  (let ((next (team-next-altered team)))
    (setf (team-next-altered team) (+ 1 next))
    (setf (aref (team-run-diffs team)  next) 
      (- runs opp-runs))))

(defun inc-pitch-outs
    (pitcher outs)
  (setf (pitcher-outs pitcher)
    (+ (pitcher-outs pitcher) outs)))

(defun inc-pitch-ERs
    (pitcher ERs)
  (setf (pitcher-ERs pitcher)
    (+ (pitcher-ERs pitcher) ERs)))

(defun inc-pitch-hits
    (pitcher hits)
  (setf (pitcher-hits pitcher)
    (+ (pitcher-hits pitcher) hits)))

(defun inc-pitch-ABs
    (pitcher ABs)
  (setf (pitcher-ABs pitcher)
    (+ (pitcher-ABs pitcher) ABs)))

(defun inc-pitch-total-bases
    (pitcher hits doubl tripl hr)
  (setf (pitcher-total-bases pitcher)
    (+ (pitcher-total-bases pitcher) 
       hits doubl tripl tripl hr hr hr)))

(defun update-home-team
    (team game)
  (let ((ABs (aref game HOME-TEAM-AB))
	(hits (aref game HOME-TEAM-HITS))
	(RBIs (aref game HOME-TEAM-RBI))
	(errors (aref game HOME-TEAM-ERRORS))
	(home-HRs (aref game HOME-TEAM-HR))
	(home-2B (aref game HOME-TEAM-2B))
	(home-3B (aref game HOME-TEAM-3B))
	(visitor-HRs (aref game VISITOR-TEAM-HR))
	(visitor-2B (aref game VISITOR-TEAM-2B))
	(visitor-3B (aref game VISITOR-TEAM-3B))
	(home-runs (aref game HOME-RUNS))
	(visitor-runs (aref game VISITOR-RUNS)))
    (inc-games team)
    (inc-ABs team ABs)
    (inc-hits team hits)
    (inc-RBIs team RBIs)
    (inc-errors team errors)
    (inc-total-bases team hits home-2B home-3B home-HRs)
    (inc-run-diffs team home-runs visitor-runs)))

(defun update-visiting-team
    (team game)
  (let ((ABs (aref game VISITING-TEAM-AB))
	(hits (aref game VISITING-TEAM-HITS))
	(RBIs (aref game VISITING-TEAM-RBI))
	(errors (aref game VISITING-TEAM-ERRORS))
	(home-HRs (aref game HOME-TEAM-HR))
        (home-2B (aref game HOME-TEAM-2B))
        (home-3B (aref game HOME-TEAM-3B))
        (visitor-HRs (aref game VISITOR-TEAM-HR))
        (visitor-2B (aref game VISITOR-TEAM-2B))
        (visitor-3B (aref game VISITOR-TEAM-3B))
        (home-runs (aref game HOME-RUNS))
        (visitor-runs (aref game VISITOR-RUNS)))
    (inc-games team)
    (inc-ABs team ABs)
    (inc-hits team hits)
    (inc-RBIs team RBIs)
    (inc-errors team errors)
    (inc-total-bases team hits visitor-2B visitor-3B visitor-HRs)
    (inc-run-diffs team visitor-runs home-runs)))

(defun update-home-pitcher
    (pitcher game)
  (let ((ABs (aref game VISITING-TEAM-AB))
	(ERs (aref game HOME-TEAM-ER))
	(hits (aref game VISITING-TEAM-HITS))
	(putouts (aref game HOME-TEAM-PUTOUTS))
	(visitor-HRs (aref game VISITOR-TEAM-HR))
        (visitor-2B (aref game VISITOR-TEAM-2B))
        (visitor-3B (aref game VISITOR-TEAM-3B)))
    (inc-pitch-ABs pitcher ABs)
    (inc-pitch-ERs pitcher ERs)
    (inc-pitch-hits pitcher hits)
    (inc-pitch-outs pitcher putouts)
    (inc-pitch-total-bases pitcher hits visitor-2B visitor-3B visitor-HRs)))

(defun update-visiting-pitcher
    (pitcher game)
  (let ((ABs (aref game HOME-TEAM-AB))
	(ERs (aref game VISITING-TEAM-ER))
	(hits (aref game HOME-TEAM-HITS))
	(putouts (aref game VISITING-TEAM-PUTOUTS))
	(home-HRs (aref game HOME-TEAM-HR))
        (home-2B (aref game HOME-TEAM-2B))
        (home-3B (aref game HOME-TEAM-3B)))
    (inc-pitch-ABs pitcher ABs)
    (inc-pitch-ERs pitcher ERs)
    (inc-pitch-hits pitcher hits)
    (inc-pitch-outs pitcher putouts)
    (inc-pitch-total-bases pitcher hits home-2B home-3B home-HRs)))

(defun update-structs
    (home-team road-team home-pitcher road-pitcher game)
  (update-home-team home-team game)
  (update-visiting-team visiting-team game)
  (update-home-pitcher home-pitcher game)
  (update-visiting-pitcher visiting-pitcher game))

(defun csv-reader
    (file)
  (with-open-file (stream file)
  (let* ((len (file-length stream))
	 (data (make-array len)))
    (read-sequence data stream)
    data)))

(defun csv-test
    (file)
  (let ((x (csv-reader file)))
    ;;(format t "~A~%" (type-of x))
    (format t "~A~%" (aref x 1))
    (format t "~A~%" (aref x 2))
    (format t "~A~%" (aref x 3))
    (format t "~A~%" (aref x 4))
    (format t "~A~%" (aref x 5))
    (format t "~A~%" (aref x 6))))
    




;;take in the values from the file
;;if the game is 1 then make a new struct for the team and
;;;;;;;;;upddate the struct according to the values
;;otherwise update the values in the file using the above
;;;;;;;;funcs and then send the correct inputs to the nn to train
;;;;;;;;the nn and do this for the whole season file
;;after reset all of the structs and run the trained nn on a new
;;;;;;;;season while still updating the teams as they go through
;;;;;;;;the season
;;keep track of the games where which team is predicted correctly
;;;;;;;as wwell as the number of games played can do this on a basis
;;;;;;;dividing the season into parts as well to see if it gets better
;;;;;;;later in the season


;;;  NN struct
;;; ------------------------------------------------------
;;;  Neurons and edges are not explicitly represented.  Instead, the 
;;;  information for all the neurons and edges (e.g., output values,
;;;  weights, delta values) is contained in various vectors and arrays.
;;;  For example, in a 3-layer network with 3 input neurons, 4 hidden neurons,
;;;  and 2 output neurons, the NN struct would have:
;;;     NUM-LAYERS = 3
;;;     LAYER-SIZES = (3 4 2)
;;;     OUTPUT-VECKS = vector of 3 vectors (one for each layer)
;;;        output-veck #0 would have 3 entries
;;;        output-veck #1 would have 4 entries
;;;        output-veck #2 would have 2 entries
;;;     DELTA-VECKS = vector of 3 vectors (similar to OUTPUT-VECKS)
;;;     WEIGHT-ARRAYS = vector of 2 weight arrays
;;;        weight-array #0 would be a 3-by-4 array of weights
;;;          (corresponding to weights between 3 input neurons
;;;           and 4 hidden neurons)
;;;        weight-array #1 would be a 4-by-2 array of weights
;;;          (corresponding to weights between 4 hidden neurons
;;;           and 2 output neurons)

(defstruct nn
  ;; NUM-LAYERS:  the number of layers in the neural network
  num-layers    
  ;; LAYER-SIZES:  a vector specifying the number of neurons in each layer
  ;;   (The input layer is layer 0; the output layer is layer (1- num-layers)
  layer-sizes   
  ;; OUTPUT-VECKS:  A vector of vectors.
  ;;  Each neuron has an associated output value.
  ;;  (svref output-vecks i) -- is a vector of the computed output
  ;;        values for the neurons in layer i.
  output-vecks 
  ;; WEIGHT-ARRAYS:  A vector of arrays.
  ;;  (svref weight-arrays i) -- is an MxN array holding the weights
  ;;        for all edges between layer i and layer (i+1).
  ;;        M = number of neurons in layer i; N = number of neurons in layer (i+1)
  weight-arrays 
  ;; DELTA-VECKS:  A vector of vectors.
  ;;  Each neuron (except those in the input layer) has an associated
  ;;  DELTA value computed during back-propagation.
  ;;  (svref delta-vecks i) -- is a vector of the delta values for
  ;;       the neurons in layer i.
  delta-vecks   
  )

;;;  INIT-NN
;;; -----------------------------------------
;;;  INPUT:  SIZES-OF-LAYERS, a list of numbers indicating how
;;;           many neurons are in each layer.  (Layer 0 corresponds
;;;           to the input layer).
;;;  OUTPUT:  A neural network (NN struct) of that size, initialized
;;;           with weights randomly selected between -0.5 and +0.5.

(defun init-nn (sizes-of-layers)
  (let* (;; NUM-LAYERS:  the number of layers in the network
	 (num-layers (length sizes-of-layers))
	 ;; LAYER-SIZES:  a vector whose ith element will say how many
	 ;;  neurons are in layer i
	 (layer-sizes (make-array num-layers))
	 ;; OUTPUT-VECKS:  a vector of vectors.  The ith vector will
	 ;;  contain output values for each neuron in layer i
	 (output-vecks (make-array num-layers))
	 ;; DELTA-VECKS:  similar to output-vecks, except they contain
	 ;;  the delta values for each neuron
	 (delta-vecks (make-array num-layers))
	 ;; WEIGHT-ARRAYS:  see documentation of NN struct
	 (weight-arrays (make-array (1- num-layers)))
	 ;; NN: the network
	 (nn (make-nn :num-layers num-layers
		      :layer-sizes layer-sizes
		      :output-vecks output-vecks
		      :weight-arrays weight-arrays
		      :delta-vecks delta-vecks)))
    ;; For each layer...
    (dotimes (i num-layers)
      ;; Set the size of that layer (i.e., how many neurons)
      (setf (svref layer-sizes i) (nth i sizes-of-layers))
      ;; Create a vector of output values for the neurons in that layer
      (setf (svref output-vecks i) (make-array (svref layer-sizes i)
					       :initial-element nil))
      ;; Create a vector of delta values for the neurons in that layer
      (setf (svref delta-vecks i) (make-array (svref layer-sizes i)
					      :initial-element nil))
      ;; For non-input neurons, create an array of weights
      ;; corresponding to edges between current layer and previous layer
      (when (> i 0)
	(let* ((num-rows (svref layer-sizes (1- i)))
	       (num-cols (svref layer-sizes i))
	       ;; The array of weights
	       (harry (make-array (list num-rows num-cols))))
	  (setf (svref weight-arrays (1- i)) harry)
	  ;; randomize weights
	  (dotimes (i num-rows)
	    (dotimes (j num-cols)
	      (setf (aref harry i j) (- (/ (random 100) 100) 0.5)))))))
    ;; return the NN
    nn))



;;;  SET-INPUTS
;;; --------------------------------------------------
;;;  INPUT:  NN, a neural network
;;;          INPUTS, a list of input values for the input neurons of NN
;;;  OUTPUT: NN
;;;  SIDE EFFECT:  Sets the "output" value of each neuron in the
;;;    input layer to the corresponding value in INPUTS.

(defun set-inputs (nn inputs)
  (let* ((out-vecks (nn-output-vecks nn))
	 ;; OUT-VECK-ZERO:  the vector of "output" values for layer 0 
	 (out-veck-zero (svref out-vecks 0))
	 (num-inputs (svref (nn-layer-sizes nn) 0)))
    (cond
     ;; CASE 1:  INPUTS has the right number of input values
     ((= num-inputs (length inputs))
      ;; For each input value...
      (dotimes (i num-inputs)
	;; Set the "output" value for the corresponding neuron in layer 0 
	(setf (svref out-veck-zero i) (nth i inputs)))
      ;; return the NN
      nn)
     ;; Case 2:  Error!
     (t
      (format t "Whoops!  Wrong number of input values for this NN!~%")))))

;;;  SIGMOID
;;; ------------------------------
;;;  SIGMOID(X) = 1/(1 + e^(-x)) -- the sigmoid (or logistic) function
     
(defun sigmoid (x)
  (/ 1.0 (+ 1 (exp (- x)))))

;;;  FEED-FORWARD
;;; ----------------------------------------------------------
;;;  INPUTS:  NN, a neural network
;;;           INPUTS, a list of input values for the input neurons in NN
;;;  OUTPUT:  NN
;;;  SIDE-EFFECT:  Applies the given INPUT values to the input layer of NN
;;;   and propagates them forward to generate output values for all neurons
;;;   in the network.

(defun feed-forward (nn inputs)
  ;; First, set the output value for each neuron to NIL
  (erase-outputs nn)
  ;; Next, set the "output" value for each neuron in the input layer
  ;; to the corresponding value in INPUTS
  (set-inputs nn inputs)
  
  (let ((num-layers (nn-num-layers nn))
	(layer-sizes (nn-layer-sizes nn))
	(output-vecks (nn-output-vecks nn))
	(weight-arrays (nn-weight-arrays nn)))
  
    ;; For each LAYER from 1 onward (i.e., not including the input layer)
    (do ((lay-num 1 (1+ lay-num)))

	;; Exit Condition
	((= lay-num num-layers)
	 nn)
      
      ;; Body of DO
      (let* ((outputs (svref output-vecks lay-num))
	     (prev-outputs (svref output-vecks (1- lay-num)))
	     (num-prev-outputs (length prev-outputs))
	     (weight-array (svref weight-arrays (1- lay-num))))
	;; For each neuron in that layer...
	(dotimes (neuron-num (svref layer-sizes lay-num))
	  ;; Compute output value of that neuron 
	  (setf (svref outputs neuron-num)
	    ;; SIGMOID of the DOT-PRODUCT of WEIGHTS and INPUT VALUES
	    ;;  (INPUTS for this neuron are OUTPUTS from neurons 
	    ;;     in previous layer)
	    (sigmoid (let ((dot-prod 0))
		       (dotimes (j num-prev-outputs)
			 (incf dot-prod
			       (* (svref prev-outputs j)
				  (aref weight-array j neuron-num))))
		       dot-prod))))))))



;;;  TRAIN-ONE
;;; ----------------------------------------------------
;;;  INPUTS:  NN, a neural network
;;;           ALPHA, a small positive number that specifies the sensitivity
;;;             of updates to the error
;;;           INPUTS, a list of input values for the neurons in the input layer
;;;           TARGET-OUTPUTS, the desired outputs for neurons in the output
;;;             layer, given the specified INPUTS.
;;;  OUTPUT:  NN
;;;  SIDE EFFECT:  Uses FEED-FORWARD to generate output values for
;;;   the given inputs; then uses the BACK-PROPAGATION algorithm to
;;;   generate DELTA values for each neuron (starting from output layer
;;;   and working back to first hidden layer); then uses the DELTA values
;;;   to update each non-input neuron.

(defun train-one (nn alpha inputs target-outputs)
  (feed-forward nn inputs)

  ;; Back prop algorithm...
  (let* ((num-layers (nn-num-layers nn))
	 (layer-sizes (nn-layer-sizes nn))
	 ;; The index for the output layer
	 (last-layer-index (1- num-layers))
	 (num-output-neurons (svref layer-sizes last-layer-index))
	 ;; The index for the layer just before the output layer
	 (penult-layer-index (1- last-layer-index))
	 ;;(num-penult-neurons (svref layer-sizes penult-layer-index))
	 (output-vecks (nn-output-vecks nn))
	 ;;(penult-output-veck (svref output-vecks penult-layer-index))
	 (last-output-veck (svref output-vecks last-layer-index))
	 (delta-vecks (nn-delta-vecks nn))
	 (last-delta-veck (svref delta-vecks last-layer-index))
	 (weight-arrays (nn-weight-arrays nn))
	 ;;(last-weight-array (svref weight-arrays penult-layer-index))
	 )
    
    ;; for each neuron in the output layer:
    (dotimes (neuron-num num-output-neurons)
      (let* ((target-output (nth neuron-num target-outputs))
	     (my-output  (svref last-output-veck neuron-num))
	     (diffy (- target-output my-output)))
	;;   DELTA_J = G'(IN_J) * (Y_J - A_J)
	;;           = G(IN_J)*(1 - G(IN_J))*(Y_J - A_J)
	;;           = A_J * (1 - A_J) * (Y_J - A_J)
	(setf (svref last-delta-veck neuron-num)
	  (* my-output (- 1 my-output) diffy))))
    
    ;; for each hidden layer...
    (do ((lay-num penult-layer-index (1- lay-num)))
	;; exit
	((= lay-num 0))
      ;; BODY of DO
      ;; ---------------------------
      (let* ((num-neurons (svref layer-sizes lay-num))
	     (curr-out-veck (svref output-vecks lay-num))
	     (next-delta-veck (svref delta-vecks (1+ lay-num)))
	     (my-delta-veck (svref delta-vecks lay-num))
	     (num-neurons-next-layer (svref layer-sizes (1+ lay-num)))
	     (curr-weight-array (svref weight-arrays lay-num))
	     )
	;; for each neuron in that layer...
	(dotimes (i num-neurons)
	  ;; DELTA_I = G'(IN_I) SUM [W_I_J DELTA_J]
	  ;;         = G(IN_I) * (1 - G(IN_I)) * SUM [ W_I_J DELTA_J ]
	  ;;         = A_I * (1 - A_I) * SUM [ W_I_J DELTA_J ]
	  (let* ((my-output (svref curr-out-veck i))
		 (sum (let ((dotty 0))
			(dotimes (j num-neurons-next-layer)
			  (incf dotty (* (aref curr-weight-array i j)
					 (svref next-delta-veck j))))
			dotty)))
	    (setf (svref my-delta-veck i)
	      (* my-output (- 1 my-output) sum))))))
    
    ;; Now, update all of the weights in the network using the DELTA values
    ;;  For each layer...
    (dotimes (lay-num (1- num-layers))
      (let ((weight-array (svref weight-arrays lay-num))
	    (delta-veck (svref delta-vecks (1+ lay-num)))
	    (output-veck (svref output-vecks lay-num)))
	;; For each neuron N_i in that layer...
	(dotimes (i (svref layer-sizes lay-num))
	  ;; For each neuron N_j in the following layer...
	  (dotimes (j (svref layer-sizes (1+ lay-num)))
	    ;; Update the weight on the edge from N_i to N_j
	    ;; W_I_J += ALPHA * A_I * DELTA_J
	    (incf (aref weight-array i j)
		  (* alpha 
		     (svref output-veck i) 
		     (svref delta-veck j)))))))
      
    ;; return the NN
    nn))






;;need to be able to extract the teams and pitchers needed to run
;;the nn on and get the inputs

(defun train-nn-year
    (nn file)
  (let* ((year (csv-reader file))
	 (size (list-length year)))
    (dolist (game year)
      (let))))
    