(load "datastructures.fas")
(load "auxfuncs.fas")

		  
(defvar *track*)
(setf *track* (loadtrack "track1.txt"))


(defvar prev-goal-state)
(setf prev-goal-state 
  (make-STATE :POS '(2 13)
	      :VEL '(0 4)
	      :ACTION '(1 1)
	      :COST 1
	      :TRACK *track*
	      :OTHER NIL))


(defvar non-goal-state)
(setf non-goal-state 
  (make-STATE :POS '(3 6)
	      :VEL '(-1 2)
	      :ACTION '(-1 0)
	      :COST 1
	      :TRACK *track*
	      :OTHER NIL))

;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st) 
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
	(track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
	 T)))

;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
	  (make-vel (+ (vel-l (state-vel st)) (acce-l act))
		    (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
	  (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
		    (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
	  (cond ((isGoalp new-state) -100)
		((isObstaclep (state-pos new-state) (state-track new-state)) 20)
		(T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
					    (pos-c (state-pos st)))))
    (values new-state)))



;; Solution of phase 2

;;; Pedir 
(defun nextStates (st)
  "generate all possible next states"

  (let ((lista '()))
    (dolist (el (possible-actions) lista)
		(push (nextState st el) lista))))

;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim)
	(let ((objective (returnlimdepthfirstsearchnode (problem-initial-state problem) problem lim lim)))
		(if (equal objective ":corte")
			":corte"
			(retiraUltimo objective))))
	
				      

;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem)
  
  (let ((depth 0))
	(loop
		(let ((result (limdepthfirstsearch problem depth)))
			(if (not (null result))
				(if (not (equal result ":corte"))
					(return result)))
		(setf depth (1+ depth))))))


(defun ultimo (l)
	;"Retorna o ultimo elemento de uma lista nao vazia"
	(cond ((null (rest l)) (first l))
		(t (ultimo (rest l)))))

(defun retira (n l)

	(cond ((null l) ())    
		((eq n (first l)) (rest l))
		(t (cons (first l) (retira n (rest l))))))
		
(defun retiraUltimo (l)
	(retira (ultimo l) l))

(defun returnlimdepthfirstsearchnode (state problem lim origLim)

  (if (funcall (problem-fn-isGoal problem) state)

    (list state)


    (if (= 0 lim) 
      ":corte"


      (let ((lista (funcall (problem-fn-nextStates problem) state)) (cutoff 0) (acabou 0) (objetivo '()))
		
        (dolist (el lista nil)
          (let ((result (returnlimdepthfirstsearchnode el problem (1- lim) origLim)))
			  (if (equal result ":corte")
				(setf cutoff 1)
				(if (not (null result))
				  (setf acabou 1)))
			  (if (= acabou 1)
			     (return (setf objetivo (cons el result))))
        ))
		(if (not (= acabou 1))
			(if (= 1 cutoff)
				":corte"
				nil))
		(if (= origLim lim)
			(cons state objetivo)
			objetivo)
	  ))))














