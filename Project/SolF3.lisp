(load "datastructures.lisp")
(load "auxfuncs.lisp")

(defvar matriz_heuristica nil) ;;;Lista de listas usada para guardar a distancia ao fim de cada posicao na funcao compute-heuristic-Best-Search.

(defstruct AuxPosition   ;;;Estrutura criada de form a facilitar o calculo das distancias em que APos = Posicao na track e G_cost = distancia do estado final
  APos
  G_cost)

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
  (let ((lista '()))
    (dolist (el (possible-actions) lista)
		(push (nextState st el) lista))))

;; Solution of phase 3

;; Heuristic



(defun membro (m l) ;;;Funcao que verifica se m pertence a lista l

	(if (null l)
		nil
		(if (equal m (first l))
			t
			(membro m (rest l))  
			))

)

	
(defun somapos (pos1 pos2 ) ;;;Funcao auxiliar a calculaEstados
	(list (+ (nth 0 pos1) (nth 0 pos2)) (+ (nth 1 pos1) (nth 1 pos2))))



(defun calculaEstados (Pos)  ;;;Funcao que calcula as posicoes adjacentes
	(let* ((nex_states nil)
		   (possible_actions (list '(0 1) '(1 0) '(-1 0) '(0 -1) '(1 -1) '(-1 1) '(1 1) '(-1 -1)))
		   (nextPos nil))
		
		(dolist (el possible_actions nex_states)
			(setf nextPos (make-AuxPosition :APos (somapos (AuxPosition-APos Pos) el) :G_cost (+ (AuxPosition-G_cost Pos) 1)))
			(push nextPos nex_states)
		)
	)
)

(defun memPos (Pos Lista) ;;;Funcao que verifica se a posicao se encontra na lista de AuxPosition
	(if (null Lista)
		nil
		(if (equal (AuxPosition-APos (first Lista)) (AuxPosition-APos Pos))
			(first Lista)
			(memPos Pos (rest Lista))))
)
  
 
(defun returnG_cost (Pos)  ;;;Getter
	(AuxPosition-G_cost Pos)
)

(defun returnF_cost (node)  ;;;Getter
	(node-f node)
)

(defun returnPosValue (Pos)  ;;;Getter
	(nth (nth 1 Pos) (nth (nth 0 Pos) matriz_heuristica))
)

;;;A heuristica utilizada corresponde a uma Procura em Largura Primeiro de forma a visitar todos os estados ate ao estado final

(defun compute-heuristic (st)
	(let ((current_position nil)
		   (nos_abertos nil)
		   (nos_fechados nil)
		   (fim nil))
		   
		(if (membro (state-pos st) (track-endpositions (state-track st)))
			(return-from compute-heuristic 0))
			
		(if (isObstaclep (state-pos st) (state-track st))
			(return-from compute-heuristic nil))
			
		(dolist (el (track-endpositions (state-track st)) nos_abertos)
			(let ((estado (make-AuxPosition :APos el :G_cost 0)))
			(push estado nos_abertos))
		)
			
		(loop
			(if (null nos_abertos)
				(return nil))
				
			(setf current_position (pop nos_abertos))
			(push current_position nos_fechados)
			(dolist (el (calculaEstados current_position) nil)
				(if (equal (state-pos st) (AuxPosition-APos el))
					(return (setf fim (AuxPosition-G_cost el))))
					
				(if (not (isObstaclep (AuxPosition-APos el) (state-track st)))
					(if (and (not (memPos el nos_abertos)) (not (memPos el nos_fechados)))
						(setf nos_abertos (append nos_abertos (list el)))
					)
				)
			)
			(if (not (null fim))
				(return))
		)
		fim	
	)
)

;;;Algoritmo A* que utiliza a Heuristica acima para o calculo do caminho optimo

;; A*
(defun a* (problem)
	(let* ((heuristica (funcall (problem-fn-h problem) (problem-initial-state problem)))
		  (current_node (make-node :parent nil :state (problem-initial-state problem) :f heuristica :g 0 :h heuristica))
		  (nos_abertos (list current_node))
		  (childNode nil))
		
		(loop
			(if (null nos_abertos)
				(return nil))
			(setf current_node (pop nos_abertos))
			
			(if (funcall (problem-fn-isGoal problem) (node-state current_node))
				(return (reconstruct_path current_node)))
			
			(dolist (el (funcall (problem-fn-nextStates problem) (node-state current_node)) nil)
				
				(setf heuristica (funcall (problem-fn-h problem) el))
				(when (not (null heuristica))
					(setf childNode (make-node :parent current_node :state el :f (+ (+ (node-g current_node) (state-cost el)) heuristica) :g (+ (node-g current_node) (state-cost el)) :h heuristica))
					(push childNode nos_abertos)
					(setf nos_abertos (sort nos_abertos #'< :key #'returnF_cost)))
				
			)

		)
	)
)

(defun reconstruct_path (current_node)  ;;;Funcao que devolve o caminho ate ao estado final
	(let* ((total_path (list)))
		(loop
			(if (null current_node)
				(return total_path)
			)
			
			(push (node-state current_node) total_path)
			(setf current_node (node-parent current_node))
		)
	)
)

;;;Para a funcao Best-Search utilizamos a mesma heuristica que utilizamos para o A* mas com uma pequena diferenca. A medida que
;;;ela calcula as distancias para as posicoes, guarda-as na variavel global matriz-heuristica, uma lista de listas que corresponde a uma track.
;;;Assim esta funcao torna-se um pouco mais lenta que a funcao compute-heuristic mas no entanto muito mais eficiente como se pode
;;;comparar pelos resultados obtidos. 
;;;Deste modo, basta chamar uma vez a funcao da heuristica para ficar com os valores de todas as posicoes que serao necessarias.
;;;Recorremos outra vez ao algoritmo A*, pois trata-se de o algoritmo mais eficiente em termos de tempo, mas com apenas uma chamada 
;;;a funcao da heuristica.
 
(defun compute-heuristic-Best-Search (st)
	(let ((current_position nil)
		   (nos_abertos nil)
		   (nos_fechados nil))
		   
		(setf matriz_heuristica (track-env (state-track st)))  
				
		(dolist (el (track-endpositions (state-track st)) nos_abertos)
			(let ((estado (make-AuxPosition :APos el :G_cost 0)))
			(push estado nos_abertos)
			(setf (nth (nth 1 (AuxPosition-APos estado)) (nth (nth 0 (AuxPosition-APos estado)) matriz_heuristica)) (AuxPosition-G_cost estado)))
		)
			
		(loop
			(if (null nos_abertos)
				(return nil))
				
			(setf current_position (pop nos_abertos))
			
			(push current_position nos_fechados)
			(dolist (el (calculaEstados current_position) nil)
				
				(if (not (isObstaclep (AuxPosition-APos el) (state-track st)))
					(when (and (not (memPos el nos_abertos)) (not (memPos el nos_fechados)))
						(setf nos_abertos (append nos_abertos (list el)))
						(setf (nth (nth 1 (AuxPosition-APos el)) (nth (nth 0 (AuxPosition-APos el)) matriz_heuristica)) (AuxPosition-G_cost el))
						
					)
				)
			)
		)
		
		(nth (nth 1 (state-pos st)) (nth (nth 0 (state-pos st)) matriz_heuristica))
	)
)


;; A*
(defun a*-Best-Search (problem)
	(let* ((current_node nil)
		  (nos_abertos nil)
		  (childNode nil)
		  (G_Value 0)
		  (H_Value 0))
		  
		(compute-heuristic-Best-Search (problem-initial-state problem))
		
		(setf H_Value (returnPosValue (state-pos (problem-initial-state problem))))
		(setf current_node (make-node :parent nil :state (problem-initial-state problem) :f H_Value :g G_Value :h H_Value))
		(push current_node nos_abertos)
		
		(loop
			(if (null nos_abertos)
				(return nil))
			(setf current_node (pop nos_abertos))
			
			(if (funcall (problem-fn-isGoal problem) (node-state current_node))
				(return (reconstruct_path current_node)))
			
			(dolist (el (funcall (problem-fn-nextStates problem) (node-state current_node)) nil)
				
				(setf H_Value (returnPosValue (state-pos el)))
				(setf G_Value (+ (node-g current_node) (state-cost el)))
				
				(when (and (not (null H_Value)) (not (equal t H_Value)))
					(setf childNode (make-node :parent current_node :state el :f (+ G_Value H_Value) :g G_Value :h H_Value))
					(push childNode nos_abertos)
					(setf nos_abertos (sort nos_abertos #'< :key #'returnF_cost)))
				
			)

		)
	)
)

