
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
(load "datastructures.fas")
(load "auxfuncs.fas")



(defun somaListas (l1 l2)
	;soma elementos de l2 a l1
	(if (null l1)
		l2
		(cons (+ (first l1) (first l2)) (somaListas (rest l1) (rest l2)))))
		

(defun isInList (pos lis)
	;devolve T se pos esta na lis e NIL caso contrario
	(if (null lis)
		nil
		(if (equal pos (first lis))
			t
			(isInList pos (rest lis)))))
			
(defun verificaEndPosition (pos track)
	;verifica se pos e uma endposition de track
	(if (not (isInList pos (track-endpositions track)))
		nil
		t))
	

(defun definepos (pos vel track)
	;calcula a nova posicao ao somar vel a posicao antiga. Se sair da pista a posicao mantem-se inalterada
	(if (isObstaclep (somaListas pos vel) track)
		pos
		(somaListas pos vel)))

		
		
		
(defun ultimo (l)
	;"Retorna o ultimo elemento de uma lista nao vazia"
	(cond ((null (rest l)) (first l))
		(t (ultimo (rest l)))))

(defun isObstaclep (pos track) 
	;"check if there is an obstacle at position pos of the track"
	(if (not (nth (ultimo pos) (nth (first pos) (track-env track))) )
		t
		nil))


(defun isGoalp (st) 
	;"check if st is a goal state"
	(if (verificaEndPosition (state-pos st) (state-track st))
		t
		nil)
)
  	

(defun nextState (st act)
	;"generate the nextState after state st and action act"
	(defvar newState)
	(setf newState
		(make-STATE :POS (state-pos st)
			:VEL (somaListas (state-vel st) act)
			:ACTION act
			:COST NIL
			:TRACK (state-track st)
			:OTHER NIL))
	(if (equal (definepos (state-pos st) (state-vel newState) (state-track st)) (state-pos st)) ;verifica se a nova posicao sai da pista.
		(setf (state-vel newState) '(0 0))														;se sim define a velocidade a 0
		(setf (state-pos newState) (somaListas (state-pos st) (state-vel newState))))			;caso contrario define a nova posicao somando a posicao anterior a velocidade
	(if (isGoalp newState) 						;verifica se a nova posicao e um objetivo
		(setf (state-cost newState) -100)		;se sim retira 100 ao custo
		(if (equal (state-vel newState) '(0 0))	;se a velocidade for 0 e porque saiu da pista e portanto o custo sera 20
			(setf (state-cost newState) 20)
			(setf (state-cost newState) 1))) 	;caso contrario e um movimento regular e portanto adiciona 1 ao custo
	newState
)


