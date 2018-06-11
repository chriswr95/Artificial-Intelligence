; Christopher Wright
; Explore Exploit Bot
;
; About: This bot operates on a simplified explore exploit tradeoff. The explore behavoir takes 3 turns,
; during which the bot turns right three times. On each turn the bot stores information about
; predators, and vegitation. At the end of the explore phase, the bot commences the exploit behavoir
; patter. During the exploit phase, the bot first runs a heuristic to identify a target piece of 
; vegitation. One the target has been identified, it moves to the target vegitation and then attempts
; to eat. 

; Future Work:
; 1. Keep data around when plants are blooming, and only eat if a plant is in bloom. 
; 2. Look for other agents, and stay away from them. 
; 3. Look for predators, and factor in avoiding them to the heuristic.

; Initialize persistant information

(define predators '())

(define vegetation '())

(define explore? #t) 

(define target #f)

(define coordinates '(0 0 0)) ; (DIRECTION XPOS YPOS)

(define passive #f)

(define arrived-at-plant? #f)

; Main Functions

(define (initialize-agent)
        "OK")

; Triage three possible behavoirs
; Idea is to alternate between explore and exploit, interrupting every time we are attacked
; to avoid loosing more energy.

(define (choose-action current-energy previous-events percepts)
	(cond 	((attacked? previous-events) (evade percepts))
		(explore? (begin (init-coordinates) (update-explore?) (gather-info percepts))) 
		(#t (exploit))))
; Evade Behavoir
(define (attacked? previous-events) 
	(cond 	((null? previous-events) #f)
		((equal? (caar previous-events) 'attacked-by)  #t)
		(#t (attacked? (cdr previous-events))))) 

(define (evade percept) 
	(cond 	((front-blocked? percept) "TURN-RIGHT")
		(#t "MOVE-PASSIVE-2")))

(define (front-blocked? percept) 
	(if (equal? (cadar percept) 'empty) #f #t))

; Explore behavoir
; This behavoir takes four turns, gathering information about the surrounding enviornment. It stores
; relative locations of preditors, and vegitation.

(define (update-explore?) 
	(if (equal? (car coordinates) 3) (set! explore? #f) "do nothing"))

(define (gather-info percepts)
	(cond 	((null? percepts) (begin 
			(incriment-dir-coordinate)
			"TURN-RIGHT")) 
		(#t (begin
			(analyze-row (car percepts)) 
			(incriment-y-coordinate)
			(reset-x-coordinate)
			(gather-info (cdr percepts))))))

(define (analyze-row row)
	(cond 	((null? row) "done")
		(#t (begin
			(analyze (car row))
			(incriment-x-coordinate)
			(analyze-row (cdr row))))))

(define (analyze percept)
	(cond 	((equal? percept 'empty) "do nothing")
		((equal? percept 'barrier) "do nothing")
		((equal? (car percept) 'vegetation) 
			(set! vegetation (cons (list coordinates percept) vegetation)))
		((equal? (car percept) 'predator)
			(set! predators (cons (list coordinates percept) predators)))
		(#t "done")))
	

; Coordinate operations
(define (init-coordinates) (set! coordinates (list (car coordinates) -1 1)))

(define (incriment-x-coordinate) 
	(set! coordinates (list (car coordinates) (+ 1 (cadr coordinates)) (caddr coordinates))))

(define (reset-x-coordinate) 
	(set! coordinates (list (car coordinates) (* -1 (caddr coordinates)) (caddr coordinates))))

(define (incriment-y-coordinate)
	(set! coordinates (list (car coordinates) (cadr coordinates) (+ 1 (caddr coordinates)))))

(define (incriment-dir-coordinate)
	(set! coordinates (list (+ 1 (car coordinates)) (cadr coordinates) (caddr coordinates))))

; Exploit Behavoir
; This behavoir pattern runs a heuristic to evaluate the most optimal piece of vegitation. Then
; executes the moves necissary to travel to the desired location. If we end up in front of a plant,
; we will attempt to eat from that plant.
(define (exploit) 
	(cond 	
		(arrived-at-plant? (plant-protocol))
		(target (begin (go-to-target)))
		(#t 	(begin (identify-target) (exploit)))))

(define (go-to-target) 
		(go-to-target-helper (car target) (cdr target)))

(define (go-to-target-helper move move-set)
	(begin (if (null? move-set) 
			(begin (set! target #f) (clear-explore-data)) 
			(set! target move-set))
			move))
		
(define (clear-explore-data)
	(begin 	(set! coordinates '(0 0 0)) 
		(set! predators '()) 
		(set! vegetation '()) 
		(set! arrived-at-plant? #t)
		))

(define (identify-target)
	(if (null? vegetation) (set! target 
		'("MOVE-PASSIVE-1" "MOVE-PASSIVE-1" "MOVE-PASSIVE-1")) 
	(set! target (heuristic (map get-moves vegetation))))) ; pass all possible move-sets to the heuristic

; The heuristic simply returns the move set to the closest vegitation. 
(define (heuristic move-set) 
	(heuristic-helper (car move-set) (cdr move-set)))

(define (heuristic-helper best-move move-set)
	(cond 	((null? move-set) best-move)
		((< (length (car move-set)) (length best-move))
			(heuristic-helper (car move-set) (cdr move-set)))
		(#t (heuristic-helper best-move (cdr move-set)))))

(define (get-moves element) 
	(if (= (cadar element) 0)
		(get-moves-helper (caar element) (cadar element) (- (caddar element) 1) '())
		(get-moves-helper (caar element) (cadar element) (caddar element) '())))

(define (get-moves-helper dir x y moves)
	(cond 	
		((> dir 0) (get-moves-helper (- dir 1) x y (cons "TURN-RIGHT" moves)))
		((> y 0) (get-moves-helper dir x (- y 1) (move-1 moves)))
		((< x 0) (get-moves-helper dir 0 (- (* -1 x) 1) (cons "TURN-LEFT"  moves)))
		((> x 0) (get-moves-helper dir 0 (- x 1) (cons "TURN-RIGHT" moves)))
		((null? moves) (cons "STAY" moves))
		(#t (reverse moves))))

(define (plant-protocol) 
	(begin 	(set! explore? #t) 
		(set! arrived-at-plant? #f) 
		"EAT-AGGRESSIVE"))
		 



; Actions
(define (move-1 moves) 
	(cond 	(passive (cons "MOVE-PASSIVE-1" moves))
		(#t (cons "MOVE-AGGRESSIVE-1" moves))))

