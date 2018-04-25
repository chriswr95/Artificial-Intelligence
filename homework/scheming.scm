;Christopher Wright
; HW 1

; find-biggest
(define (find-biggest lst)
	(cond	((null? (cdr lst)) (car lst))
		((< (car lst) (find-biggest (cdr lst))) (find-biggest (cdr lst)))
		(#t (car lst))))

; count-from
(define (count-from from to)
	(cond	((< from to) (begin (display from) (display "\n") (count-from (+ from 1) to)))
		(#t (begin (display from) (display "\n")))))

; nth-item
(define (nth-item pos lst)
	(cond 	((= pos 1) (car lst))
		(#t (nth-item (- pos 1) (cdr lst)))))
	
; replace-nth-item
(define (replace-nth-item pos lst item) 
	(cond 	((= pos 1) (cons item (cdr lst)))
		(#t (cons (car lst) (replace-nth-item (- pos 1) (cdr lst) item)))))

; sorted?
(define (sorted? lst) 
	(cond  	((null? (cdr lst)) #t) ;return true if only one element in list
		((> (car lst) (cadr lst)) #f) ;return false if any part of list is unsorted
		(#t (sorted? (cdr lst))))) ;recurse with cdr of list
; apply-action
(define (apply-action state action) 
	(cond	((equal? action "STAY") state)
		((equal? action "MOVE-1") (move state 1))
		((equal? action "MOVE-2") (move state 2))
		((equal? action "MOVE-3") (move state 3))
		(#t (turn-wrapper state action))))
				
(define (turn-wrapper state action) 
	(list (car state) (cadr state) (convert-direction (turn (convert-direction (caddr state)) action))))

(define (move state distance)
	(cond 	((equal? (cddr state) "N") 
			(cons (car state)(cons (+ (cadr state) distance)(cddr state))))
		((equal? (cddr state) "S") 
			(cons (car state)(cons (- (cadr state) distance)(cddr state))))
		((equal? (cddr state) "E")
			(cons (+ distance (car state)) (cdr state)))
		(#t ; facing "W"
			(cons (- distance (car state)) (cdr state)))))

(define (convert-direction dir)
	(cond 	((equal? dir "N") 0)
		((equal? dir "E") 1)
		((equal? dir "S") 2)
		((equal? dir "W") 3)
		((= dir 0) "N")
		((= dir 1) "E")
		((= dir 2) "S")
		(#t "W")))

(define (turn direction action)
	(cond 	((equal? action "TURN-RIGHT") (modulo (+ direction 1) 4))
		((equal? action "TURN-LEFT)") (modulo (- direction 1) 4))
		((equal? action "TURN-AROUND") (modulo (+ direction 2) 4)))) 
	
