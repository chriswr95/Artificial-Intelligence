; Christopher Wright
; Hw 2

; import
(load "map.scm")

; pre: a and b must be numbers, lst is a list 
; post: the items at index a and b are switched
(define (swap-elements a b lst) 
	(cond 	((= a 1) (swap-helper b lst))
		((= b 1) (swap-helper a lst))
		(#t (cons (car lst) (swap-elements (- a 1) (- b 1) (cdr lst))))))

(define (swap-helper pos lst)
	(replace-nth-item pos (replace-nth-item 1 lst (nth-item pos lst)) (car lst)))

; replace-nth-item was implimented in hw1
(define (replace-nth-item pos lst item) 
	(cond 	((= pos 1) (cons item (cdr lst)))
		(#t (cons (car lst) (replace-nth-item (- pos 1) (cdr lst) item)))))

; nth-item was implimented in hw1
(define (nth-item pos lst)
	(cond 	((= pos 1) (car lst))
		(#t (nth-item (- pos 1) (cdr lst)))))

; pre: none
; post: returns true if two items are adjascent in map.scm, else returns false 
(define (is-adjacent? a b) (adjacency-helper a b adjacency-map))

(define (adjacency-helper a b lst)
	(cond 	((equal? (caar lst) a) (contains? (car lst) b))
		(#t (adjacency-helper a b (cdr lst)))))
;pre: none
;post: returns true if lst contains item
(define (contains? lst item)
	(cond 	((null? lst) #f)
		((equal? (car lst) item) #t)
		(#t (contains? (cdr lst) item))))


; pre: give a state
; post: a list of all children of that state
(define (get-children state)
	(map (lambda (pair) 
		(get-child state (car pair) (cadr pair))
	) (enumerate-combinations (length (car state)))))

(define (get-child state a b)
	(list (swap-elements a b (car state)) (append (cadr state) (list (list a b)))))

;this function returns all the possible swaps of a list of n elements
(define (enumerate-combinations n)
	(cond 	((= n 1) '())
		(#t (enumerate-helper1 1 n))))
	
(define (enumerate-helper1 a b) 
	(cond	((= a b) '())
		(#t (append (enumerate-helper2 a (+ a 1) b) (enumerate-helper1 (+ a 1) b)))))

(define (enumerate-helper2 a b c) 
	(cond	((= b c) (cons (list a b) '()))
		(#t (cons (list a b) (enumerate-helper2 a (+ b 1) c)))))

; pre: give a state
; post: true if state is a goal state, else false
(define (goal-state? state) (goal-state-helper (car state)))

(define (goal-state-helper lst)
	(cond 	((= (length lst) 1) #t)
		((is-adjacent? (car lst) (cadr lst)) (goal-state-helper (cdr lst)))
		(#t  #f))) 

; pre: lst is a list of states, depth is the maximum number of swaps
; post: output a single goal state, or #f if there is no goal state reachable via DFS
(define (dfs lst depth)
;	(begin (display "\n state: ") (display (car lst)) (display "\n") (display "depth: ")
;		(display depth) (display " length: ") (display (length (cadar lst)))  
	(cond 	((null? lst) #f)
		((goal-state? (car lst)) (car lst))
		((equal? (length (cadar lst)) depth) (dfs (cdr lst) depth))
		(#t (dfs  (append (get-children (car lst)) (cdr lst)) depth))))

; id-dfs is a wrapper for the helper function that converts the input into a frontier (list of states)
; and then passes to id-dfs-helper
(define (id-dfs input) ;input is different from a state, bc doesn't contain list of swaps
	(cond 	((null? input) (begin (display "Error no states input\n") #f))
		(#t (id-dfs-helper (list (list input '())) 1 (length input)))))

; id-dfs-helper calls iteratively deepening DFS on the initial frontier up to a depth equal to the 
; number of states in the list.
(define (id-dfs-helper init-frontier current-depth max-depth)
	(cond 	((= current-depth max-depth) (dfs init-frontier current-depth)) ;final dfs
		(#t (let ((ans (dfs init-frontier current-depth)))
			(if ans ans (id-dfs-helper init-frontier (+ current-depth 1) max-depth))))))

; pre: input is a list of states, ie (California Washington Alaska)
; post: the list sorted so that every state is geographically adjacent to every state next to
; it in the list. As well as a list of swaps in order to transition from input to output.
; NOTE UNFINISHED: I could not figure out how to impliment the heuristic, but have included as much of my
; code as possible. 
(define (A* input) 
	(A*-helper (list (list input '()))))

(define (A*-helper frontier)
	(cond	((goal-state? (car frontier)) (car frontier))
		(A*-helper (heuristic (append (get-children (car frontier)) (cdr frontier))))))

(define (heuristic frontier)
	(swap-elements 0 (best-index frontier) frontier))

; best-index should return the index of the state with the lowest combined cost and heuristic. 
; I could not figure out how to impliment this.
(define (best-index frontier) 0)


