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

; replace-nth-item (implimented in hw1)
(define (replace-nth-item pos lst item) 
	(cond 	((= pos 1) (cons item (cdr lst)))
		(#t (cons (car lst) (replace-nth-item (- pos 1) (cdr lst) item)))))

; nth-item (implimented in hw1)
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

;
