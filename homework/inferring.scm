; Christopher Wright

; Basic Assignment 
; NOTE: need to check if it is okay to return elements in list out of order provided

; atom? checks to see if the passed in element is a single element, or a pair
; This is used for our NOT functionality
(define (atom? element) (not (pair? element)))

(define (NOT element) 
	(cond 	((atom? element) (list 'NOT element))
		(#t (cadr element))))

(define (resolve c1 c2)
	(if (can-unify? c1 c2)
	(let ((ans (recurse-c1 c1 c2))) 
		(cond 	((null? ans) 'CONTRADICTION)
			(#t ans))) 
	#f))

(define (recurse-c1 c1 c2) 
	(cond 	((null? c1) #f)
		(#t (let ((ans (recurse-c2 c1 c2))) 
			(if ans ans (let ((ans2 (recurse-c1 (cdr c1) c2))) 
				(if ans2 (cons (car c1) ans2) #f))))))) 	

; resolve helper takes in two lists, c1 is guranteed to have at least 1 element
; if can unify first element of c1, with any element of c2, returns unified list
; otherwise returns false.
(define (recurse-c2 c1 c2)
	(cond 	((null? c2) #f)
		((equal? (NOT (car c1)) (car c2)) (append (cdr c1) (cdr c2)))
		(#t (let ((ans (recurse-c2 c1 (cdr c2)))) 
			(if ans (cons (car c2) ans) #f)))))
(define (can-unify? c1 c2)
	(if (and (element-in-common c1 c2) (distinct-element c1 c2)) #t #f))

(define (element-in-common c1 c2)
	(cond 	((null? c1) #f)
		(#t (if (contains? c2 (NOT (car c1))) #t (element-in-common (cdr c1) c2)))))

(define (distinct-element c1 c2)
	(cond 	((null? c1) #f)
		(#t (if	(contains? c2 (NOT (car c1))) (distinct-element (cdr c1) c2) #t))))

;pre: none
;post: returns true if lst contains item
(define (contains? lst item)
	(cond 	((null? lst) #f)
		((equal? (car lst) item) #t)
		(#t (contains? (cdr lst) item))))

; Intermediate Assignment

; Define Knowledge Base 
; KB is a list, each element in list is statement in CNF form
(define KB '())

(define (tell statement)
	(set! KB (cons statement KB))) 
