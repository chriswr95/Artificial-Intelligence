(define (a* search frontier)
	(let 	(best( (find-smallest-a* frontier)))
		(cond 	((is-solution best) best)
			(#t (a*search 
				(append (remove-from frontier best) 
					(get-children best)))))))


