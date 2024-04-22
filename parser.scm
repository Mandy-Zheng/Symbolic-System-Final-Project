(define (sharp) '#)
(define (flat) 'b)
(define (double-sharp) '##)
(define (double-flat) 'bb)

(define (accidentals? expr)
  (or (eqv? expr sharp)
      (eqv? expr flat)
      (eqv? expr double-sharp)
      (eqv? expr double-flat))
  )

(define letter? expr
  (member expr (list 'A 'B 'C 'D 'E 'F 'G))
  )


(define (pitch? expr)
  (let ((str (symbol->string expr))
	(len (length expr)))
    (let ((letter (string->symbol (substring str 0 1)))
	  (accidental-double (string->symbol (substring str 1 3)))
	  (possible-octave (string->symbol (substring str 3 len))))
      (if (and (letter? letter) (accidentals? accidental-double) (number? possible-octave-1))
	  #t
	  (let ((accidental-single (string->symbol (substring str 1 2)))
		(possible-octave-2 (string->symbol (substring str 2 len))))
	    (and (letter? letter) (accidentals? accidental-single) (number? possible-octave-2))
	    )
	  )
      )
    )
  )

(define (frequency? expr) (display "To Do"))

(define (note? expr)
  (and (list? expr) (>= 2 (length expr))  ;; check list and at least one note and duration
       (let lp ((expr expr))
	 (if (= 1 (length expr))
	     (frequency? expr) ;; last item is note duration
	     (and (pitch? (car expr)) (lp (cdr expr))) ;; check every item before frequency is a valid pitch
	     )
	 )
       )
  )



#|
A measure predicate, at least two notes as arguments.
Return true if at least two notes, else false.
|#
(define (measure? . expr)
  (>= 2 (length expr)) ;; at least 2 args
  (define (check-element elts)
    (if (null? elts) ;; empty
	#t
	(and (note? (car elts)) (check-element (cdr elts)))))
  (check-elements expr))

(measure? (list ‘a#4 ‘bb3 2) (list ‘g# ‘b 2))	
(measure? (list ‘a#4 ‘bb3 2))
