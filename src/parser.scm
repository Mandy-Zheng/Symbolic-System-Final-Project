(define (print . args)
  (let lp ((statement args))
    (if (not (= (length statement) 0))
	(begin (display (car statement))
	(display "\n")
	(lp (cdr statement)))))
  )

(define sharp "#")
(define flat "b")
(define double-sharp "##")
(define double-flat "bb")

#|
An accidental predicate where it returns true if the symbol is '# or 'b.
|#
(define (accidentals? expr)
  (or (string=? expr sharp)
      (string=? expr flat))
  )
(accidentals? "#") ;#t
(accidentals? "b") ;#t
(accidentals? "##") ;#f
(accidentals? "###") ;#f

#|
A double accidental predicate where it returns true if the symbol is '## or 'bb.
|#

(define (double-accidentals? expr)
  (or (string=? expr double-sharp)
      (string=? expr double-flat))
  )

(double-accidentals? "b") ; #f
(double-accidentals? "bb") ; #t
(double-accidentals? "##") ; #t
(double-accidentals? "bbb") ;#f

#|
A letter predicate where it returns true if the symbol is a letter from A-G.
|#
(define (letter? expr)
  (member expr (list "A" "B" "C" "D" "E" "F" "G"))
  )


(letter? "A") ;("A" "B" "C" "D" "E" "F" "G")
(letter? "a") ;#f
(letter? "C") ; ("C" "D" "E" "F" "G")
(letter? "Ab") ;#f
(letter? "F") ;("F" "G")
(letter? "G") ;("G")


(define (get-letter pitch)
  (substring pitch 0 1)
  )

(define (get-accidentals pitch)
  (cond ((and (> (string-length pitch) 2) (double-accidentals? (substring pitch 1 3))) (substring pitch 1 3))
	((accidentals? (substring pitch 1 2)) (substring pitch 1 2))
	(else ""))
  )

(define (get-octave pitch)
  (let ((start-idx (+ (string-length (get-letter pitch)) (string-length (get-accidentals pitch)))))
    (substring pitch start-idx (string-length pitch))
    )
  )

(get-letter "A#3") ; A
(get-accidentals "A#3") ; #
(get-octave "A#3") ; 3

(get-letter "G2") ; G
(get-accidentals "G2") ; ''
(get-octave "G2") ; 2

(get-letter "Fbb10") ; F
(get-accidentals "Fbb10") ; bb
(get-octave "Fbb10") ; 10

(define (get-first-pitch note)
  (car note))

(define (get-frequency note)
  (list-ref note (- (length note) 1)))


#|
A pitch predicate where it returns true if the input is a valid symbol following the CFL:
pitch ::= letter accidental? octave 
letter ::= A | B | C | D | E | F | G
accidental ::= # | ## | b | bb
octave ::= [0-9]+
|#
(define (pitch? str)
  (let ((len (string-length str)))
    (cond ((< len 2) #f)
	  ((= len 2) (and (letter? (substring str 0 1)) (string->number (substring str 1 2))))
	  (else
	   (let ((letter (substring str 0 1))
		 (accidental-double (substring str 1 3))
		 (possible-octave-1 (substring str 3 len)))
	     (if (and (letter? letter) (double-accidentals? accidental-double) (string->number possible-octave-1))
		 #t
		 (let ((accidental-single (substring str 1 2))
		       (possible-octave-2 (substring str 2 len))
		       (possible-octave-3 (substring str 1 len)))
		   (print (letter? letter) (accidentals? accidental-single) (string->number possible-octave-2))
		   (if (and (letter? letter) (accidentals? accidental-single) (string->number possible-octave-2))
		       #t
		       (and (letter? letter) (number? possible-octave-3)))
		   )
		 )
	     )
	   )
	  )))

(pitch? "A#3") ;#t
(pitch? "#3") ;#f
(pitch? "f##3") ;#f
(pitch? "G2") ;#t
(pitch? "Cbb") ;#f
(pitch? "Cbb3") ;#t
(pitch? "F##17") ;#t
(pitch? "2") ;#f

(define (frequency? expr)
  (and (string->number expr) (>= (string->number expr) 0))
  )

(frequency? "1") ;#t
(frequency? "4") ;#t
(frequency? "1/3") ;#t
(frequency? "a1") ;#f
(frequency? "-4") ;#f
(frequency? "0") ;#t

#|
A note predicate where it is represented by a pitch and then a duration for teh note.
Return true if the symbol follows this pattern, else false.
|#
(define (note? expr)
  (print (>= (length expr)))
  (and (list? expr) (>= (length expr) 2)  ;; check list and at least one note and duration
       (let lp ((expr expr))
	 (if (= 1 (length expr))
	     (frequency? (car expr)) ;; last item is note duration
	     (and (pitch? (car expr)) (lp (cdr expr))) ;; check every item before frequency is a valid pitch
	     )
	 )
       )
  )


(note? (list "A#2" "3")) ;#t
(note? (list "Cb3" "G##2" "2")) ;#t
(note? (list "Cb3" "G##2")) ;#f
(note? (list "2")) ;#f
(note? (list "Bbb4" "D##2" "F2" "7")) ;#t


;;TODO: should we make meta-info more specific? like key and time signature etc
#|
A meta-info predicate, all elements must be strings or number.
|#
(define (meta-info? expr)
  (list? expr)
  (let lp ((expr expr))
    (if (null? expr)
	#t
	(and (or (number? (car expr)) (string? (car expr)))
	     (lp (cdr expr))))))

(meta-info? (list "hello" 1)) ; #t
(meta-info? (list "hello" 1 (list 1))) ; #f   

(define (get-notes-in-measure measure)
  (cadr measure)
  )
#|
A measure predicate, at least two notes as arguments.
Return true if at least two notes, else false.
|#
(define (measure? . expr)
  (>= 2 (length expr)) ;; at least 2 args
  (define (check-elements elts)
    (if (null? elts) ;; empty
	#t
	(and (note? (car elts)) (check-elements (cdr elts)))))
  (if (meta-info? (car expr)) ;; has optional
      (and (>= 2 (length (cdr expr))) ;; at least two notes
	   (check-elements (cdr expr)))	
      (check-elements expr)))


(measure? (list "A#4" "Bb3" "2") (list "G#2" "Bb1" "2")) ;#t	
(measure? (list "A#4" "Bb3" "2")) ; #t


#|
A section predicate, at least two measures as arguments.
Return true if at least two measures, else false.
|#
; TODO: no longer symbols
; TODO: first letter must be capitalized
(define (section? . expr)
  (>= 2 (length expr)) ;; at least 2 args
  (define (check-elements elts)
    (if (null? elts) ;; empty
	  #t
	  (and (measure? (car elts)) (check-elements (cdr elts)))))
  (check-elements expr))

					; TODO: fix test cases
;;TODO measure = ((meta info) (notes)))
(measure? (list (list ‘a#4 ‘bb3 2) (list ‘g# ‘b 2)) (list (list ‘a#4 ‘bb3 2) (list ‘g# ‘b 2)))	
(measure? (list (list ‘a#4 ‘bb3 2) (list ‘g# ‘b 2)))

; TODO: intermediate fn

; (add (meta-info ...) (A#4 Bb3 2) (G#2 Bb1 2) | (A#4 Bb3 2) (G#2 Bb1 2) |)

; only meta-info for measure and section

(load-option 'synchronous-subprocess)
(define (open-pdf file-path)
  (run-shell-command (string-append "open " file-path))) ;;see if it works on macs
(open-pdf "score.pdf")
