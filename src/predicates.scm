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
#|
(accidentals? "#") ;#t
(accidentals? "b") ;#t
(accidentals? "##") ;#f
(accidentals? "###") ;#f
|#

#|
A double accidental predicate where it returns true if the symbol is '## or 'bb.
|#

(define (double-accidentals? expr)
  (or (string=? expr double-sharp)
      (string=? expr double-flat))
  )
#|
(double-accidentals? "b") ; #f
(double-accidentals? "bb") ; #t
(double-accidentals? "##") ; #t
(double-accidentals? "bbb") ;#f
|#

#|
A letter predicate where it returns true if the symbol is a letter from A-G.
|#
(define (letter? expr)
  (member expr (list "A" "B" "C" "D" "E" "F" "G"))
  )

#|
(letter? "A") ;("A" "B" "C" "D" "E" "F" "G")
(letter? "a") ;#f
(letter? "C") ; ("C" "D" "E" "F" "G")
(letter? "Ab") ;#f
(letter? "F") ;("F" "G")
(letter? "G") ;("G")
|#

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
#|
(get-letter "A#3") ; A
(get-accidentals "A#3") ; #
(get-octave "A#3") ; 3

(get-letter "G2") ; G
(get-accidentals "G2") ; ''
(get-octave "G2") ; 2

(get-letter "Fbb10") ; F
(get-accidentals "Fbb10") ; bb
(get-octave "Fbb10") ; 10
|#

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
		   ;(print (letter? letter) (accidentals? accidental-single) (string->number possible-octave-2))
		   (if (and (letter? letter) (accidentals? accidental-single) (string->number possible-octave-2))
		       #t
		       (and (letter? letter) (number? possible-octave-3)))
		   )
		 )
	     )
	   )
	  )))
#|
(pitch? "A#3") ;#t
(pitch? "#3") ;#f
(pitch? "f##3") ;#f
(pitch? "G2") ;#t
(pitch? "Cbb") ;#f
(pitch? "Cbb3") ;#t
(pitch? "F##17") ;#t
(pitch? "2") ;#f
|#

(define (frequency? expr)
  (and (string->number expr) (>= (string->number expr) 0))
  )

#|
(frequency? "1") ;#t
(frequency? "4") ;#t
(frequency? "1/3") ;#t
(frequency? "a1") ;#f
(frequency? "-4") ;#f
(frequency? "0") ;#t
|#

#|
A note predicate where it is represented by a pitch and then a duration for teh note.
Return true if the symbol follows this pattern, else false.
|#
(define (note? expr)
  ; (print (>= (length expr)))
  (and (list? expr) (>= (length expr) 2)  ;; check list and at least one note and duration
       (let lp ((expr expr))
	 (if (= 1 (length expr))
	     (frequency? (car expr)) ;; last item is note duration
	     (and (pitch? (car expr)) (lp (cdr expr))) ;; check every item before frequency is a valid pitch
	     )
	 )
       )
  )


#|
(note? (list "A#2" "3")) ;#t
(note? (list "Cb3" "G##2" "2")) ;#t
(note? (list "Cb3" "G##2")) ;#f
(note? (list "2")) ;#f
(note? (list "Bbb4" "D##2" "F2" "7")) ;#t
|#


#|
Get a list of notes from the measure
|#
(define (get-notes-in-measure measure)
  (cdr measure))

#|
(pp (get-notes-in-measure (list (list "test" 1)
				(list (list "A#4" "Bb3" "2")
				      (list "G#2" "Bb1" "2")))))
;; (("A#4" "Bb3" "2") ("G#2" "Bb1" "2"))

|#


#|
A measure predicate, at least two notes as arguments.
Has 1 meta and a list of notes.
Return true if at least two notes and meta, else false.
|#
(define (measure? expr)
  (define (check-elements elts)
    (if (null? elts) ;; empty
	#t
	(and (note? (car elts)) (check-elements (cdr elts)))))
  (if (metadata? (car expr))
      (and (<= 2 (length (second expr))) ;; at least two notes
	   (check-elements (second expr)))
      #f))

#|
(measure? (list
	   (list "3" "4" (list "F" "major") "bass") ; meta
	   (list (list "A#4" "Bb3" "2")
		 (list "G#2" "Bb1" "2")))) ;; #t

;; meta with 1 note only
(measure? (list
	   (list "3" "4" (list "F" "major") "bass") ; meta
	   (list (list "A#4" "Bb3" "2")))) ; #f

;; no meta with 1 note only		
(measure? (list
	   (list (list "A#4" "Bb3" "2")))) ; #f


(measure? (list
	   (list (list "A#4" "Bb3" "2")
		(list "A#4" "Bb3" "2")))) ; #f

|#

#|
A section predicate, at least one measure as arguments.
Return true if at least one measure, else false.
|#
(define (section? expr)
	(define (check-elements elts)
    	(if (null? elts) ;; empty
		#t
		(and (measure? (car elts)) (check-elements (cdr elts)))))
	(and (<= 1 (length expr)) ;; at least two measures
	   (check-elements expr)))
#|

(section? (list (list
					(list "test" 1) ; meta
					(list
						(list "A#4" "Bb3" "2")
						(list "G#2" "Bb1" "2"))))) ; -> #t

(section? (list (list
					(list "test" 1) ; meta
					(list
						(list "A#4" "Bb3" "2")
						(list "G#2" "Bb1" "2")))
				(list
					(list "test" 1) ; meta
					(list
						(list "A#4" "Bb3" "2")
						(list "G#2" "Bb1" "2"))))) ; -> #t

; malformed measure
(section? (list (list
					(list
						(list "A#4" "Bb3" "2")
						(list "G#2" "Bb1" "2"))))) ; -> #f

; malformed measure
(section? (list (list
					(list "test" 1) ; meta
					(list (list "G#2" "Bb1" "2"))))) ; -> #f


|#
; "G#3" --> "Ges'''"

;;; convert to a specific number
;;; more esaily

;;; Metadata

(define (clef? clef)
  (let
  	((res (member clef (list "treble" "bass" "alto" "tenor" "percussion"))))
	(and res (list? res) (> (length res) 0))))

#|
(clef? "treble") ; -> #t
(clef? "alto") ; -> #t
(clef? "trebel") ; -> #f
|#


(define (time-signature? time)
	(and
		(list? time)
		(= (length time) 2)
		(number? (string->number (first time)))
		(number? (string->number (second time)))))

#|
(time-signature? (list "4" "4")) ; -> #t
(time-signature? (list "4")) ; -> #f
(time-signature? "4/4") ; -> #f
(time-signature? "4/") ; -> #f
(time-signature? (list "3" "4")) ; -> #t
(time-signature? (list "12" "8")) ; -> #t
|#


(define (key-signature? key)
  (and
    (= 2 (length key))
    (if
		(= (string-length (car key)) 1)
		(letter? (car key))
		(and
			(letter? (char->name (string-ref (car key) 0)))
			(and (accidentals? (substring (car key) 1)))))
    (or (string=? "major" (cadr key)) (string=? "minor" (cadr key))))
  )

#|
(key-signature? (list "C" "major")) ; -> #t
(key-signature? (list "D" "minor")) ; -> #t
(key-signature? (list "Ab" "minor")) ; -> #t
(key-signature? (list "G#" "major")) ; -> #t
(key-signature? (list "major")) ; -> #f
(key-signature? (list "C")) ; -> #f
|#


; these are getters for measures, not metadata
(define (get-metadata measure)
  (car measure))

; formatted as x/y
(define (get-time measure)
  (string-append (first measure) "/" (second measure)))

#|
(get-time (list "3" "4" (list "F" "major") "bass")) ; -> 3/4
(get-time (list "12" "8" (list "F" "major") "bass")) ; -> 12/8
|#

(define (get-key measure)
  (cadr (car measure)))

(define (get-clef measure)
  (caddr (car measure)))


; TODO: clean up the CFL syntax below

#|
A metadata predicate where it returns true if the input is a valid symbol following the CFL:
metadata ::= time-signature key-signature clef 
time-signature ::= integer integer
key-signature ::= letter-accidental tonality
tonality ::= major | minor
letter ::= A | B | C | D | E | F | G
accidental ::= # | ## | b | bb
clef ::= treble | bass | alto | tenor | percussion
|#

(define (metadata? expr)
  (and
    (list? expr)
    (= (length expr) 4)
    (time-signature? (sublist expr 0 2))
    (key-signature? (list-ref expr 2))
    (clef? (list-ref expr 3))))

#|
(metadata? (list "3" "4" (list "F" "major") "bass")) ; -> #t
(metadata? (list "3/4" (list "F" "major") "bass")) ; -> #f
(metadata? (list "5" "4" (list "D" "minor") "bass")) ; -> #t
(metadata? (list "3" "4" (list "A#" "minor") "alto")) ; -> #t
(metadata? (list "2" "4" (list "C#" "minor") "treble")) ; -> #t
(metadata? (list "2" "4" (list "C#" "minor"))) ; -> #f
(metadata? (list "2" "4" "C#" "minor" "treble")) ; -> #f
(metadata? (list "G#2" "A2")) ; -> #f
|#
