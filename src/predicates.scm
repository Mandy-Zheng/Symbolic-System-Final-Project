(define sharp "#")
(define flat "b")
(define double-sharp "##")
(define double-flat "bb")

; An accidental predicate where it returns true if the symbol is '# or 'b.
(define (single-accidentals? expr)
  (or (string=? expr sharp)
      (string=? expr flat))
  )

#|
(single-accidentals? "#") ; -> #t
(single-accidentals? "b") ; -> #t
(single-accidentals? "##") ; -> #f
(single-accidentals? "###") ; -> #f
|#

; A double accidental predicate where it returns true if the symbol is '## or 'bb.
(define (double-accidentals? expr)
  (or (string=? expr double-sharp)
      (string=? expr double-flat))
  )
#|

(double-accidentals? "b") ; -> #f
(double-accidentals? "bb") ; -> #t
(double-accidentals? "##") ; -> #t
(double-accidentals? "bbb") ; -> #f
|#

(define (accidentals? expr)
  (or (double-accidentals? expr) (single-accidentals? expr)))

; A letter predicate where it returns true if the symbol is a letter from A-G.
(define (letter? expr)
  (member expr (list "A" "B" "C" "D" "E" "F" "G"))
  )

(define (rest? expr)
  (string=? expr "R")
  )

(define (dot? expr)
  (string=? expr ".")
  )

#|
(letter? "A") ; -> ("A" "B" "C" "D" "E" "F" "G")
(letter? "a") ; -> #f
(letter? "C") ; -> ("C" "D" "E" "F" "G")
(letter? "Ab") ; -> #f
(letter? "F") ; -> ("F" "G")
(letter? "G") ; -> ("G")
|#

(define (get-letter pitch)
  (substring pitch 0 1)
  )

(define (get-accidentals pitch)
  (cond ((and (> (string-length pitch) 2) (double-accidentals? (substring pitch 1 3))) (substring pitch 1 3))
	((single-accidentals? (substring pitch 1 2)) (substring pitch 1 2))
	(else ""))
  )

(define (get-octave pitch)
  (let ((start-idx (+ (string-length (get-letter pitch)) (string-length (get-accidentals pitch)))))
    (substring pitch start-idx (string-length pitch))
    )
  )

#|
(get-letter "A#3") ; -> A
(get-accidentals "A#3") ; -> #
(get-octave "A#3") ; -> 3

(get-letter "G2") ; -> G
(get-accidentals "G2") ; -> ''
(get-octave "G2") ; -> 2

(get-letter "Fbb10") ; -> F
(get-accidentals "Fbb10") ; -> bb
(get-octave "Fbb10") ; -> 10
|#

(define (get-first-pitch note)
  (car note))

(define (get-frequency note)
  (list-ref note (- (length note) 1)))


; A pitch predicate where it returns true if the input is a valid symbol following the CFL:

; pitch ::= letter accidental? octave 
; letter ::= A | B | C | D | E | F | G
; accidental ::= # | ## | b | bb
; octave ::= [0-9]+

(define (pitch? str)
  (let ((len (string-length str)))
    (cond ((< len 2) (rest? str))
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
		   (if (and (letter? letter) (single-accidentals? accidental-single) (string->number possible-octave-2))
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
(pitch? "R") ;#t
|#


(define (frequency? expr)
  (let ((base-duration (string->number (substring expr 0 1)))
	(len (string-length expr)))
    (cond ((= len 1) (and base-duration (>= base-duration 0)))
	  ((= len 2) (and base-duration (>= base-duration 0) (dot? (substring expr 1))))
	  (else #f))
    )
  )

#|
(frequency? "1") ;#t
(frequency? "4") ;#t
(frequency? "1/3") ;#t
(frequency? "a1") ;#f
(frequency? "-4") ;#f
(frequency? "0") ;#t
(frequency? "2.") ;#t
(frequency? "1..") ;#f
(frequency? ".8") ;#f
|#


#|
A note predicate where it is represented by a pitch and then a duration for teh note.
Return true if the symbol follows this pattern, else false.
|#

(define (note? expr)

  (and (list? expr) (>= (length expr) 2)  ;; check list and at least one note and duration
       (if (and (rest? (car expr)) (frequency? (cadr expr))) ;handle rest notes
	   #t
	   (let lp ((expr expr))
	     (if (= 1 (length expr))
		 (frequency? (car expr)) ;; last item is note duration
		 (and (not (rest? (car expr))) (pitch? (car expr)) (lp (cdr expr))) ;; check every item before frequency is a valid pitch
		 )
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
(note? (list "R" "2")) ;#t
(note? (list "R" "A#2" "2")) ;#f
|#


; A bar predicate.
(define (bar? expr) (equal? expr '||))

#|
(bar? '||) ; -> #t
(bar? "||") ; -> #f
(bar? 'a) ; -> #f
|#

; Returns a list of notes from the measure.
(define (get-notes-in-measure measure)
  (cdr measure))

#|
(pp (get-notes-in-measure (list (list "test" 1)
				(list (list "A#4" "Bb3" "2")
				      (list "G#2" "Bb1" "2")))))
; -> (("A#4" "Bb3" "2") ("G#2" "Bb1" "2"))
|#

; A measure predicate, at least 1 note as arguments.
; Has 1 meta and a list of notes.
; Return true if at least 1 notes and meta, else false.
(define (measure? expr)
  (define (check-elements elts)
    (if (null? elts) ;; empty
	#t
	(and (note? (car elts)) (check-elements (cdr elts)))))
  (if (metadata? (car expr))
      (and (<= 1 (length (cdr expr))) ;; at least two notes
	   (check-elements (cdr expr)))
      #f))

;; meta with 1 note only

#|
(measure? (list
	   (list "3/4" (list "F" "major") "bass") ; meta
	   (list "A#4" "Bb3" "2")
		 (list "G#2" "Bb1" "2"))) ; -> #t

;; meta with 1 note only
(measure? (list
	   	(list "3/4" (list "F" "major") "bass") ; meta
		(list "A#4" "Bb3" "2"))) ; -> #t

(measure? (list
	   (list "3/4" (list "F" "major") "bass") ; meta
	   (list "A#4"  "2."))) ; -> #t

;; no meta with 1 note only		
(measure? (list
	   (list "A#4" "Bb3" "2"))) ; -> #f

(measure? (list
	   (list "A#4" "Bb3" "2")
		(list "A#4" "Bb3" "2"))) ; -> #f
|#

; A section predicate, at least one measure as arguments.
; Return true if at least one measure, else false.
(define (section? expr)
	(define (check-elements elts)
    	(if (null? elts) ;; empty
		#t
		(and (measure? (car elts)) (check-elements (cdr elts)))))
	(and (<= 1 (length expr)) ;; at least two measures
	   (check-elements expr)))

#|
(section? (list (list
					(list "4/4" (list "F" "major") "bass") ; meta
					(list "G3" "Bb3" "2")
					(list "C4" "E4" "2")))) ; -> #t

(section? (list (list
					(list "3/4" (list "F" "major") "bass") ; meta
					(list "A#4" "Bb3" "2")
					(list "G#2" "Bb1" "2"))
				(list
					(list "3/4" (list "F" "major") "bass") ; meta
					(list "A#4" "Bb3" "2")
					(list "G#2" "Bb1" "2")))) ; -> #t

; malformed measure
(section? (list (list
					(list "A#4" "Bb3" "2")
					(list "G#2" "Bb1" "2")))) ; -> #f

; malformed measure
(section? (list (list
					(list "3/4" (list "F" "major") "bass") ; meta
					(list "G#2" "Bb1" "2")))) ; -> #f
|#

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
	 (string? time)
	 (>= (string-length time) 3)
	 (let lp ((idx 0)
		  (time-length (string-length time)))
	 
	   (if (< idx time-length)
	       (if (string=? (substring time idx (+ idx 1)) "/")
		   (and (string->number (substring time 0 idx)) (string->number (substring time (+ idx 1) time-length)))
		   (lp (+ idx 1) time-length)
		   )
	       #f)
	   )))

(define (time-signature? time)
	(let ((ix (string-search-forward "/" time)))
		(and
			ix
			(number? (string->number (substring time 0 ix)))
			(number? (string->number (substring time (+ ix 1)))))))

#|
(time-signature? "4/4") ; -> #t
(time-signature? "3/4") ; -> #t
(time-signature? "12/8") ; -> #t
(time-signature? "4/") ; -> #f
|#

(define (key-signature? key)
  (and
   (= 2 (length key))
   (or (string=? "major" (cadr key)) (string=? "minor" (cadr key)))
   (letter? (substring (car key) 0 1))
   (if (> (string-length (car key)) 1)     
       (accidentals? (substring (car key) 1))
       #t)
   )
  )

#|
(key-signature? (list "C" "major")) ; -> #t
(key-signature? (list "D" "minor")) ; -> #t
(key-signature? (list "Ab" "minor")) ; -> #t
(key-signature? (list "G#" "major")) ; -> #t
(key-signature? (list "Gbb" "major")) ; -> #t
(key-signature? (list "major")) ; -> #f
(key-signature? (list "C")) ; -> #f
|#


; these are getters for measures, not metadata

(define (get-metadata measure)
	(if (metadata? (car measure))
		(car measure)
		(car (get-last-measure)))) ; grabs the metadata from the last measure of the piece

(define (get-time measure) (caar measure))

(define (get-key measure)
  (cadr (car measure)))

(define (get-clef measure)
  (caddr (car measure)))



; A metadata predicate where it returns true if the input is a valid symbol following the CFL:

(define (metadata? expr)
  (and
    (list? expr)
    (= (length expr) 3)
    (time-signature? (list-ref expr 0))
    (key-signature? (list-ref expr 1))
    (clef? (list-ref expr 2))))

#|
(metadata? (list "3/4" (list "F" "major") "bass")) ; -> #t
(metadata? (list "12/8" (list "F" "major") "bass")) ; -> #t
(metadata? (list "5/4" (list "D" "minor") "bass")) ; -> #t
(metadata? (list "3/4" (list "A#" "minor") "alto")) ; -> #t
(metadata? (list "2/4" (list "C#" "minor") "treble")) ; -> #t
(metadata? (list "2/4" (list "C#" "minor"))) ; -> #f
(metadata? (list "2/4" "C#" "minor" "treble")) ; -> #f
(metadata? (list "G#2" "A2")) ; -> #f
|#
