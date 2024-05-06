;;(load "~/Symbolic-System-Final-Project/src/parser.scm")
;(load "~/Symbolic-System-Final-Project/src/predicates.scm")
(load "predicates.scm")
(define chromatic-scale
  (list
    (list "A" "G##" "Bbb")
   (list "A#" "Bb" "Cbb")
   (list "B" "Cb" "A##")
   (list "C" "B#" "Dbb")
   (list "C#" "Db" "B##")
   (list "D" "C##" "Ebb")
   (list "D#" "Eb" "Fbb")
   (list "E" "Fb" "D##")
   (list "F" "E#" "Gbb")
   (list "F#" "Gb" "E##")
   (list "G" "F##" "Abb")
   (list "G#" "Ab")
   ))

(define (get-chromatic-index letter)
  (let lp ((idx 0))
      (if (member letter (list-ref chromatic-scale idx))
	  idx
	  (lp (+ idx 1))
	  )
      )
  )

(get-chromatic-index "A") ; 0
(get-chromatic-index "D##") ; 7
(get-chromatic-index "Gb") ; 9

(define (transpose-pitch steps pitch)
  (let (
	(chromatics chromatic-scale)
	(idx (get-chromatic-index (string-append (get-letter pitch) (get-accidentals pitch))))
	)
    (let ((new-letter-idx (modulo (+ idx steps) 12))
	  (new-octave (+ (string->number (get-octave pitch)) (quotient (+ idx steps) 12)))
	  )
      (if (or (< new-octave 0) (> new-octave 8))
	  (error "Transposing out of bounds"))
      (if (and (< steps 0) (> (string-length (car (list-ref chromatics new-letter-idx))) 1)) ;; if its an accidental, and we are going down, we use the second option with flats
	  (string-append (cadr (list-ref chromatics new-letter-idx)) (number->string new-octave))
	  (string-append (car (list-ref chromatics new-letter-idx)) (number->string new-octave)))
	
	)
    )
  )



(transpose-pitch 12 "E3") ;;"E4"
(transpose-pitch 12 "E#4") ;"F5"
(transpose-pitch 3 "A##4") ;"D4"
(transpose-pitch -3 "A##4") ; "Ab4"
(transpose-pitch 2 "F##4") ; "A5"

(define (transpose-note steps note)
  (map (lambda (pitch)
	 (if (frequency? pitch)
	     pitch
	     (transpose-pitch steps pitch)
	     )
	 ) note)
  )

(define (transpose-measure steps measure)
  (append (list (get-metadata measure)) (map (lambda (note) (transpose-note steps note)) (get-notes-in-measure measure)))
  )

(define (transpose-section steps sections)
  (map (lambda (measure) (transpose-measure steps measure)) sections)
  )

(transpose-section 3 (list (list (list "3/4" (list "E" "minor") "treble") (list "F#1" "2") (list "A2" "1")) (list (list "3/4" (list "E" "minor") "treble") (list "F#1" "2") (list "A2" "1")))) ;;((("3/4" ("E" "minor") "treble") ("A2" "2") ("C2" "1")))
