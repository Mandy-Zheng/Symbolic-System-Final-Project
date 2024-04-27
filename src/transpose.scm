(load "~/Symbolic-System-Final-Project/src/parser.scm")
(define chromatic-scale
  (list
   (list "C" "B#" "Dbb")
   (list "C#" "B##" "Db")
   (list "D" "C##" "Ebb")
   (list "D#" "Eb" "Fbb")
   (list "E" "D##" "Fb")
   (list "F" "E#" "Gbb")
   (list "F#" "E##" "Gb")
   (list "G" "F##" "Abb")
   (list "G#" "Ab")
   (list "A" "G##" "Bbb")
   (list "A#" "Bb" "Cbb")
   (list "B" "A##" "Cb") ))

;;; TODO- TRANSPOSE STEP LIMIT??
(define (transpose-pitch steps pitch)
  (let lp ((chromatics chromatic-scale)
	   (letter (string-append (get-letter pitch) (get-accidentals pitch)))
	   (new-octave  (number->string (+ (string->number (get-octave pitch)) (quotient steps 12))))
	   (idx 0))
    
    (if (member letter (list-ref chromatics idx))
	(string-append (car (list-ref chromatics (modulo (+ idx steps) 12))) new-octave)
	(lp chromatics letter new-octave (+ idx 1))
	)
    )
  )

(transpose-pitch 12 "E3")

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

(define (transpose-section steps . sections)
  (map (lambda (measure) (transpose-measure steps measure)) sections)
  )

(transpose-section 1 (list (list "3/4" (list "E" "minor") "treble") (list "F#1" "2") (list "A2" "1")))