; Generic add procedures
(load "~/Symbolic-System-Final-Project/src/parser.scm")
(define (default-convert expr)
  (error "Unknown expression type" expression))

(define convert
  (simple-generic-procedure 'add 1 default-add))

(define (convert-accidental accidental)
  (cond ((string=? accidental sharp) "is")
	((string=? accidental double-sharp) "isis")
	((string=? accidental flat) "es")
	((string=? accidental double-flat) "eses")
	(else "") 
	)
  )

(define (convert-pitch pitch)
  ;(print pitch)
  (string-append (get-letter pitch) (convert-accidental (get-accidentals pitch)) (convert-octave (string->number (get-octave pitch))))
  )

(define (convert-note note)
  (if (= 2 (length note))
      (string-append (convert-pitch (get-first-pitch note)) (get-frequency note))
      (let lp ((chord-info note)
	       (frequency (get-frequency note))
	       (converted "<"))
        (print note)
	(if (= 1 (length chord-info))
	    (string-append converted ">" (car chord-info))
	    (lp (cdr chord-info) frequency (string-append converted (convert-pitch (car chord-info)) " "))
	    
	    )
	)
      )
  )
(convert-note (list "A#2" "1")) ; "Ais,,1"

(convert-note (list "A#2" "G2" "Bb4" "1")) ; "<Ais,, G,, Bes >1"

(define (convert-octave octave)
  (cond ((= 0 octave)  ",,,,")
	((= 1 octave) ",,,")
	((= 2 octave) ",,")
	((= 3 octave) ",")
	((= 4 octave) "")
	((= 5 octave) "'")
	((= 6 octave)  "''")
	((= 7 octave)  "'''")
	((= 8 octave)  "''''")
	(else (error "Invalid octave")))

  )

(define (convert-measure measure-info)
  (let lp ((measure (get-notes-in-measure measure-info))
	   (converted ""))
    (if (= (length measure) 1)
	(string-append converted (convert-note (car measure)) " |")
	(lp (cdr measure) (string-append converted (convert-note (car measure)) " "))
    )
    )
  )

(convert-measure (list (list) (list (list "A2" "1") (list "G##2" "1") (list "C1" "E1" "G1" "2") ) )) ; "A,,1 Gisis,,1 <C,,, E,,, G,,, >2 |"


(define (convert-section . sections)
  (apply string-append (map (lambda (measure) (string-append (convert-measure measure) " ")) sections))
  )

(convert-section (list (list) (list (list "A2" "1") (list "G##2" "1") (list "C1" "E1" "G1" "2"))) (list (list) (list (list "B#4" "1") (list "Db2" "1") (list "F4" "A3" "C4" "2")))) ;"A,,1 Gisis,,1 <C,,, E,,, G,,, >2 | Bis1 Des,,1 <F A, C >2 | "


(define-generic-procedure-handler convert
  (match-args note?)
  convert-note)

(define-generic-procedure-handler convert
  (match-args measure?)
  convert-measure)

(define-generic-procedure-handler convert
  (match-args section?)
  convert-section)


(define (convert-metadata metadata)
  (format #t
  "global = {
    \time 4/4
    \numericTimeSignature
    \key c \major
  }"
  (time-signature metadata)
  (key metadata)
  )
)


(define (convert-note))
(define (convert-measure))

(define (convert-music voice)
  
  
  
  (

  
  "cf = \relative {
  \clef bass
  \global
  c4 c' b a |
  g a f d |
  e f g g, |
  c1
}"
  )
;;relative pitches to specified one

;; \relative c' {
;;  c4 d e f | g2 g4 | a8 a g4 f2 | e1 | r2 r4 r8 r16 r32 r64 |
;; }
;;times with duration

;;c is middle c
;; c, is lower by one octave
;; c' is higher by one octave


;;\relative c' {
;;  \key g \major % Change to the key of G major
;;  c4 d e f | g2 g4 | a8 a g4 f2 | e1 | r2 r4 r8 r16 r32 r64 |
;;}
