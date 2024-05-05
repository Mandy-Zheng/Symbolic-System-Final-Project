
					; Generic add procedures
(load "predicates.scm")
(load "parser.scm")
;; (define (default-convert expr)
;;   (error "Unknown expression type" expression))

;;NOTE: TO USE CONVERT ON A PIECE OF MUSIC, USE CONVERT-PIECE OR CONVERT-SECTION
(define (convert-metadata measure)
  (string-append
   (convert-time (get-time measure))
   (convert-key (get-key measure))
   (convert-clef (get-clef measure))
   "\n"
  )
  )

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

(define (convert-accidental accidental)
  (cond ((string=? accidental sharp) "is")
	((string=? accidental double-sharp) "isis")
	((string=? accidental flat) "es")
	((string=? accidental double-flat) "eses")
	(else "") 
	)
  )

(define (convert-pitch pitch)
  (string-append (string-downcase (get-letter pitch)) (convert-accidental (get-accidentals pitch)) (convert-octave (string->number (get-octave pitch))))
  )

(define (convert-note note)
  (if (= 2 (length note))
      (string-append (convert-pitch (get-first-pitch note)) (get-frequency note))
      (let lp ((chord-info note)
	       (frequency (get-frequency note))
	       (converted "<"))
	(if (= 1 (length chord-info))
	    (string-append converted ">" (car chord-info))
	    (lp (cdr chord-info) frequency (string-append converted (convert-pitch (car chord-info)) " "))
	    
	    )
	)
      )
  )

;;(convert-note (list "A#2" "1")) ; "ais,,1"

;;(convert-note (list "A#2" "G2" "Bb4" "1")) ; "<ais,, g,, bes >1"

(define (convert-measure measure-info)
  (let lp ((measure (get-notes-in-measure measure-info))
	   (converted ""))
    (if (= (length measure) 1)
	(string-append converted (convert-note (car measure)) " | \n")
	(lp (cdr measure) (string-append converted (convert-note (car measure)) " "))
    )
    )
  )

;;(convert-measure (list (list "3/4" (list "E" "minor") "treble") (list "F#1" "2") (list "A2" "1"))) ;"fis,,,2 a,,1 | \n"

(define (convert-time time)
  (string-append "\\time " time " "))
(define (convert-key key)
  (string-append "\\key " (string-downcase (car key)) " \\" (string-downcase (cadr key)) " "))
(define (convert-clef clef)
  (string-append "\\clef " clef " "))

(define (convert-time-if-different ptime time)
  (if (not (string=? ptime time))
      (convert-time time)
      ""
      ))

(define (convert-key-if-different pkey key)
  (if (not (and (string=? (car pkey) (car key)) (string=? (cadr pkey) (cadr key))))
      (convert-key key)
      ""
      ))

(define (convert-clef-if-different pclef clef)
  (if (not (string=? pclef clef))
      (convert-clef clef)
      ""
      ))

(define (convert-metadata-if-different ptime pkey pclef time key clef)
  (let ((time (convert-time-if-different ptime time))
    (key (convert-key-if-different pkey key))
    (clef (convert-clef-if-different pclef clef)))
    (if (= 0 (string-length (string-append time key clef)))
	(string-append "")
	(string-append "\\bar \"||\" " time key clef "\n")
	)

    ))

;;(display (convert-metadata-if-different "4/4" (list "c" "major") "treble" "4/4" (list "c" "minor") "bass")) ; \bar "||" \key c \minor \clef bass 

(define (convert-section . sections)
  (let lp ((sections sections)
	   (measure (car sections))
	   (converted (convert-metadata (car sections)))
	   (past-time (get-time (car sections)))
	   (past-key (get-key (car sections)))
	   (past-clef (get-clef (car sections)))
	   )
    (let ((time (get-time measure))
	  (key (get-key measure))
	  (clef (get-clef measure)))
      (if (= 0 (length (cdr sections)))
	  (string-append converted (convert-metadata-if-different past-time past-key past-clef time key clef) (convert-measure measure))
	  (lp (cdr sections) (cadr sections) (string-append converted (convert-metadata-if-different past-time past-key past-clef time key clef) (convert-measure measure)) time key clef)
      
      )
      )
    )
  )
;;(display (convert-section (list (list "3/4" (list "E" "minor") "treble") (list "F#1" "2") (list "A2" "1"))))

;; (define-generic-procedure-handler convert
;;   (match-args note?)
;;   convert-note)

;; (define-generic-procedure-handler convert
;;   (match-args measure?)
;;   convert-measure)

;; (define-generic-procedure-handler convert
;;   (match-args section?)
;;   convert-section)
(define (convert-voice voice)
  (string-append "\\new Staff { \n \\relative c' {\n" (apply convert-section (cadr voice)) "} \n } \n")
  )

#|
(display (convert-voice (list (list "voice1")
			      (list
			       (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2")) 
			       (list (list "3/4" (list "F" "major") "bass")  (list "B3" "4") (list "D4" "4") (list "F4" "A4" "C4" "4") )))))
|#

(define (convert-piece . piece)
  (call-with-output-file "output.ly"
    (lambda (output-port)
      (display "\\version \"2.22.0\"\n" output-port)
      (display "\\score {\n << \n" output-port)
      (display (apply string-append (map convert-voice piece)) output-port)
      (display ">> \n \\layout {} \n \\midi {} \n}\n" output-port))))
#|
(display (convert-piece (list (list "voice1") (list (list (list "4/4" (list "C" "major") "treble")  (list "C4" "4") (list "D4" "4") (list "E4" "4") (list "F4" "4"))
			      (list (list "4/4" (list "C" "major") "treble")  (list "G4" "4") (list "A4" "4") (list "C4" "E4" "G4" "2"))))
			(list (list "voice2") (list (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2") )
			      (list (list "4/4" (list "C" "major") "bass")  (list "B4" "4") (list "D4" "4") (list "F4" "A4" "C4" "2"))))))

(get-clef (list (list "4/4" (list "c" "major")  "treble") (list))) ; "treble"
|#

(load-option 'synchronous-subprocess)
(define (open-pdf file-path)
  (run-shell-command (string-append "lilypond " (string-append file-path ".ly")))
  (run-shell-command (string-append "emacs " (string-append file-path ".pdf")))) ;;see if it works on macs

(define (play-music file-path)
  (run-shell-command (string-append "lilypond " (string-append file-path ".ly")))
  (run-shell-command (string-append "open " (string-append file-path ".midi")))) ;;see if it works on macs

;;output 
(open-pdf "output")

  
#|

  
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
|#

