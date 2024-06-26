(load "predicates.scm")
(load "parser.scm")

(define (convert-metadata measure)
  (string-append
   (convert-time (get-time measure))
   (convert-key (get-key measure))
   (convert-clef (get-clef measure))
   "\n"))

(define (convert-octave octave)
  (cond ((= 0 octave)  ",,,")
	((= 1 octave) ",,")
	((= 2 octave) ",")
	((= 3 octave) "")
	((= 4 octave) "'")
	((= 5 octave) "''")
	((= 6 octave)  "'''")
	((= 7 octave)  "''''")
	((= 8 octave)  "'''''")
	(else (error "Invalid octave"))))

(define (convert-accidental accidental)
  (cond ((string=? accidental sharp) "is")
	((string=? accidental double-sharp) "isis")
	((string=? accidental flat) "es")
	((string=? accidental double-flat) "eses")
	(else "")))

(define (convert-pitch pitch)
  (if (rest? pitch)
      (string-append (string-downcase (get-letter pitch)))
      (string-append (string-downcase (get-letter pitch))
		     (convert-accidental (get-accidentals pitch))
		     (convert-octave (string->number (get-octave pitch))))
      )
  )
#|
(convert-pitch "R") ;r
(convert-pitch "C#3") ;cis
|#

(define (convert-note note)
  (if (= 2 (length note))
      (string-append (convert-pitch (get-first-pitch note)) (get-frequency note))
      (let lp ((chord-info note)
	       (frequency (get-frequency note))
	       (converted "<"))
	(if (= 1 (length chord-info))
	    (string-append converted ">" (car chord-info))
	    (lp (cdr chord-info) frequency (string-append converted (convert-pitch (car chord-info)) " "))))))

#|
(convert-note (list "A#2" "1")) ; "ais,,1"
(convert-note (list "A#2" "G2" "Bb4" "1")) ; "<ais,, g,, bes >1"
(convert-note (list "R" "2.")) ;r2.
(convert-note (list "Fb2" "4.")) ;fes,4.
|#

(define (convert-measure measure-info)
  (let lp ((measure (get-notes-in-measure measure-info))
	   (converted ""))
    (if (= (length measure) 1)
	(string-append converted (convert-note (car measure)) " | \n")
	(lp (cdr measure) (string-append converted (convert-note (car measure)) " ")))))

#|
(convert-measure (list (list "3/4" (list "E" "minor") "treble") (list "F#1" "2") (list "A2" "1"))) ; -> "fis,,,2 a,,1 | \n"
|#

(define (convert-time time)
  (string-append "\\time " time " "))

(define (convert-key key)
  (string-append "\\key "
		 (string-downcase (car key))
		 " \\"
		 (string-downcase (cadr key)) " "))

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
	)))

#|
(display (convert-metadata-if-different "4/4" (list "c" "major") "treble" "4/4" (list "c" "minor") "bass")) ; \bar "||" \key c \minor \clef bass 
|#

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
	  (string-append converted
			 (convert-metadata-if-different past-time past-key past-clef time key clef)
			 (convert-measure measure))
	  (lp (cdr sections) (cadr sections)
	      (string-append converted
			     (convert-metadata-if-different past-time past-key past-clef time key clef)
			     (convert-measure measure)) time key clef)))))

#|   
(display (convert-section (list (list "3/4" (list "E" "minor") "treble") (list "F#1" "2") (list "A2" "1"))))
|#

(define (convert-voice voice)
  (string-append "\\new Staff { \n  {\n" (apply convert-section (cadr voice)) "} \n } \n")
  )

#|
(display (convert-voice (list (list "voice1")
			      (list
			       (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2")) 
			       (list (list "3/4" (list "F" "major") "bass")  (list "B3" "4") (list "D4" "4") (list "F4" "A4" "C4" "4") )))))
|#

(define (convert-piece name piece)
  (call-with-output-file "output.ly"
    (lambda (output-port)
      (display "\\version \"2.22.0\"\n" output-port)
      (display (string-append "\\header {title=\"" name "\"}\n") output-port)
      (display "\\score {\n << \n" output-port)
      (display (apply string-append (map convert-voice piece)) output-port)
      (display ">> \n \\layout {indent=0} \n \\midi {} \n}\n" output-port))))

#|
(display (convert-piece "name" (list (list (list "voice1") (list (list (list "4/4" (list "C" "major") "treble") (list "R" "4") (list "C4" "4") (list "R" "2"))
			      (list (list "4/4" (list "C" "major") "treble")  (list "R" "4") (list "C4" "E4" "G4" "2."))))
			(list (list "voice2") (list (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2") )
			      (list (list "4/4" (list "C" "major") "bass")  (list "B4" "4") (list "D4" "4") (list "F4" "A4" "C4" "2")))))))

(get-clef (list (list "4/4" (list "c" "major")  "treble") (list))) ; "treble"
|#

(define (os-open-command)
   (if (eq? microcode-id/operating-system 'unix)
    "open "
    "xdg-open ")
  )

;; output
 
(load-option 'synchronous-subprocess)

(define (open-pdf file-path)
  (run-shell-command (string-append "lilypond " (string-append file-path ".ly")))
  (run-shell-command (string-append (os-open-command) (string-append file-path ".pdf")))) 

(define (play-music file-path)
  (run-shell-command (string-append "lilypond " (string-append file-path ".ly")))
  (run-shell-command (string-append (os-open-command) (string-append file-path ".midi")))) 


#|
(open-pdf "output")

(play-music "output")
|#
