(define (clef? clef)
  (not (not (member clef (list "treble" "bass" "alto" "tenor" "percussion")))))

#|
(clef? "treble") ; -> #t
(clef? "alto") ; -> #t
(clef? "trebel") ; -> #f
|#

; TODO: fix for 12/8 (length-4 string) [see below ]
(define (time-signature? time)
  (and
    (= 3 (string-length time))
    (char-numeric? (string-ref time 0))
    (string=? (char->name (string-ref time 1)) "/")
    (char-numeric? (string-ref time 2))))

#|
(time-signature? "4/4") ; -> #t
(time-signature? "4/") ; -> #f
(time-signature? "3/4") ; -> #t
(time-signature? "12/8") ; -> #t [TODO]
|#

; TODO: fix for A# minor (see below)
(define (key-signature? key)
  (and
    (= 2 (length key))
    (letter? (car key))
    (or (string=? "major" (cadr key)) (string=? "minor" (cadr key))))
  )

#|
(key-signature? (list "C" "major")) ; -> #t
(key-signature? (list "D" "minor")) ; -> #t
(key-signature? (list "Ab" "minor")) ; -> #t [TODO]
(key-signature? (list "G#" "major")) ; -> #t [TODO]
(key-signature? (list "major")) ; -> #f
(key-signature? (list "C")) ; -> #f

; these are getters for measures, not metadata
(define (get-clef measure)
  (caar measure))

(define (get-time measure)
  (cadr (car measure)))

(define (get-key measure)
  (caddr (car measure)))


; TODO: clean up the CFL syntax below

#|
A metadata predicate where it returns true if the input is a valid symbol following the CFL:
metadata ::= time-signature key-signature clef 
time-signature ::= integer "/" integer
key-signature ::= letter-accidental tonality
tonality ::= major | minor
letter ::= A | B | C | D | E | F | G
accidental ::= # | ## | b | bb
clef ::= treble | bass | alto | tenor | percussion


(define (metadata? expr)
  (and
    (list? expr)
    (= (length expr) 3)
    (time-signature? (list-ref expr 0))
    (key-signature? (list-ref expr 1))
    (clef? (list-ref expr 2))))

#|
(metadata? (list "3/4" (list "F" "major") "bass")) ; -> #t
(metadata? (list "5/4" (list "D" "minor") "bass")) ; -> #t
(metadata? (list "3/4" (list "A#" "minor") "alto")) ; -> #t [TODO]
(metadata? (list "2/4" (list "C#" "minor") "treble")) ; -> #t
(metadata? (list "2/4" (list "C#" "minor"))) ; -> #f
(metadata? (list "2/4" "C#" "minor" "treble")) ; -> #f
(metadata? (list "G#2" "A2)) ; -> #f
|#
