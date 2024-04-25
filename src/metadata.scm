(define (clef? clef)
  (member clef (list "treble" "bass" "alto" "tenor" "percussion")))

(define (time-signature? time)
  (and (= 3 (string-length time)) (string->number (string-ref time 0)) (string->number (string-ref time 2) (string=? (string-ref time 1) "/"))))

(define (key-signature? key)
  (and (= 2 (length key)) (letter? (car key)) (or (string=? "major" (cadr key)) (string=? "minor" (cadr key))))
  )

(define (get-clef measure)
  (caar measure))

(define (get-time measure)
  (cadr (car measure)))

(define (get-key measure)
  (caddr (car measure)))


(define (get-key measure)
  (caddr (car measure)))

(string->number "a")


