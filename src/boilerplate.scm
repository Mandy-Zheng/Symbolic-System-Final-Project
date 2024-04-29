; Returns whether the expression contains at least one incidence of "|".
(define (contains-bar expr)
  (if (= (length expr) 0)
    #f
    (or
      (and (string? (first expr)) (string=? (first expr) "|"))
      (contains-bar (cdr expr)))))

#|
(contains-bar '("|")) ; #t
(contains-bar '()) ; #f
(contains-bar '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1"))) ; #t
(contains-bar '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1") "|")) ; #t
(contains-bar '("G#2" "2")) ; #f
(contains-bar '(("test" 1) ("G#2" "2") ("A2" "1"))) ; #f
|#


; Separate a list into sublists by "|"
; Elements can themselves be lists or strings
(define (separate-by-measure string-expr)
  (define (helper lst acc current)
    (cond ((null? lst)
            (if (not (null? current)) ; "|" at the end of the section
                (reverse (cons (reverse current) acc))
                (reverse acc)))
          ((and
                (string? (car lst))
                (equal? (car lst) "|"))
            (helper
                (cdr lst)
                (cons (reverse current) acc)
                '()))
          (else
            (helper
                (cdr lst)
                acc
                (cons (car lst)
                current)))))
  (helper string-expr '() '()))

#|
(separate-by-measure '(("test" "1") ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1"))) ; -> '((("test" "1") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")))
(separate-by-measure '(("test" "1") ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1") "|")) ; -> '((("test" "1") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")))
(separate-by-measure '(("test" "1") ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1"))) ; -> '((("test" "1") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")))
(separate-by-measure '(("test" "1") ("G#2" "2") ("A2" "1") "|" ("test" "2") ("G#2" "2") ("A2" "1") "|" ("test" "3") ("G#2" "2") ("A2" "1"))) ; -> '((("test" "1") ("G#2" "2") ("A2" "1")) (("test" "2") ("G#2" "2") ("A2" "1")) (("test" "3") ("G#2" "2") ("A2" "1")))
(separate-by-measure '(("test" "1") ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1") "|" ("test" "3") ("G#2" "2") ("A2" "1"))) ; -> '((("test" "1") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("test" "3") ("G#2" "2") ("A2" "1")))
|#

; Since Scheme symbols are automatically converted to lowercase, we need to fix the case for pitches
; by making them uppercase.
(define (fix-case string-unit)
    (if
        (pitch? (string-upcase string-unit))
        (string-upcase string-unit)
        string-unit))

#|
(fix-case "a#2") ; -> "A#2"
(fix-case "test") ; -> "test"
(fix-case "G3") ; -> "G3"
|#

; Converts all of the symbols and numbers in the expression to strings.
(define (stringify-terms expr)
  (cond
    ((symbol? expr)
        (fix-case (symbol->string expr)))
    ((number? expr)
        (number->string expr))
    ((pair? expr)
        (map stringify-terms expr))
    (else expr)))

#|
(stringify-terms '(a#2 b3 1)) ; -> ("A#2" "B3" "1")
(stringify-terms '(a#2 (1/3 c1) d4)) ; -> ("A#2" ("1/3" "C1") "D4")
(stringify-terms '(a#2 (b3 c1 (d4 2/3 (f3 4))))) ; -> ("A#2" ("B3" "C1" ("D4" "2/3" ("F3" "4"))))
|#

; TODO
; Assign each measure with its proper metadata.

; Case 1 - M1 notes | notes | ...
; Case 2 – M1 notes | notes | M2 notes | notes
; Case 3 – notes | notes | ...
; Look at metadata from last measure

(define (propagate-metadata string-expr))

; Parses the expression into our music data types
; The main entry point for user input
(define (parse expr)
    (let ((string-expr (stringify-terms expr)))
        (cond
            ((and
                (string? (first string-expr))
                (note? string-expr))
                string-expr) ; note (no further processing)
            ((contains-bar string-expr)
                (propagate-metadata (separate-by-measure string-expr))) ; section (contains at least one "|")
            (else (list (car string-expr) (cdr string-expr)))))) ; measure (split up metadata)


; TODO: catch error if malformed? so it doesn't try to apply?
; TODO: move parse to user-interface when done
; TODO (fix section? with meta) [or maybe not? internal rep of section is just nested lists -- easier to deal with!]

; should work for: measure, section, add, insert, delete

; TODO: | (no quotes) is not a valid Scheme symbol name
; we use "|" for now but we could change the character?

#|
; note no quotes!
(measure? (parse '((test 1) (G#2 2) (A2 1))))
(measure? (parse '((test 1) (G#2 2) (A2 1) (B2 1))))

(section? (parse '((test 1) (G#2 2) (A2 1) "|" (G#2 2) (A2 1))))

(section? (parse '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1")))) ; #t (TODO)
(section? (parse '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1") "|"))) ; #t (TODO)
|#
