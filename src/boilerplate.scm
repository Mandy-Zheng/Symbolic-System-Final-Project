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

; Separate by measures with metadata -- this collects
; all measures will the same metadata in one list.

; TODO (this edge case)
; If there is no metadata in the entire expression, then
; return a single list with the metadata from the last measure
; of the current piece prepended to it. (TODO: this is fake metadata for now)
(define (separate-by-metadata string-expr)
  (define (helper lst acc current)
    ; lst is a list of measures, each with or without metadata (invariant)
    ; acc is a list of list of measures, each with or without metadata (invariant)
    ; current is a list of measures, each with or without metadata (invariant)
    (cond ((null? lst) 
           (if (not (null? current))
               (reverse (cons (reverse current) acc))
               acc))
          ((metadata? (caar lst))
           (helper (cdr lst)
                   (if (not (null? current))
                       (cons (reverse current) acc)
                       acc)
                   (list (car lst))))
          (else
           (helper (cdr lst)
                   acc
                   (cons (car lst) current)))))
  (helper string-expr '() '()))

#|
(equal? (separate-by-metadata
            '((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t
(equal? (separate-by-metadata
            '((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))))
        '(((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))))) ; -> #t
(equal? (separate-by-metadata
            '((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))))
        '(((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))))) ; -> #t
(equal? (separate-by-metadata
            '((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t
(equal? (separate-by-metadata
            '((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("3/4" ("A" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3/4" ("A" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t     
(equal? (separate-by-metadata
            '((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("3/4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("3/4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3/4" ("A" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3/4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t  
(equal? (separate-by-metadata
            '((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("3/4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("3/4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3/4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))) ((("3/4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t
(equal? (separate-by-metadata
            '((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("3/4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3/4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))) ((("3/4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t
|#


; TODO
; Assign each measure with its proper metadata.

; Case 1 - M1 notes | notes | ...
; Case 2 – M1 notes | notes | M2 notes | notes
; Case 3 – notes | notes | ...
; Look at metadata from last measure
(define (propagate-metadata string-expr)
    (if
        (measure? (first string-expr))
        ()
        ()))

; Applies a series of transformations to properly process a section expression (tested with parse below).
(define (process-section string-expr)
    (propagate-metadata
        (separate-by-metadata
            (separate-by-measure string-expr))))

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
                (process-section string-expr)) ; section (contains at least one "|")
            (else (list (car string-expr) (cdr string-expr)))))) ; measure (split up metadata)


; TODO: catch error if malformed? so it doesn't try to apply?
; TODO: move parse to user-interface when done

; should work for: measure, section, add, insert, delete

; TODO: | (no quotes) is not a valid Scheme symbol name
; we use "|" for now but we could change the character?

#|
; note no quotes!
(measure? (parse '((test 1) (G#2 2) (A2 1))))
(measure? (parse '((test 1) (G#2 2) (A2 1) (B2 1))))

; TODO -- two test cases for each case of section with measure propagation
(section? (parse '((test 1) (G#2 2) (A2 1) "|" (G#2 2) (A2 1))))

(section? (parse '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1")))) ; #t (TODO)
(section? (parse '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1") "|"))) ; #t (TODO)
|#
