(load "predicates.scm")

; Returns whether the expression contains at least one incidence of "||".
(define (contains-bar expr)
  (if (= (length expr) 0)
    #f
    (or
      (and (string? (first expr)) (string=? (first expr) "||"))
      (contains-bar (cdr expr)))))

#|
(contains-bar '("||")) ; #t
(contains-bar '()) ; #f
(contains-bar '(("test" 1) ("G#2" "2") ("A2" "1") "||" ("G#2" "2") ("A2" "1"))) ; #t
(contains-bar '(("test" 1) ("G#2" "2") ("A2" "1") "||" ("G#2" "2") ("A2" "1") "||")) ; #t
(contains-bar '("G#2" "2")) ; #f
(contains-bar '(("test" 1) ("G#2" "2") ("A2" "1"))) ; #f
|#


; Since Scheme symbols are automatically converted to lowercase, we need to fix the case for pitches
; by making them uppercase.
(define (fix-case string-unit)
    (if
        (or (pitch? (string-upcase string-unit)) (pitch? (string-append (string-upcase string-unit) "4"))) ; TODO: hacky fix
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
    ((bar? expr) "||")
    ((symbol? expr)
        (fix-case (symbol->string expr)))
    ((number? expr)
        (number->string expr))
    ((pair? expr)
        (map stringify-terms expr))
    (else expr)))

#|
(stringify-terms '(||)) ; -> ("||")
(stringify-terms '(a#2 b3 1)) ; -> ("A#2" "B3" "1")
(stringify-terms '(a#2 (1 3 c1) d4)) ; -> ("A#2" ("1" "3" "C1") "D4")
(stringify-terms '(a#2 (1 3 c1) || d4)) ; -> ("A#2" ("1" "3" "C1") "||" "D4")
(stringify-terms '(a#2 (b3 c1 (d4 2 3 (f3 4))))) ; -> ("A#2" ("B3" "C1" ("D4" "2" "3" ("F3" "4"))))
|#


; Separate a list into sublists by "||"
; Elements can themselves be lists or strings
(define (separate-by-measure string-expr)
  (define (helper lst acc current)
    (cond ((null? lst)
            (if (not (null? current)) ; "||" at the end of the section
                (reverse (cons (reverse current) acc))
                (reverse acc)))
          ((and
                (string? (car lst))
                (equal? (car lst) "||"))
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

; TODO: fix these test cases with actual metadata
#|
(separate-by-measure '(("test" "1") ("G#2" "2") ("A2" "1") "||" ("G#2" "2") ("A2" "1"))) ; -> '((("test" "1") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")))
(separate-by-measure '(("test" "1") ("G#2" "2") ("A2" "1") "||" ("G#2" "2") ("A2" "1") "||")) ; -> '((("test" "1") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")))
(separate-by-measure '(("test" "1") ("G#2" "2") ("A2" "1") "||" ("G#2" "2") ("A2" "1") "||" ("G#2" "2") ("A2" "1"))) ; -> '((("test" "1") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")))
(separate-by-measure '(("test" "1") ("G#2" "2") ("A2" "1") "||" ("test" "2") ("G#2" "2") ("A2" "1") "||" ("test" "3") ("G#2" "2") ("A2" "1"))) ; -> '((("test" "1") ("G#2" "2") ("A2" "1")) (("test" "2") ("G#2" "2") ("A2" "1")) (("test" "3") ("G#2" "2") ("A2" "1")))
(separate-by-measure '(("test" "1") ("G#2" "2") ("A2" "1") "||" ("G#2" "2") ("A2" "1") "||" ("test" "3") ("G#2" "2") ("A2" "1"))) ; -> '((("test" "1") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("test" "3") ("G#2" "2") ("A2" "1")))
|#

; Separate by measures with metadata -- this collects
; all measures will the same metadata in one list.

; TODO (this edge case below)
; If there is no metadata in the entire expression, then
; return a single list with the metadata from the last measure
; of the current piece prepended to it. (TODO: this is fake metadata for now)
(define (separate-by-metadata string-expr)
  (define (helper lst acc current)
    ; lst is a list of measures, each with or without metadata (invariant)
    ; acc is a list of lists of measures, each with or without metadata (invariant)
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
            '((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t
(equal? (separate-by-metadata
            '((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))))
        '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))))) ; -> #t
(equal? (separate-by-metadata
            '((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))))
        '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))))) ; -> #t
(equal? (separate-by-metadata
            '((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t
(equal? (separate-by-metadata
            '((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t     
(equal? (separate-by-metadata
            '((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("3" "4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t  
(equal? (separate-by-metadata
            '((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("3" "4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))) ((("3" "4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t
(equal? (separate-by-metadata
            '((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("3" "4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))
        '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))) ((("3" "4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))) ; -> #t
|#


; Assign each measure with its proper metadata (tested below)
; string-expr is a list of lists of measures, where each list's measure all have the same metadata
; Case 1 - M1 notes | notes | ...
; Case 2 – M1 notes | notes | M2 notes | notes
; Case 3 – notes | notes | ... [TODO] (look at metadata from last measure)
(define (propagate-metadata string-expr)
    (map (lambda (measure-list)
        (let ((metadata (get-metadata (first measure-list))))
            (cons (first measure-list)
                (map (lambda (measure) (cons metadata measure)) (cdr measure-list)))))
    string-expr)) ; assume first measure in each list has metadata

#|
(equal?
    (propagate-metadata '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1"))) ((("3" "4" ("C" "major") "bass") ("G#2" "2") ("A2" "1")))))
    '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("C" "major") "bass") ("G#2" "2") ("A2" "1")))))
; -> #t
(equal?
    (propagate-metadata '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")))))
    '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")))))
; -> #t
|#

; metadata-splits is a list of lists of measures
(define (flatten-section lst)
    (map
        (lambda (sublist)
            (list (first sublist) (cdr sublist)))
    (apply append lst))) ; keep in a separate procedure to properly use apply

#|
(section? (flatten-section '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("C" "major") "bass") ("G#2" "2") ("A2" "1"))))))
; -> #t
(section? (flatten-section '(((("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ((("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1")) (("3" "4" ("A" "major") "bass") ("G#2" "2") ("A2" "1"))))))
; -> #t
|#

; Applies a series of transformations to properly process a section expression.
(define (process-section string-expr)
    (flatten-section
        (propagate-metadata
            (separate-by-metadata
                (separate-by-measure string-expr)))))

#|
(section? (process-section (stringify-terms '((3 4 (F major) bass) (G#2 2) (A2 1) || (G#2 2) (A2 1))))) ; more test cases below
|#

; Whether a given string expression representing a measure has metadata.
(define (has-metadata? string-expr) (metadata? (car string-expr)))

#|
(has-metadata? '(("3" "4" ("F" "major") "bass") ("G#2" "2") ("A2" "1"))) ; -> #t
(has-metadata? '(("G#2" "2") ("A2" "1"))) ; -> #f
|#

; Properly handles whether there is metadata or not
(define (process-measure string-expr)
    (let
        ((metadata (get-metadata string-expr))
        (notes
            (if (has-metadata? string-expr)
            (cdr string-expr)
            string-expr)))
    (list metadata notes)))

; Combines "3" "4" in the metadata to "3/4"
; Assumes well-formed input
(define (combine-time string-expr)
    (map (lambda (elem) (if
        (and (list? elem) (> (length elem) 2) (number? (string->number (first elem))) (number? (string->number (second elem)))) ; has a time signature
        (append (list (string-append (first elem) "/" (second elem))) (list-tail elem 2))
        elem))
    string-expr))

#|
(process-measure (combine-time (stringify-terms '((3 4 (F major) bass) (G#2 2) (A2 1))))) ; -> (("3/4" ("F" "major") "bass") (("G#2" "2") ("A2" "1")))
|#

; Parses the expression into our music data types
; The main entry point for user input
(define (parse expr)
    (let ((string-expr (combine-time (stringify-terms expr))))
        (cond
            ((and
                (string? (first string-expr))
                (note? string-expr))
                string-expr) ; note (no further processing)
            ((contains-bar string-expr)
                (process-section string-expr)) ; section (contains at least one "||")
            (else (process-measure string-expr))))) ; measure

#|
; note no quotes!
(measure? (parse '((3 4 (F major) bass) (G#2 2) (A2 1))))  ; -> #t
(measure? (parse '((3 4 (F major) bass) (G#2 2) (A2 1) (B2 1)))) ; -> #t

(section? (parse '((3 4 (F major) bass) (G#2 2) (A2 1) || (G#2 2) (A2 1)))) ; -> #t
(section? (parse '((3 4 (F major) bass) (G#2 2) (A2 1) || (C#2 2) (D3 1)))) ; -> #t
(section? (parse '((3 4 (F major) bass) (G#2 2) (A2 1) || (G#2 2) (A2 1) ||))) ; -> #t

(section? (parse '((3 4 (F major) bass) (G#2 2) (A2 1) || (G#2 2) (A2 1) || (G#2 2) (A2 1)))) ; -> #t
(section? (parse '((3 4 (F major) bass) (G#2 2) (A2 1) || (G#2 2) (A2 1) || (5 4 (G major) bass) (G#2 2) (A2 2)))) ; -> #t
(section? (parse '((3 4 (F major) bass) (G#2 2) (A2 1) || (G#2 2) (A2 1) || (5 4 (G major) bass) (G#2 2) (A2 2) || (3 4 (A major) bass) (G#2 2) (A2 1) || (G#2 2) (A2 1)))) ; -> #t
|#
