






;; Returns whether the expression contains at least one incidence of "|".
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


; Add by parsing
; TODO: combine with add! above
; TODO: catch error if malformed? so it doesn't try to apply?
(define (add-parse expr)
  (cond
    ((and (string? (first expr)) (note? expr)) expr) ; just return the note list (no flattening)
    ((contains-bar expr) expr) ; section (contains at least one "|")
    (else (list (car expr) (cdr expr))))) ; measure


; Separate a list into sublists by "|"
; Elements can themselves be lists
(define (separate-by-measure lst)
  (define (helper lst acc)
    (cond ((null? lst) (reverse acc))
          ((and (pair? (car lst)) (not (null? (car lst)))) ; check if empty
           (helper (cdr lst) (cons (separate-by-measure (car lst)) acc)))
          ((and (string? (car lst)) (string=? (car lst) "|")) (helper (cdr lst) (cons '() acc)))
          (else (helper (cdr lst) (cons (cons (car lst) (car acc)) (cdr acc))))))
  (helper lst (list '())))


#|
(separate-by-measure '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1"))) ; -> '((("test" 1) ("G#2" "2") ("A2" "1")) (("G#2" "2") ("A2" "1")))
(separate-by-measure '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1") "|"))
(separate-by-measure '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1") "|" ("G#2" "2")))
|#

; TODO: don't need to add notes

; measure, section, add, insert, delete


#|
(note? (add-parse '("G#2" "2"))) ; #t
(note? (add-parse '("G#2" "B2" "2"))) ; #t
(measure? (add-parse '(("test" 1) ("G#2" "2") ("A2" "1") ("B2" "1")))) ; #t (note the simpler syntax with fewer parentheses)

; note the simpler syntax with fewer parentheses and no metadata for second measure
(section? (add-parse '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1")))) ; #t (TODO)
(section? (add-parse '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1") "|"))) ; #t (TODO)
|#

; TODO (fix section? with meta)
; (internal rep of section is just nested lists -- easier to deal with!)
; TODO: get-section-meta
; TODO: get-number-measures-in-section

;;; TESTING UI
;;(section? (add-parse '(("test" 1) ("G#2" "2") ("A2" "1") "|" ("G#2" "2") ("A2" "1"))))

; TODO: can we try to eliminate "" by storing away note names?
(add! (add-parse '(("test" 1) ("G#2" "2") ("A2" "1") ("B2" "1"))))
