; Generic add procedures

(load "generic-procedures.scm")

(define (default-add expr)
  (error "Unknown expression type" expression))

(define add
  (simple-generic-procedure 'add 1 default-add))

; TODO: remove add-note
(define (add-note expr) (display 'TODO))
(define (add-measure expr) (
  expr ;TODO? should be in scheme in not already
  ))
(define (add-section expr) (display 'TODO))

(define-generic-procedure-handler add
  (match-args note?)
  add-note)

(define-generic-procedure-handler add
  (match-args measure?)
  add-measure)

(define-generic-procedure-handler add
  (match-args section?)
  add-section)