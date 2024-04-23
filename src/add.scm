; Generic add procedures

(define (default-add expr)
  (error "Unknown expression type" expression))

(define add
  (simple-generic-procedure 'add 1 default-add))

(define (add-note expr) (display 'TODO))
(define (add-measure expr) (display 'TODO))
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