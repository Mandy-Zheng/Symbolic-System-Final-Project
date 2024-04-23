; TODO
; Generic edit procedures

(define (default-edit expr)
  (error "Unknown expression type" expression))

(define edit
  (simple-generic-procedure 'edit 1 default-edit))

(define (edit-note expr) (display 'TODO))
(define (edit-measure expr) (display 'TODO))
(define (edit-section expr) (display 'TODO))

(define-generic-procedure-handler edit
  (match-args note?)
  edit-note)

(define-generic-procedure-handler edit
  (match-args measure?)
  edit-measure)

(define-generic-procedure-handler edit
  (match-args section?)
  edit-section)