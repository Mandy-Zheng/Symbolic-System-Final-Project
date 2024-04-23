(define current-piece)
(define author-name)

(define (start-composing my-name)
  (set! author-name my-name)
  (welcome-description))

(define (display-message message)
  (if (pair? message)
      (begin
        (fresh-line)
        (display-item (car message))
        (for-each (lambda (item)
                    (display " ")
                    (display-item item))
                  (cdr message))))
  'done)

(define (display-item item)
  (display item)
  'done)

(define (welcome-description)
  (display-message (list "Welcome" author-name "!"))
  (display-message (list "Are you ready to compose your music?"))
  (display-message (list "Here are the available commands that you can use to compose your music:"))
  (display-message (list "Use (add ...) to add notes, measures, section, and voice together")))

;; TODO make commands for user 
; will call the generic adds
(define (add! . expr)
  (display (list "TODO" expr)))


(start-composing 'nhung)
(add! (list "a#4" 2))


