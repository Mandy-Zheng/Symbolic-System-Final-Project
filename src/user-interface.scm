
;; Load logic for session environment
(load "environment.scm")

;; global vars for each session
(define current-piece-name)
(define author-name)
(define session-environment)

;; FOR add and modify to use to get body
(define (get-current-piece-body)
  (if (null? current-piece-name)
     ; (begin
;	(display-message (list "No piece selected or defined at the moment"))
	#f ;; don't do anything
        (lookup-variable-value current-piece-name session-environment)))

(define (start-composing my-name)
  (set! author-name my-name)
  (set! session-environment (make-global-environment))
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

;; Get the names of all current pieces.
(define (get-all-pieces!)
  (let ((all-pieces (environment-variables session-environment)))
    (if (null? all-pieces)
	(display-message (list "You haven't written any piece."))
	(display-message (list "All pieces: " (environment-variables
					       session-environment))))))

(define (define-new-piece! piece-name)
  (define-variable! piece-name (list ) session-environment)
  (set! current-piece-name piece-name)
  (display-message (list "You're starting a new piece:" piece-name)))
  
;; TODO make commands for user 
; will call the generic adds
(define (add! . expr)
  (display (list "TODO" expr)))


(start-composing 'nhung)
(get-all-pieces!)
(define-new-piece! 'twinkle)
; (add! (list "a#4" 2))


