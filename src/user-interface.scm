
;; Load logic for session environment
(load "environment.scm")
;; generic add
(load "add.scm")
(load "parser.scm")

;; global vars for each session
(define current-piece-name)
(define author-name)
(define session-environment)

;; FOR modify to use to get body
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

(define (display-measures message)
  (if (pair? message)
      (begin
        (fresh-line)
        (display-item (car message))
        (for-each (lambda (item)
                    (display "\n")
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


;;; Helper functions
;; Return the measure at index i
(define (get-measure-at-index i)
  (list-ref (lookup-variable-value current-piece-name session-environment) i))

(define (save-body new-body)
  (set-variable-value! current-piece-name new-body session-environment))

;;; Getter: display stuffs for users
;; Get the names of all current pieces.
(define (get-all-pieces!)
  (let ((all-pieces (environment-variables session-environment)))
    (if (null? all-pieces)
	(display-message (list "You haven't written any piece."))
	(display-message (list "All pieces: " (environment-variables
					       session-environment))))))

(define (get-current-piece!)
  (display-message (list "The current piece is:" current-piece-name)))

(define (get-current-body!)
  (if (null? current-piece-name)
      (display-message (list "Oops, you haven't selected a piece to compose."))
      (begin
	(let ((body (lookup-variable-value current-piece-name session-environment)))
	  
	  (display-message (list "Here's your current piece:"))
	  (display-measures (map (lambda (measure index)
				  (list
				   (string-append "Measure-" (number->string index))
				   measure))
				body (iota (length body))))))))

	

;; Commands to compose
(define (define-new-piece! piece-name)
  (define-variable! piece-name (list ) session-environment)
  (set! current-piece-name piece-name)
  (display-message (list "You're starting a new piece:" piece-name)))


;; Can only add by measure and section.
(define (add! . expr)
  (let ((new-additions expr) ; TODO use add generic
	(body (get-current-piece-body)))
    (if (measure? expr)
	(save-body (append! body (list new-additions))) ; keep measure as 1 list
	(save-body (append! body new-additions))) ; like extend   
    (get-current-body!)))

;; Insert by splitting two portion. Helper for insert!.
(define (insert-at index element lst)
  (if (= index 0)
      (cons element lst) ; add to end
      (cons (car lst)    ; add to front
            (insert-at (- index 1) element (cdr lst))))) 
#|
(define my-list '(1 2 3 4 5))
(display (insert-at 2 100 my-list))
|#

(define (insert! insert-i new-measure)
  (let ((current-body (get-current-piece-body)))
    (save-body (insert-at insert-i new-measure current-body)))
  (get-current-body!))


(define (delete-by-index index lst)
  (cond ((null? lst) '())  ; empty
        ((= index 0) (cdr lst)) ; return the rest of the list
        (else (cons (car lst) (delete-by-index (- index 1) (cdr lst))))))
#|
(define my-list '(1 2 3 4 5))
(display (delete-by-index 1 my-list)) ;(1 3 4 5)
|#

;; Given the index of the measure, delete the measure.
(define (delete! delete-i)
  (let ((current-body (get-current-piece-body)))
    (save-body (delete-by-index delete-i current-body)))
  (get-current-body!))  


;;; TESTING UI

(start-composing 'nhung)
(get-all-pieces!)
(define-new-piece! 'twinkle)

(get-all-pieces!)
(get-current-piece!)
(get-current-body!)

(add! (list
	   (list "test" 1) ; meta
	   (list (list "A#4" "Bb3" "2")
		 (list "G#2" "Bb1" "2"))))

(add! (list
	   (list "test-meta 2" 2) ; meta
	   (list (list "A#4" "Bb3" "2")
		 (list "G#2" "Bb1" "2"))))


(insert! 1 (list
	  (list "test-meta 3" 2) ; meta
	  (list (list "A#4" "Bb3" "2")
		(list "G#2" "Bb1" "2"))))


(delete! 1)
(delete! 0)


