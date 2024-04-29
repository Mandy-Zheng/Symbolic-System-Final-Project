
;; Load logic for session environment
(load "environment.scm")
;; generic add
;;(load "add.scm")
(load "parser.scm")
(load "converter.scm")

;; global vars for each session
(define current-piece-name)
(define author-name)
(define session-environment) ;; list of voices, voice is list of measures
(define current-voice-name)
(define current-voice-index) 


;; Display welcome message, and make an empty environment
(define (start-composing my-name)
  (set! author-name my-name)
  (set! session-environment (make-global-environment))
  (welcome-description))


;; Display helper functions
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

;; same as display-message, just have new line in between
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


;;; Helper functions for add, insert, delete
(define (find-index-by-first-element list-of-lists name)
  (let loop ((lists list-of-lists)
             (index 0))
    (cond ((null? lists) #f) ; not found
          ((equal? (caar lists) name) index)
          (else (loop (cdr lists) (+ index 1))))))

;; updates the current-voice-index
(define (update-current-voice-index)
  (if (null? current-voice-name)
      #f
      (begin
	(let ((body (get-current-piece-body)))
	  (set! current-voice-index
		(find-index-by-first-element body current-voice-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions to Read From Environment ;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-all-pieces-names)
 (environment-variables session-environment)) 

;; Get the current piece's body, include all voices
(define (get-current-piece-body)
  (if (null? current-piece-name)
	#f ;; don't do anything
        (lookup-variable-value current-piece-name session-environment)))

;; Get the current voice body with the voice name and list of measures
(define (get-current-voice-body)
  (let ((body (get-current-piece-body)))
    (list-ref body current-voice-index)))

;; Get only the measures in the current voice
(define (get-current-voice-measures)
  (cadr (get-current-voice-body)))

;; Return the measure at index i of the current-voice
(define (get-measure-at-index i)
  (let ((voice-body (get-current-voice-measures)))
    (list-ref voice-body i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions to Modify Environment ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save the full body of the piece given the new body
(define (save-body new-body)
  (set-variable-value! current-piece-name new-body session-environment))

;; Save the new voice body in the current piece
(define (save-voice new-voice-body)
  ;; call save-body with the new full content
  (let ((current-body (get-current-piece-body))) ; full body
    (save-body (map (lambda (voice index)
		      (if (= index current-voice-index)
			  (list current-voice-name new-voice-body) ;replace with new voice
			  voice))
		    current-body (iota (length current-body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper Functions to Modidy Global vars ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reset  current-voice-name and index because piece was changed
(define (reset-voice-vars)
  (set! current-voice-name '())
  (set! current-voice-index '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getter/Commands for users: display stuffs for users ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get the names of all current pieces.
(define (get-all-pieces-names!)
  (let ((all-pieces (get-all-pieces-names)))
    (if (null? all-pieces)
	(display-message (list "You haven't written any piece."))
	(display-message (list "All pieces: " (environment-variables
					       session-environment)))))
  'done)

(define (get-current-piece-name!)
  (display-message (list "The current piece is:" current-piece-name))
  'done)

;; display full voice body
(define (get-current-voice-piece!)
  (if (null? current-voice-name)
      (display-message (list "Oops, you haven't selected a voice to edit."))
      (begin
	(let ((voice-body (get-current-voice-body)))
	  (display-message (list "Here's your current voice" (car voice-body)))
	  (display-measures (map (lambda (measure index)
			           (list
				    (string-append "Measure-" (number->string index))
				    measure))
				 (cadr voice-body) (iota (length (cadr voice-body))))))))
  'done)

;; display full piece body
(define (get-current-piece!)
  (if (null? current-piece-name)
      (display-message (list "Oops, you haven't selected a piece to compose."))
      (begin
	(let ((body (get-current-piece-body)))
	  (display-message (list "Here's your current piece:"))
	  ;; just new line instead 
	  (for-each (lambda (voice index)
			  (let ((all-measures    
			     	(map (lambda (measure index)
				       (list
					(string-append "Measure-" (number->string index))
					measure))
				     (cadr voice) (iota (length (cadr voice))))))
			    (display-message (list "Voice" (car voice)))
			    (if (null? all-measures)
				(display-message (list "No measure added yet."))
				(display-measures all-measures))))
		    body (iota (length body))))))
  'done)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands to compose ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-new-piece! piece-name)
  (define-variable! piece-name (list ) session-environment)
  (set! current-piece-name piece-name)
  (display-message (list "You're starting a new piece:" piece-name))
  (display-message (list "Please start by defining a voice for your first voice.")))

(define (define-new-voice! voice-name)
  (set! current-voice-name voice-name)
  ;; append new voice to body
  (let ((piece-body (get-current-piece-body)))
    (save-body (append! piece-body (list (list voice-name (list  ))))))
  (update-current-voice-index)
  (display-message (list "You're now starting voice" voice-name
			 "at index:" current-voice-index))
  (get-current-piece!))
  
;; Can only add by measure and section.
(define (add! . expr)
  (let ((new-additions expr) ; TODO use parse 
	(voice-body (get-current-voice-measures)))
    (if (measure? expr)
	(save-voice (append! voice-body (list new-additions))) ; keep measure as 1 list
	(save-voice (append! voice-body new-additions))) ; like extend   
    (get-current-voice-piece!)))

;; Insert by splitting two portion. Helper for insert!.
(define (insert-at index element lst)
  (if (= index 0)
      (cons element lst) ; add to end
      (cons (car lst)    ; add to front
            (insert-at (- index 1) element (cdr lst))))) 

(define (insert! insert-i new-measure)
  (let ((current-body (get-current-voice-measures)))
    (save-voice (insert-at insert-i new-measure current-body)))
  (get-current-voice-piece!))

(define (delete-by-index index lst)
  (cond ((null? lst) '())  ; empty
        ((= index 0) (cdr lst)) ; return the rest of the list
        (else (cons (car lst) (delete-by-index (- index 1) (cdr lst))))))

;; Given the index of the measure, delete the measure.
(define (delete! delete-i)
  (let ((current-body (get-current-voice-measures)))
    (save-voice (delete-by-index delete-i current-body)))
  (get-current-voice-piece!))  


;; Given the index of the measure, replace it with the new one.
(define (edit-measure! index new-measure)
  (let ((current-body (get-current-voice-measures)))
    (save-voice (map (lambda (measure i)
		      (if (= index i)
			  new-measure
			  measure))
		      current-body (iota (length current-body)))))
  (get-current-voice-piece!))

(define (switch-voice! new-voice)
  (let ((new-voice-i (find-index-by-first-element (get-current-piece-body) new-voice)))
    (if new-voice-i
	(begin
	  (set! current-voice-name new-voice)
	  (set! current-voice-index new-voice-i)
	  (display-message (list "You're currently working on voice" new-voice))
	  (get-current-voice-piece!))
	(begin
	  (display-message (list "The system cannot find" new-voice "in the current piece."
				 "Please make sure you spell the name correctly."))
	  (get-current-piece!))))
  'done)

(define (switch-piece! new-piece-name)
  (let ((all-pieces-names (get-all-pieces-names)))
    (if (member new-piece-name all-pieces-names)
	(begin
	  (set! current-piece-name new-piece-name)
	  (reset-voice-vars)
	  (display-message (list "You're currently working on piece" new-piece-name))
	  (get-current-piece!)
	  (display-message (list "Please start by selecting a voice to start editing.")))
	(begin
	  (display-message (list "The system cannot find" new-piece-name
				 "in the environment."))
	  (get-all-pieces-names!))))
  'done)



;; TODO fix it
;; Show pdf of the current piece, use converter from lilypond
(define (show-pdf!)
  (apply convert-piece (get-current-piece-body)))


(get-current-piece-body)

(get-current-piece-body)




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

#|
;;; testing UI
(start-composing 'nhung)
(get-all-pieces-names!)
(define-new-piece! 'twinkle)

(define-new-voice! 'one)

(get-all-pieces-names!)
(get-current-piece!)


(add! (list
	   (list "test" 1) ; meta
	   (list (list "A#4" "Bb3" "2")
		 (list "G#2" "Bb1" "2"))))


(add! (list
	   (list "test-meta 2" 2) ; meta
	   (list (list "A#4" "Bb3" "2")
		 (list "G#2" "Bb1" "2"))))


(insert! 1 (list
	  (list "test-meta new insert at 1" 2) ; meta
	  (list (list "A#4" "Bb3" "2")
		(list "G#2" "Bb1" "2"))))


(delete! 1)
(delete! 0)

(define-new-voice! 'two)

(add! (list
	   (list "test" 1) ; meta
	   (list (list "A#4" "Bb3" "2")
		 (list "G#2" "Bb1" "2"))))



(add! (list
	   (list "test-meta 2" 2) ; meta
	   (list (list "A#4" "Bb3" "2")
		 (list "G#2" "Bb1" "2"))))


(insert! 1 (list
	  (list "test-meta new insert at 1" 2) ; meta
	  (list (list "A#4" "Bb3" "2")
		(list "G#2" "Bb1" "2"))))


(delete! 1)

(get-current-voice-piece!)  
(get-current-piece!)

(edit-measure! 1 (list
		  (list "new edit at 1" 2) ; meta
		  (list (list "A#4" "Bb3" "2")
			(list "G#2" "Bb1" "2"))))

(get-current-piece!)
(switch-voice! 'one)
(switch-voice! 'doesnotexist)


(define-new-piece! 'twinkle1)
(define-new-voice! 'one)
(switch-piece! 'twinkle1)
(switch-piece! 'doesnotexist)
(switch-piece! 'twinkle)
|#


;; test with real content
(start-composing 'nhung)
(define-new-piece! 'twinkle)   
(define-new-voice! 'one)

(add! (list (list "4/4" (list "C" "major") "treble")
	    (list "C4" "4") (list "D4" "4") (list "E4" "4") (list "F4" "4")))

(add! (list (list "4/4" (list "C" "major") "treble")
	    (list "G4" "4") (list "A4" "4") (list "C4" "E4" "G4" "2")))

(define-new-voice! 'two)

(add! (list (list "4/4" (list "C" "major") "bass")
	    (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2") ))

(add!  (list (list "4/4" (list "C" "major") "bass")
	     (list "B4" "4") (list "D4" "4") (list "F4" "A4" "C4" "2") ))



(get-current-piece!)
(get-current-piece-body)

(apply convert-piece (get-current-piece-body))

(show-pdf!)

#|
(define example (list
  (list (list "voice1") (list (list "4/4" (list "C" "major") "treble")  (list "C4" "4") (list "D4" "4") (list "E4" "4") (list "F4" "4"))
			      (list (list "4/4" (list "C" "major") "treble")  (list "G4" "4") (list "A4" "4") (list "C4" "E4" "G4" "2")))
			(list (list "voice2") (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2") )
			      (list (list "4/4" (list "C" "major") "bass")  (list "B4" "4") (list "D4" "4") (list "F4" "A4" "C4" "2") ))))


(apply convert-piece example)
|#



(define (edit-metadata-time time measure-start measure-end measure-list)
  (let ((measure-list measure-list #|(get-current-voice|#)
	(start-idx (- measure-start 1))
	(end-idx (- measure-end 1))
	(edit-length (+ (- measure-end measure-start) 1))
	)
    (let ((edit (list-head (list-tail measure-list start-idx) edit-length))
	  (front (list-head measure-list start-idx))
	  (end (list-tail measure-list measure-end))
	  )
      (print (length edit))
      ;;set voice to 
      (append front (map (lambda (measure) (list (list time (get-key measure) (get-clef measure)) (cdr measure))) edit) end)
    )
  )
  )

(define (edit-metadata-key key measure-start measure-end measure-list)
  (let ((measure-list measure-list #|(get-current-voice|#)
	(start-idx (- measure-start 1))
	(end-idx (- measure-end 1))
	(edit-length (+ (- measure-end measure-start) 1))
	)
    (let ((edit (list-head (list-tail measure-list start-idx) edit-length))
	  (front (list-head measure-list start-idx))
	  (end (list-tail measure-list measure-end))
	  )
   
      ;;set voice to 
      (append front (map (lambda (measure) (list (list (get-time measure) key (get-clef measure)) (cdr measure))) edit) end)
    )
  )
  )

(define (edit-metadata-clef clef measure-start measure-end measure-list)
  (let ((measure-list measure-list #|(get-current-voice|#)
	(start-idx (- measure-start 1))
	(end-idx (- measure-end 1))
	(edit-length (+ (- measure-end measure-start) 1))
	)
    (let ((edit (list-head (list-tail measure-list start-idx) edit-length))
	  (front (list-head measure-list start-idx))
	  (end (list-tail measure-list measure-end))
	  )
      (print (length edit))
      ;;set voice to 
      (append front (map (lambda (measure) (list (list (get-time measure) (get-key measure) clef) (cdr measure))) edit) end)
    )
  )
)


(edit-metadata-clef "treble" 2 3 (list (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2") )
		     (list (list "3/4" (list "F" "major") "bass")  (list "B3" "4") (list "D4" "4") (list "F4" "A4" "C4" "4") )
		     (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2") )
		     (list (list "3/4" (list "F" "major") "bass")  (list "B3" "4") (list "D4" "4") (list "F4" "A4" "C4" "4") )
		     (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2") )
		     (list (list "3/4" (list "F" "major") "bass")  (list "B3" "4") (list "D4" "4") (list "F4" "A4" "C4" "4") )
		     ))

(edit-metadata-key (list "E" "minor")  1 4 (list (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2") )
		     (list (list "3/4" (list "F" "major") "bass")  (list "B3" "4") (list "D4" "4") (list "F4" "A4" "C4" "4") )
		     (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2") )
		     (list (list "3/4" (list "F" "major") "bass")  (list "B3" "4") (list "D4" "4") (list "F4" "A4" "C4" "4") )
		     (list (list "4/4" (list "C" "major") "bass")  (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "G4" "2") )
		     (list (list "3/4" (list "F" "major") "bass")  (list "B3" "4") (list "D4" "4") (list "F4" "A4" "C4" "4") )
		     ))
