(load "user-interface.scm")

;;;;;;;;;;;;
;;; DEMO ;;;
;;;;;;;;;;;;

(start-composing! 'nhung) 

;; explore the interface
(help!)

(show-all-display-commands!)

(show-all-commands-for-piece!)

(show-all-commands-for-voice!)

(show-all-commands-for-sections!)

(show-all-commands-for-measures!) 

(show-all-commands-for-notes!)

;; actual content
(define-new-piece! 'twinkle)   

(define-new-voice! 'one)

;; start by adding measure or section

#| TODO update all materials 
(add! (list (list "4/4" (list "C" "major") "treble")
	    (list "C4" "4") (list "D4" "4") (list "E4" "4") (list "F4" "4")))

(add! (list (list "4/4" (list "C" "major") "treble")
	    (list "G4" "4") (list "A4" "4") (list "C4" "E4" "2")))
|#

;; second voice
(define-new-voice! 'two)

;; 
(add! (list (list "4/4" (list "C" "major") "bass")
	    (list "A3" "4") (list "G#4" "4") (list "C4" "E4" "2") ))

(add!  (list (list "4/4" (list "C" "major") "bass")
	     (list "B4" "4") (list "D4" "4") (list "F4" "A4" "2") ))

;; oops, we forgot the insert: can insert by measure and section
(insert! 0  (list (list "1/4" (list "C" "major") "bass")
		  (list "B4" "4") (list "D4" "4") (list "F4" "A4" "2") ))

(insert! 0 (list
	    (list (list "4/4" (list "C" "major") "bass")
		  (list "B4" "4") (list "D4" "4") (list "F4" "A4" "2") )
	    (list (list "4/4" (list "C" "major") "bass")
		  (list "B4" "4") (list "D4" "4") (list "F4" "A4" "2"))))

;; forgot to add more to voice one so we can switch to it
(switch-voice! 'one) 

; add section
(add! (list
       (list (list "4/4" (list "C" "major") "bass")
	    (list "B4" "4") (list "D4" "4") (list "F4" "A4" "2") )
       (list (list "4/4" (list "C" "major") "bass")
	     (list "B4" "4") (list "D4" "4") (list "F4" "A4" "2"))))

;; let's see the current voice and piece
(get-current-voice-piece!)

(get-current-piece!)


;; oops, we added the one measure, let's delete it
(delete! 0)

;; by section
(delete! 1 3)

;; instead of deleting, we can also edit
(edit-measure! 0
	       ;TODO measure here 
	       )

(get-current-voice-piece!)

(get-measure! measue-i)

(edit-note! measure-i note-i new-note) 


;; Let's see the piece in pdf form

(show-pdf!)
