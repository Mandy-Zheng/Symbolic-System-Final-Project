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
(define-new-piece! 'twinkle1)   

(define-new-voice! 'right)

;; start by adding measure or section

;; a measure
(add! '((4 4 (C major) treble) (C4 4) (C4 4) (G4 4) (G4 4)))

;; a whole section!
(add! '((4 4 (C major) treble) (A4 4) (A4 4) (G4 2) || (F4 4) (F4 4) (E4 4) (E4 4) || (D4 4) (D4 4) (C4 2)))

;; another section
(add! '((4 4 (C major) treble) (C4 4) (C4 4) (G4 4) (G4 4) || (A4 4) (A4 4) (G4 2) || (F4 4) (F4 4) (E4 4) (E4 4) || (D4 4) (D4 4) (C4 2)))

;; show the current piece
(get-current-piece!)

;; so nice!
(play-music!)

;; oh no! what about up above the world?! we forgot the middle section
(insert! 5 '((4 4 (C major) treble) (G4 4) (G4 4) (F4 4) (F4 4) || (E4 4) (E4 4) (D4 2) || (G4 4) (G4 4) (F4 4) (F4 4) || (E4 4) (E4 4) (D#4 2) || (E4 4) (E4 4) (D#4 2)))

;; we have a duplicate measure at the end!
(delete! 9)

;; but, our last note is still wrong...

;; let's fix it!
(edit-note! 8 3 '(C4 2))

;(show-pdf!)


;; let's create a new piece to make things more interesting

(define-new-piece! 'twinkle2)

;; new voice in new piece
(define-new-voice! 'left)

;; the entire part
(add! '((4 4 (C major) bass) (C3 E3 G3 1) || (F3 A3 2) (C3 E3 G3 2) || (F3 A3 2) (C3 G3 2) || (F3 G3 2) (C3 G3 2)))

(add! '((4 4 (C major) bass) (G3 1) || (G3 1) || (G3 1) || (G3 1)))

(add! '((4 4 (C major) bass) (C3 E3 G3 1) || (F3 A3 2) (C3 E3 G3 2) || (F3 A3 2) (C3 G3 2) || (F3 G3 2) (C3 G3 2)))

(get-all-pieces-names!)
(get-current-piece!)

;; now, switch!
(switch-piece! 'twinkle1)
(get-current-piece!)

;; merge them!
(define-new-voice! 'left)
(copy-voice-to-current-voice! 'twinkle2 'left 1)

(get-current-piece!)


(get-current-voice-piece!)

;; let's go to D major for the last section
(transpose-section! 2 9 12)


;; but, the key signature is now wrong...
(edit-key! 9 12 '(D major))

;; our masterpiece!
(show-pdf!)
(play-music!)
