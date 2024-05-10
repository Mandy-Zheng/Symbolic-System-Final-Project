(load "user-interface.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bach's Fugue in G Minor ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(start-composing! 'bach) 

;; explore the interface
(help!)

(show-all-display-commands!)

(show-all-commands-for-piece!)

(show-all-commands-for-voice!)

(show-all-commands-for-sections!)

(show-all-commands-for-measures!) 

(show-all-commands-for-notes!)

(note-example!)

(measure-example!)

;; actual content
(define-new-piece! 'fugue-in-g-minor)   

(define-new-voice! 'right)

(add! '(
    (4 4 (G minor) treble) (G4 4) (D5 4) (Bb4 4.) (A4 8) ||
    (G4 8) (Bb4 8) (A4 8) (G4 8) (F#4 8) (A4 8) (D4 4) ||
    (G4 8) (D4 8) (A4 8) (D4 8) (Bb4 8) (A4 16) (G4 16) (A4 8) (D4 8) ||
    (G4 8) (D4 16) (G4 16) (A4 8) (D4 16) (A4 16) (Bb4 8) (A4 16) (G4 16) (A4 16) (D4 16) (D5 16) (C5 16) ||
    (Bb4 16) (A4 16) (G4 16) (Bb4 16) (A4 16) (G4 16) (F#4 16) (A4 16) (G4 16) (D4 16) (G4 16) (A4 16) (Bb4 16) (C5 16) (D5 16) (E5 16) ||
    (F5 16) (E5 16) (D5 16) (F5 16) (E5 16) (D5 16) (C#5 16) (E5 16) (D5 8) (A4 8) (D5 8) (E5 8) ||
    (F5 16) (G5 16) (F5 16) (G5 16) (G5 32) (A5 32) (G5 32) (A5 32) (G5 32) (A5 32) (F5 32) (G5 32) (A5 16) (G5 16) (A5 16) (Bb5 16) (A5 16) (G5 16) (F5 16) (E5 16)
))

(get-current-piece!)

(define-new-voice! 'left)

(add! '(
    (4 4 (G minor) bass) (R 1) || (R 1) || (R 1) || (R 1) || (R 1) ||
    (4 4 (G minor) treble) (D4 4) (A4 4) (F4 4.) (E4 8) ||
    (D4 8) (F4 8) (E4 8) (D4 8) (C#4 8) (E4 8) (A3 4)))

(get-current-piece!)

(show-pdf!)

(play-music!)
