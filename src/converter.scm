


(define (convert-metadata metadata)
  (format #t
  "global = {
    \time 4/4
    \numericTimeSignature
    \key c \major
  }"
  (time-signature metadata)
  (key metadata)
  )
)

(define (convert-music)
  "cf = \relative {
  \clef bass
  \global
  c4 c' b a |
  g a f d |
  e f g g, |
  c1
}"
  )
;;relative pitches to specified one

;; \relative c' {
;;  c4 d e f | g2 g4 | a8 a g4 f2 | e1 | r2 r4 r8 r16 r32 r64 |
;; }
;;times with duration

;;c is middle c
;; c, is lower by one octave
;; c' is higher by one octave


;;\relative c' {
;;  \key g \major % Change to the key of G major
;;  c4 d e f | g2 g4 | a8 a g4 f2 | e1 | r2 r4 r8 r16 r32 r64 |
;;}
