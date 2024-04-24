(load "~/Symbolic-System-Final-Project/parser.scm")



(define (get-environment)
  ;; TODO
  (list (list 'piece) )
  )

(define (set-environment)
  ;; TODO
  (list (list 'piece) )
  )


(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

(pp error)
(define (create title composer)
  (let ((environment (get-environment)))
    (get-environment)
  )


    ;;   (let lp ((var environment))
    ;;   (if (= 0 (length var))
    ;; 	 (error "Pieces not instantiated in environment") 
    ;; 	  (if (tagged-list? (car var) 'piece)
    ;; 	      (
    ;; 	      (lp
    ;; 	       )
    ;; 	      ))
    ;;   )
    ;; )

;; ('piece (title) (composer) (voices (<voice1>) (<voice2>)))
;; voice1 = ((clef time signature key measure) (clef time signature key measure))
