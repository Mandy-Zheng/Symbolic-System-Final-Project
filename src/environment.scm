

#|
Environment from SDF.
|#

(define initial-env-bindings '())

(define the-empty-environment (list '*the-empty-environment*))

;;; Use this to make env
(define (make-global-environment)
  (extend-environment (map car initial-env-bindings)
                      (map cdr initial-env-bindings)
                      the-empty-environment))


(define (extend-environment variables values base-environment)
  (if (not (fix:= (length variables) (length values)))
      (if (fix:< (length variables) (length values))
          (error "Too many arguments supplied" variables values)
          (error "Too few arguments supplied" variables values)))
  (vector variables values base-environment))

(define lookup-scheme-value
  (let ((env (the-environment)))
    (named-lambda (lookup-scheme-value var)
      (lexical-reference env var))))

(define (lookup-variable-value var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
        (lookup-scheme-value var)
        (let scan
            ((vars (vector-ref env 0))
             (vals (vector-ref env 1)))
          (cond ((null? vars) (plp (vector-ref env 2)))
                ((eq? var (car vars)) (car vals))
                (else (scan (cdr vars) (cdr vals))))))))

(define (environment-variables env) (vector-ref env 0))
(define (environment-values env) (vector-ref env 1))

;;; use this add to env
(define (define-variable! var val env)
  (let scan
      ((vars (vector-ref env 0))
       (vals (vector-ref env 1)))
    (cond ((null? vars)
           (vector-set! env 0 (cons var (vector-ref env 0)))
           (vector-set! env 1 (cons val (vector-ref env 1))))
          ((eq? var (car vars))
           (set-car! vals val))
          (else
           (scan (cdr vars) (cdr vals))))))

;;; use this to modify
(define (set-variable-value! var val env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var))
    (let scan
        ((vars (vector-ref env 0))
         (vals (vector-ref env 1)))
      (cond ((null? vars) (plp (vector-ref env 2)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))))


#|
;; example use
(define current-environment (make-global-environment))

(pp current-environment)

(define-variable! 'hi 2 current-environment)

(pp current-environment)

(lookup-variable-value 'hi current-environment)

(define-variable! 'another (list 'hi 'hi) current-environment)  


(environment-variables current-environment) ;Value: (another hi)
(environment-values current-environment) ; Value: ((hi hi) 2)
|#


