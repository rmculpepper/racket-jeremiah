#lang racket/base
(require (for-syntax racket/base)
         racket/stxparam)
(provide (all-defined-out))

;; ----------------------------------------

(define-syntax-parameter delay-exception
  (lambda (stx) (raise-syntax-error #f "used out of `with-delay-exception` context")))

(define-syntax-rule (with-delay-exceptions . body)
  (call/call/delay-exception
   (lambda (c/de)
     (syntax-parameterize ((delay-exception
                            (syntax-rules ()
                              [(_ . inner)
                               (c/de (lambda () . inner))])))
       . body))))

(define (call/call/delay-exception proc)
  (define exns null) ;; (Listof Exn), mutated
  (define (call/delay-exception inner-proc)
    (with-handlers ([exn:fail? (lambda (e) (set! exns (cons e exns)))])
      (inner-proc)))
  (proc call/delay-exception)
  (when (pair? exns)
    (for ([e (in-list (reverse exns))])
      ((error-display-handler) (exn-message e) e))
    (j-error "~s errors, exiting" (length exns))))

;; ----------------------------------------

(define (j-error fmt . args) (apply error 'jeremiah fmt args))
