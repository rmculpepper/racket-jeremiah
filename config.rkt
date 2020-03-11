#lang racket/base
(provide (all-defined-out))

;; ============================================================
;; Build configuration

(define root-dir (make-parameter #f))
(define post-src-dir (make-parameter #f))
(define cache-dir (make-parameter #f))

(define (get-post-src-dir)
  (cond [(post-src-dir) => values]
        [(root-dir) => (lambda (root) (build-path root "_posts"))]
        [else (error 'get-post-src-dir "not set")]))

(define (get-cache-dir #:fail-ok? [fail-ok? #f])
  (cond [(cache-dir) => values]
        [(root-dir) => (lambda (root) (build-path root "_cache"))]
        [fail-ok? #f]
        [else (error 'get-cache-dir "not set")]))

(define (get-post-cache-dir #:fail-ok? [fail-ok? #f])
  (cond [(get-cache-dir #:fail-ok? #t)
         => (lambda (d) (build-path d "posts"))]
        [fail-ok? #f]
        [else (error 'get-post-cache-dir "not set")]))

;; ============================================================
;; Page configuration
