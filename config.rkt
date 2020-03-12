#lang racket/base
(require racket/string
         racket/list
         net/url)
(provide (all-defined-out))

;; ============================================================
;; Build configuration

;; Directories

(define root-dir (make-parameter #f))
(define post-src-dir (make-parameter #f))

(define (get-post-src-dir)
  (cond [(post-src-dir) => values]
        [(root-dir) => (lambda (root) (build-path root "_posts"))]
        [else (error 'get-post-src-dir "not set")]))

(define (get-cache-dir #:fail-ok? [fail-ok? #f])
  (cond [(cache-dir) => values]
        [(root-dir) => (lambda (root) (build-path root "_cache"))]
        [fail-ok? #f]
        [else (error 'get-cache-dir "not set")]))

(define cache-dir (make-parameter #f))
(define (get-post-cache-dir #:fail-ok? [fail-ok? #f])
  (cond [(get-cache-dir #:fail-ok? #t)
         => (lambda (d) (build-path d "posts"))]
        [fail-ok? #f]
        [else (error 'get-post-cache-dir "not set")]))

(define dest-dir (make-parameter #f))

(define (get-dest-dir)
  (cond [(dest-dir) => values]
        [(root-dir) => (lambda (root) (build-path root "_build"))]
        [else (error 'get-dest-dir "not set")]))

(define (get-tags-dir) (build-path (get-dest-dir) "tags"))
(define (get-feeds-dir) (build-path (get-feeds-dir) "feeds"))

;; URL

;; base-url : (Parameterof URL) -- not string!
(define base-url (make-parameter #f))

(define (get-base-url)
  (cond [(base-url) => values]
        [else (error 'get-base-url "not set")]))

;; path->rel-www : Path -> String
;; Convert filesystem path to URL path (not encoded) relative to URL base.
;; PRE: path is subpath of (get-dest-dir).
;; Ex: (path->rel-url "/path/to/dest/2000/03/14/x.html") = "2000/03/14/x.html"
(define (path->rel-www path)
  (define path* (simplify-path (path->complete-path path)))
  (string-join (abs->rel 'path->rel-url path* (get-dest-dir)) "/"))

;; abs->rel : Symbol Path Path -> (Listof PathSegment)
(define (abs->rel who a b)
  (define as (explode-path a))
  (define bs (explode-path b))
  (define-values (prefix tail _) (split-common-prefix as bs))
  (unless (equal? prefix bs)
    (error who "path does not extend base\n  path: ~e\n  base: ~e" b a))
  tail)

;; path->abs-url : Path -> URL
(define (path->abs-url path)
  (combine-url/relative (get-base-url) (path->rel-www path)))

;; slug : String -> String
;; Convert a string into a "slug", in which:
;; - The string is Unicode normalized to NFC form.
;; - Consecutive characters that are neither char-alphabetic? nor char-numeric?
;;   are replaced by hyphens.
;; - The string is Unicode normalized to NFD form.
(define (slug s)
  (let* ([s (string-normalize-nfc s)]
         [s (let ([cleaned-s (string-copy s)])
              (for ([c (in-string cleaned-s)] [i (in-naturals)])
                (unless (or (char-alphabetic? c) (char-numeric? c))
                  (string-set! cleaned-s i #\-)))
              cleaned-s)]
         [s (regexp-replace* #px"-{2,}" s "-")]
         [s (regexp-replace #px"-{1,}$" s "")]
         [s (regexp-replace #px"^-{1,}" s "")]) ;; breaks compat w/ Frog
    (string-normalize-nfd s)))


;; ============================================================
;; Page configuration

