#lang racket/base
(require racket/string
         racket/list
         racket/match
         net/url)
(provide (all-defined-out))

;; See the notes in this directory's README.md for some terminology
;; definitions and implementation naming conventions.

(define-logger jeremiah)

;; ----------------------------------------
;; URL utils

;; local-url : URL -> URL
(define (local-url u #:who [who 'local-url])
  (match u
    [(url scheme user host port #t path '() #f)
     (url #f     #f   #f   #f   #t path '() #f)]
    [_ (error who "cannot convert to local URL: ~e" u)]))

;; build-url : URL String ... -> URL
(define (build-url base . paths)
  (define (trim-final-/ pps)
    (cond [(and (pair? pps) (match (last pps) [(path/param "" '()) #t] [_ #f]))
           (drop-right pps 1)]
          [else pps]))
  (match base
    [(url scheme user host post #t pps '() #f)
     (define new-pps
       (for*/list ([path (in-list paths)] [part (in-list (regexp-split #rx"/" path))])
         (path/param part null)))
     (define pps* (append (trim-final-/ pps) new-pps))
     (url scheme user host post #t pps* '() #f)]))

;; build-link : URL/String String ... -> String
(define (build-link base #:local? [local? #t] . paths)
  (let* ([base (if (url? base) base (string->url base))]
         [base (if local? (local-url base) base)])
    (url->string (apply build-url base paths))))

;; no-end-/ : String -> String
(define (no-end-/ str) (regexp-replace #rx"/$" str ""))

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
