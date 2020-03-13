#lang racket/base
(require racket/string
         racket/list
         net/url)
(provide (all-defined-out))

;; ============================================================
;; Site configuration

(define site-author (make-parameter "Site Author"))
(define site-title (make-parameter "Site Title"))
(define site-max-feed-items (make-parameter 100))

(define (get-site-author #:who [who 'get-site-author])
  (or (site-author) (error who "site author not set")))
(define (get-site-title #:who [who 'get-site-title])
  (or (site-title) (error who "site title not set")))


;; ============================================================
;; Build configuration

;; Directories

(define root-dir (make-parameter #f))

(define (get-root-dir #:who [who 'get-root-dir])
  (or (root-dir) (error who "root directory not set")))

(define-syntax-rule (define-get-path (getter x ...) relpath ...)
  (define (getter x ... #:who [who 'getter])
    (build-path (get-root-dir #:who who) relpath ...)))

(define-get-path (get-post-src-dir)   "_posts")
(define-get-path (get-cache-dir)      "_cache")
(define-get-path (get-post-cache-dir) "_cache" "posts")
(define-get-path (get-dest-dir)       "_build")
(define-get-path (get-feeds-dest-dir) "_build" "feeds")

;; URL

;; base-url : (Parameterof URL) -- not string!
;; PRE: path must end in "/" (represented as empty final path/param)
(define base-url (make-parameter #f))

(define (get-base-url #:who [who 'get-base-url])
  (or (base-url) (error who "base URL not set")))

(define-syntax-rule (define-get-url (getter x ...) relpath ...)
  (define (getter x ... #:who [who 'getter])
    (build-url (get-base-url #:who who) relpath ...)))
(define-syntax-rule (define-get-enc-url (getter x ...) get-url)
  (define (getter x ... #:who [who 'getter])
    (url->string (get-url x ... #:who who))))

(define-get-url (get-tags-url)  "tags")
(define-get-url (get-feeds-url) "feeds")
(define-get-url (get-tag-url tag) "tags" (format "~a.html" (slug tag)))
(define-get-url (get-atom-feed-url tag) "feeds" (format "~a.atom.xml" (slug tag)))

(define-get-enc-url (get-enc-base-url) get-base-url)
(define-get-enc-url (get-enc-tags-url) get-tags-url)
(define-get-enc-url (get-enc-feeds-url) get-feeds-url)
(define-get-enc-url (get-enc-tag-url tag) get-tag-url)
(define-get-enc-url (get-enc-atom-feed-url tag) get-atom-feed-url)

(define (get-enc-base-url-no-slash #:who [who 'get-enc-base-url-no-slash])
  (no-end-/ (get-enc-base-url #:who who)))

;; URL utils

;; build-url : URL String ... -> URL
(define (build-url url . paths)
  (for/fold ([url url]) ([path (in-list paths)])
    (combine-url/relative url path)))

;; enc-url : URL -> String
(define (enc-url url) (url->string url))

;; build-enc-url : URL String ... -> String
(define (build-enc-url url . paths)
  (url->string (apply build-url url paths)))

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


;; ============================================================
;; Tag URI configuration

;; References for "tag" URI scheme:
;; - https://tools.ietf.org/html/rfc4151

(define tag-uri-entity (make-parameter #f)) ;; eg, "ryanc@racket-lang.org,2020"
(define tag-uri-prefix (make-parameter #f)) ;; eg, "blog:"

;; Feed examples:
;; - tag:ryanc@racket-lang.org,2020:blog:feed/all.atom.xml
;; - tag:ryanc@racket-lang.org,2020:blog:feed/tag-slug.atom.xml
;; Post examples:
;; - tag:ryanc@racket-lang.org,2020:blog:2020/01/01/title-slug

;; build-tag-uri : String -> String
(define (build-tag-uri suffix #:who [who 'build-tag-uri])
  (define entity (or (tag-uri-entity) (error who "tag URI entity not set")))
  (define prefix (or (tag-uri-prefix) (error who "tag URI prefix not set")))
  (format "tag:~a:~a~a~a" entity prefix (if (equal? suffix "") "" ":") suffix))

;; ============================================================
;; Page configuration

(define permalink-pattern (make-parameter "{year}/{month}/{title}"))
(define draft-permalink-pattern (make-parameter "draft/{title}"))

(define (get-permalink-pattern)
  (cond [(permalink-pattern) => values]
        [else (error 'get-permalink-pattern "not set")]))

(define (get-draft-permalink-pattern)
  (cond [(draft-permalink-pattern) => values]
        [else (error 'get-draft-permalink-pattern "not set")]))

(define post-renderer (make-parameter #f))

(define (get-post-renderer)
  (cond [(post-renderer) => values]
        [else (error 'get-post-renderer "not set")]))
