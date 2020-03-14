#lang racket/base
(require racket/string
         racket/list
         racket/match
         net/url)
(provide (all-defined-out))

;; ============================================================
;; Terminology

;; "url" refers to instances of the net/url struct, and "link" refers to the
;; string representation of a URL. A url or link can be "full" (includes scheme
;; and host) or "local" (no scheme or host, but absolute path including
;; prefix). If "full" is not specified, "local" is implicit. A "rel-www" is like
;; a local link with the prefix removed (and no initial "/").

;; Examples:
;; - "https://mysite.com/prefix/feeds/neato.atom.xml"   -- full link
;; - "/prefix/feeds/neato.atom.xml"                     -- local link
;; - "feeds/neato.atom.xml"                             -- rel-www


;; ============================================================
;; Site configuration

(define site-author (make-parameter "Site Author"))
(define site-title (make-parameter "Site Title"))
(define site-max-feed-items (make-parameter 100))
(define site-posts-per-page (make-parameter 10))

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

(define-get-path (get-static-src-dir) "static")
(define-get-path (get-post-src-dir)   "posts")
(define-get-path (get-cache-dir)      "cache")
(define-get-path (get-post-cache-dir) "cache" "posts")
(define-get-path (get-dest-dir)       "build")
(define-get-path (get-feeds-dest-dir) "build" "feeds")
(define-get-path (get-tags-dest-dir)  "build" "tags")

;; URL

;; root-url : (Parameterof URL) -- not string!
(define base-url (make-parameter #f))

(define (get-base-url #:who [who 'get-full-base-url])
  (or (base-url) (error who "base URL not set")))

(define-syntax-rule (define-get-url (getter x ...) relpath ...)
  (define (getter x ... #:who [who 'getter])
    (build-url (get-base-url #:who who) relpath ...)))

(define-get-url (get-tags-url)  "tags")
(define-get-url (get-feeds-url) "feeds")
(define-get-url (get-tag-url tag) "tags" (format "~a.html" (slug tag)))
(define-get-url (get-atom-feed-url tag) "feeds" (format "~a.atom.xml" (slug tag)))

(define (get-base-link-no-slash #:who [who 'get-base-link-no-slash])
  (no-end-/ (url->string (local-url (get-base-url #:who who)))))

(define (get-tag-full-link tag) (url->string (get-tag-url tag)))
(define (get-tag-link tag) (url->string (local-url (get-tag-url tag))))
(define (get-atom-feed-full-link tag) (url->string (get-atom-feed-url tag)))
(define (get-atom-feed-link tag) (url->string (local-url (get-atom-feed-url tag))))

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


;; ============================================================
;; Tag URI configuration

;; References for "tag" URI scheme:
;; - https://tools.ietf.org/html/rfc4151

(define tag-uri-entity (make-parameter #f)) ;; eg, "ryanc@racket-lang.org,2020"
(define tag-uri-prefix (make-parameter #f)) ;; eg, "blog:"

;; Feed examples:
;; - tag:me@mysite.com,2020:blog:feed/all.atom.xml
;; - tag:me@mysite.com,2020:blog:feed/tag-slug.atom.xml
;; Post examples:
;; - tag:me@mysite.com,2020:blog:2020/01/01/title-slug

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

(define page-renderer (make-parameter #f))
(define (get-page-renderer)
  (or (page-renderer) (error 'get-page-renderer "not set")))

(define post-renderer (make-parameter #f))
(define (get-post-renderer)
  (or (post-renderer) (error 'get-post-renderer "not set")))

(define index-entry-renderer (make-parameter #f))
(define (get-index-entry-renderer)
  (or (index-entry-renderer) (error 'get-index-entry-renderer "not set")))

;; ============================================================
;; Write phase state

;; During the write phase, the following parameters are set so that
;; renderers and templates can use them.

(define the-site (make-parameter #f)) ;; site%
