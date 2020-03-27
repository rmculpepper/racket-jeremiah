#lang racket/base
(require racket/contract/base
         racket/string
         racket/list
         racket/match
         net/url
         "util.rkt")
(provide call/allow-configuration
         ;; ... names defined by defconfig ...
         the-site)

;; ============================================================
;; User configuration support

(define (call/allow-configuration proc)
  (parameterize ((the-config-h default-config))
    (proc)))

(define the-config-h (make-parameter #f))
(define default-config '#hash()) ;; mutated
(define no-value (void)) ;; represents unconfigured key

(define (get/set/c ctc) (case-> (-> ctc) (-> ctc any)))

(define-syntax-rule (defconfig name ctc default)
  (begin
    (set! default-config (hash-set default-config 'name default))
    (define name (config-get/set 'name))
    (provide (contract-out [name (get/set/c ctc)]))))


(define (config-get/set key)
  (define (check-configuring)
    (unless (hash? (the-config-h))
      (error key "not currently allowing configuration")))
  (case-lambda
    [()
     (check-configuring)
     (define v (hash-ref (the-config-h) key))
     (if (eq? v no-value) (error key "not configured") v)]
    [(value)
     (check-configuring)
     (the-config-h (hash-set (the-config-h) key value))]))

;; ----------------------------------------
;; Site options

(defconfig site-title string? no-value)
(defconfig site-author string? no-value)
(defconfig site-max-feed-items exact-positive-integer? 100)
(defconfig site-posts-per-page exact-positive-integer? 10)

;; ----------------------------------------
;; Paths

(defconfig base-dir complete-path? no-value)
(defconfig pre-static-src-dirs (listof complete-path?) null)

;; ----------------------------------------
;; URL

(defconfig base-url url? no-value)

;; ----------------------------------------
;; Tag URI (for Atom IDs)

;; References for "tag" URI scheme:
;; - https://tools.ietf.org/html/rfc4151

;; Feed examples:
;; - tag:me@mysite.com,2020:blog:feed/all.atom.xml
;; - tag:me@mysite.com,2020:blog:feed/tag-slug.atom.xml
;; Post examples:
;; - tag:me@mysite.com,2020:blog:2020/01/01/title-slug

(defconfig tag-uri-entity string? no-value)     ;; eg, "me@mysite.com,2020"
(defconfig tag-uri-prefix (or/c #f string?) "") ;; eg, "blog"

;; ----------------------------------------
;; Post paths

(defconfig permalink-pattern string? "{year}/{month}/{title}")
(defconfig draft-pattern string? "draft/{title}")

;; ----------------------------------------
;; Rendering hooks

(define page/c any/c) ;; post% or index-page% object
(define post/c any/c) ;; post% object
(define html/c any/c) ;; whatever is allowed by template processing

(defconfig page-renderer (-> page/c html/c) void)
(defconfig post-renderer (-> post/c html/c) void)
(defconfig index-entry-renderer (-> post/c html/c) void)

(defconfig extra-html (-> symbol? page/c html/c) void)


;; ============================================================
;; Write phase state

;; During the write phase, the following parameters are set so that
;; renderers and templates can use them.

(define the-site (make-parameter #f)) ;; site%
