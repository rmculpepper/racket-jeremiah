#lang racket/base
(require racket/class
         racket/file
         racket/path
         racket/string
         net/url
         (only-in racket/sequence in-slice)
         "../config.rkt"
         "data.rkt"
         "render.rkt"
         "xexpr.rkt")
(provide (all-defined-out))

;; ============================================================
;; Write Post

;; write-post : Post -> Void
;; Note: prev = older, next = newer
(define (write-post post)
  (make-directory* (send post get-out-dir))
  (define content-html (render-post post))
  (define page-html (render-page post content-html))
  (with-output-to-file (build-path (send post get-out-dir) "index.html")
    #:exists 'replace
    (lambda () (write-string page-html)))
  (parameterize ((current-directory (send post get-cachedir)))
    (for ([file (in-list (find-files file-exists?))]
          #:when (not (dont-copy-file? file)))
      (copy-file file (build-path (send post get-out-dir) file)))))

;; render-post : Post -> String
(define (render-post post)
  ((get-post-renderer) post))

;; render-page : Page String -> String
(define (render-page page content-html)
  ((get-page-renderer) page content-html))

(define (dont-copy-file? path)
  (regexp-match? #rx"^_" (path->string (file-name-from-path path))))


;; ============================================================
;; Write Index

;; write-index : Index -> Void
(define (write-index index)
  (define posts (send index get-posts))
  (define num-posts (length posts))
  (define num-pages (ceiling (/ num-posts (site-posts-per-page))))
  (for ([page-num (in-range num-pages)]
        [page-posts (in-slice (site-posts-per-page) posts)])
    (define index-page
      (new render-index-page% (index index) (posts page-posts)
           (page-num page-num) (num-pages num-pages)))
    (write-index-page index-page)))

(define (write-index-page index-page)
  (define rendered-posts
    (for/list ([post (in-list (send index-page get-posts))])
      (render-index-entry post)))
  (define content-html (string-join rendered-posts "\n"))
  (define page-html (render-page index-page content-html))
  (make-parent-directory* (send index-page get-dest-file))
  (with-output-to-file (send index-page get-dest-file)
    #:exists 'replace
    (lambda () (write-string page-html))))

(define (render-index-entry post)
  ((get-index-entry-renderer) post))


;; ============================================================
;; Write Atom Feed

;; References:
;; - https://validator.w3.org/feed/docs/atom.html
;; - https://tools.ietf.org/html/rfc4287

;; write-atom-feed : Index -> Void
(define (write-atom-feed index)
  (make-directory* (get-feeds-dest-dir))
  (with-output-to-file (send index get-feed-dest-file)
    #:exists 'replace
    (lambda () (write-string (render-atom-feed index)))))

;; write-atom-feed : Index -> String
;; If tag is #f, feed name is "all" but tag index is main site.
(define (render-atom-feed index)
  (define posts (send index get-posts))
  (define title "TITLE");; FIXME
  (define updated (or (send index get-updated-8601) "N/A"))
  (define feed-x
    `(feed
      ([xmlns "http://www.w3.org/2005/Atom"]
       [xml:lang "en"])
      (title ([type "text"]) ,(format "~a: ~a" (get-site-title) title))
      (author (name ,(get-site-author)))
      (link ([rel "self"] [href ,(url->string (send index get-feed-url))]))
      (link ([rel "alternate"] [href ,(url->string (send index get-tag-url))]))
      (id ,(build-tag-uri (format "feeds/~a" (send index get-feed-file-name))))
      (updated ,updated)
      ,@(for/list ([post (in-list posts)]
                   [_ (in-range (site-max-feed-items))])
          (post->atom-feed-entry-xexpr post))))
  (string-append 
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
   (xexpr->html feed-x)))

;; post->atom-feed-entry-xexpr : String Post -> XExpr
(define (post->atom-feed-entry-xexpr post)
  `(entry
    (title ([type "text"]) ,(send post get-title))
    (link ([rel "alternate"] [href ,(send post get-full-link)]))
    (id ,(build-tag-uri (send post get-rel-www)))
    (published ,(send post get-date-8601))
    (updated ,(send post get-date-8601))
    ;; (author (name ,(send post get-author)))
    (content ([type "html"])
             ,(send post get-blurb-html)
             ,(cond [(send post get-more?)
                     (xexpr->html
                      `(a ([href ,(send post get-full-link)])
                          (em "More" hellip)))]
                    [else ""]))))
