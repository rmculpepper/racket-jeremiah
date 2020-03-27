#lang racket/base
(require racket/class
         racket/file
         racket/path
         racket/string
         net/url
         (only-in xml [xexpr->string xml:xexpr->string])
         (only-in racket/sequence in-slice)
         "config.rkt"
         "data.rkt"
         "render.rkt"
         "xexpr.rkt")
(provide (all-defined-out))

;; ============================================================
;; Write Post

;; write-post : Post -> Void
;; Note: prev = older, next = newer
(define (write-post post)
  (make-directory* (send post get-dest-dir))
  (define page-html (send post render-page-html))
  (with-output-to-file (build-path (send post get-dest-dir) "index.html")
    #:exists 'replace
    (lambda () (write-string page-html)))
  (parameterize ((current-directory (send post get-cachedir)))
    (for ([file (in-list (find-files file-exists?))]
          #:when (not (dont-copy-file? file)))
      (define dest-file (build-path (send post get-dest-dir) file))
      (when (file-exists? dest-file) (delete-file dest-file))
      (copy-file file dest-file))))

(define (dont-copy-file? path)
  (regexp-match? #rx"^_" (path->string (file-name-from-path path))))


;; ============================================================
;; Write Index

;; write-index : Index -> Void
(define (write-index index)
  ;; FIXME: move paginate to index%, but need render-index-page% ...
  (define config (get-field config index)) ;; FIXME
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
  (define page-html (send index-page render-page-html))
  (make-parent-directory* (send index-page get-dest-file))
  (with-output-to-file (send index-page get-dest-file)
    #:exists 'replace
    (lambda () (write-string page-html))))


;; ============================================================
;; Write Atom Feed

;; References:
;; - https://validator.w3.org/feed/docs/atom.html
;; - https://tools.ietf.org/html/rfc4287

;; write-atom-feed : Index -> Void
(define (write-atom-feed index)
  (define feed-file (send index get-feed-dest-file))
  (make-parent-directory* feed-file)
  (with-output-to-file feed-file
    #:exists 'replace
    (lambda () (write-string (render-atom-feed index)))))

;; write-atom-feed : Index -> String
;; If tag is #f, feed name is "all" but tag index is main site.
(define (render-atom-feed index)
  (define config (get-field config index)) ;; FIXME?
  (define posts (send index get-posts))
  (define title "TITLE");; FIXME
  (define updated (or (send index get-updated-8601) "N/A"))
  (define feed-x
    `(feed
      ([xmlns "http://www.w3.org/2005/Atom"]
       [xml:lang "en"])
      (title ([type "text"]) ,(format "~a: ~a" (send config get-site-title) title))
      (author (name ,(send config get-site-author)))
      (link ([rel "self"] [href ,(url->string (send index get-feed-url))]))
      (link ([rel "alternate"] [href ,(url->string (send index get-index-main-url))]))
      (id ,(send index get-atom-id))
      (updated ,updated)
      ,@(for/list ([post (in-list posts)]
                   [_ (in-range (site-max-feed-items))])
          (post->atom-feed-entry-xexpr post))))
  (string-append 
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
   (xml:xexpr->string feed-x)))

;; post->atom-feed-entry-xexpr : String Post -> XExpr
(define (post->atom-feed-entry-xexpr post)
  `(entry
    (title ([type "text"]) ,(send post get-title))
    (link ([rel "alternate"] [href ,(send post get-full-link)]))
    (id ,(send post get-atom-id))
    (published ,(send post get-date-8601))
    (updated ,(send post get-date-8601))
    ,@(map (lambda (author) `(author (name ,author))) (send post get-authors))
    (content ([type "html"])
             ,(send post get-blurb-html)
             ,(cond [(send post get-more?)
                     (xexpr->html
                      `(a ([href ,(send post get-full-link)])
                          (em "More" hellip)))]
                    [else ""]))))
