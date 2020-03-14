#lang racket/base
(require racket/match
         racket/class
         racket/path
         racket/file
         racket/string
         net/url
         (only-in racket/sequence in-slice)
         (only-in markdown xexpr->string)
         "../config.rkt"
         (prefix-in config: "../config.rkt")
         "post.rkt")
(provide (all-defined-out))

;; ============================================================
;; Site information

(define site%
  (class object%
    (init-field posts)
    (super-new)

    (define/public (get-posts) posts)
    (define/public (get-tags)
      (define h (make-hash))
      (for ([post (in-list posts)]
            #:when (send post index?)
            [tag (in-list (send post get-tags))])
        (hash-set! h tag #t))
      (sort (hash-keys h) string<?))

    (define/public (get-header-html)
      (xexprs->html (get-header-xexprs)))
    (define/public (get-header-xexprs)
      (define (mkcss path)
        `(link ([rel "stylesheet"] [type "text/ccs"]
                [href ,(build-link #:local? #t (get-base-url) path)])))
      `((meta ([charset "utf-8"]))
        (meta ([name "viewport"] [content "width=device-width, initial-scale=1.0"]))
        (link ([rel "icon"] [href ,(build-link #:local? #t (get-base-url) "favicon.ico")]))
        ;; CSS
        ,(mkcss "css/bootstrap.min.css")
        ,(mkcss "css/pygments.css")
        ,(mkcss "css/scribble.css")
        ,(mkcss "css/custom.css")))

    ;; Util

    (define/public (link [path ""]) (build-link #:local? #t (get-base-url) path))
    (define/public (full-link [path ""]) (build-link (get-base-url) path))
    (define/public (uri-prefix) (get-base-link-no-slash))
    ))

(define page<%>
  (interface ()
    get-rel-www
    get-header-html

    ;; Note: page does not include content-html!
    ))


;; ============================================================
;; Indexes

;; Index = instance of index%
(define index%
  (class object%
    (init-field tag     ;; String/#f
                posts)  ;; (Listof PostInfo), sorted most recent first
    (super-new)

    (define prev-h (make-hasheq)) ;; Hasheq[PostInfo => PostInfo/#f]
    (define next-h (make-hasheq)) ;; Hasheq[PostInfo => PostInfo/#f]
    (for ([post (in-list posts)]
          [next-post (in-list (if (pair? posts) (cdr posts) '()))])
      (hash-set! next-h post next-post)
      (hash-set! prev-h next-post post))

    (define/public (get-title)
      (cond [tag (format "Posts tagged '~a'" tag)]
            [else (site-title)]))
    (define/public (get-tag) tag)
    (define/public (get-posts) posts)
    (define/public (get-prev p) (hash-ref prev-h p #f))
    (define/public (get-next p) (hash-ref next-h p #f))

    (define/public (get-updated-8601)
      (and (pair? posts) (send (car posts) get-date-8601)))

    (define/public (get-feed-file-name)
      (format "~a.atom.xml" (or tag "all")))
    (define/public (get-feed-dest-file)
      (build-path (get-feeds-dest-dir) (get-feed-file-name)))
    (define/public (get-feed-url)
      (build-url (get-feeds-url) (get-feed-file-name)))
    (define/public (get-feed-local-link)
      (url->string (local-url (get-feed-url))))

    (define/public (get-tag-url) ;; no tag => base url (implicit /index.html)
      (if tag (config:get-tag-url tag) (get-base-url)))

    (define/public (get-tag-dest-file-name-base)
      (if tag (slug tag) "index"))
    (define/public (get-tag-dest-dir)
      (if tag (get-tags-dest-dir) (get-dest-dir)))
    ))

;; IndexPage = instance of index-page%
(define index-page%
  (class object%
    (init-field index           ;; Index
                posts           ;; (Listof Post)
                page-num        ;; Nat
                num-pages)      ;; Nat
    (super-new)

    (define/public (get-index) index)
    (define/public (get-posts) posts)
    (define/public (get-page-num) page-num)
    (define/public (get-num-pages) num-pages)
    (define/public (get-title)
      (cond [(zero? page-num) (send index get-title)]
            [else (format "~a (page ~a)" (send index get-title) (add1 page-num))]))

    (define/public (get-dest-file-name)
      (file/page (send index get-tag-dest-file-name-base) page-num))
    (define/public (get-dest-file)
      (build-path (send index get-tag-dest-dir) (get-dest-file-name)))

    (define/public (get-url)
      (let ([tag (send index get-tag)])
        (cond [tag (build-url (get-tags-url) (get-dest-file))]
              [else (build-url (get-base-url) (get-dest-file-name))])))
    (define/public (get-local-link)
      (build-link #:local? #t (get-url)))

    (define/public (get-enc-feed-local-link)
      (send index get-feed-local-link))

    (define/public (get-header-html) (xexprs->html (get-header-xexprs)))
    (define/public (get-header-xexprs)
      (define tag (send index get-tag))
      `((title ,(send index get-title))
        ;; (meta ([name "description"] [content ""])) ;; FIXME
        ,@(if tag (list `(meta ([name "keywords"] [content ,tag]))) '())
        (link ([rel "alternate"] [type "application/atom+xml"] [title "Atom Feed"]
               [href ,(send index get-feed-local-link)]))))

    (define/public (get-pagination-html)
      (define file-name-base (send index get-tag-dest-file-name-base))
      (xexpr->string `(footer ,(bootstrap-pagination file-name-base page-num num-pages))))
    ))

;; build-index : String/#f (Listof Postinfo) -> Index
(define (build-index tag posts)
  (define sorted-posts
    (sort (filter (lambda (post)
                    (and (send post index?)
                         (if tag (member tag (send post get-tags)) #t)))
                  posts)
          string>?
          #:key (lambda (post) (send post sortkey))))
  (new index% (tag tag) (posts sorted-posts)))

;; ============================================================
;; Write Post

;; write-post : PostInfo PostInfo/#f PostInfo/#f Site -> Void
;; Note: prev = older, next = newer
(define (write-post post prev-post next-post site)
  (make-directory* (send post get-out-dir))
  (define content-html (render-post post prev-post next-post))
  (define page-html (render-page post content-html site))
  (with-output-to-file (build-path (send post get-out-dir) "index.html") #:exists 'replace
    (lambda () (write-string page-html)))
  (parameterize ((current-directory (send post get-cachedir)))
    (for ([file (in-list (find-files file-exists?))]
          #:when (not (dont-copy-file? file)))
      (copy-file file (build-path (send post get-out-dir) file)))))

;; render-post : PostInfo PostInfo/#f PostInfo/#f -> String
(define (render-post post prev-post next-post)
  ((get-post-renderer) post prev-post next-post))

;; render-page : Page String Site -> String
(define (render-page page content-html site)
  ((get-page-renderer) page content-html site))

(define (dont-copy-file? path)
  (regexp-match? #rx"^_" (path->string (file-name-from-path path))))

;; ============================================================
;; Write Index

;; write-index : Index Site -> Void
(define (write-index index site)
  (define posts (send index get-posts))
  (define num-posts (length posts))
  (define num-pages (ceiling (/ num-posts (site-posts-per-page))))
  (for ([page-num (in-range num-pages)]
        [page-posts (in-slice (site-posts-per-page) posts)])
    (define index-page
      (new index-page% (index index) (posts page-posts)
           (page-num page-num) (num-pages num-pages)))
    (write-index-page index-page site)))

(define (write-index-page index-page site)
  (define rendered-posts
    (for/list ([post (in-list (send index-page get-posts))])
      (render-index-entry post)))
  (define content-html (string-join rendered-posts "\n"))
  (define page-html (render-page index-page content-html site))
  (with-output-to-file (send index-page get-dest-file)
    #:exists 'replace
    (lambda () (write-string page-html))))

(define (file/page file-name-base page-num)
  (cond [(zero? page-num) (format "~a.html" file-name-base)]
        [else (format "~a-~a.html" file-name-base page-num)]))

(define (bootstrap-pagination file-name-base page-num num-pages)
  `(ul ([class "pagination"])
       ,(cond [(zero? page-num)
               `(li ([class "page-item disabled"])
                    (a ([class "page-link"] [href "#"]) 'larr))]
              [else
               `(li ([class "page-item"])
                    (a ([class "page-link"]
                        [href ,(file/page file-name-base (sub1 page-num))])
                       'larr))])
       ,@(for/list ([n (in-range num-pages)])
           `(li ([class ,(cond [(= n page-num) "page-item active"] [else "page-item"])])
                (a ([class "page-link"]
                    [href ,(file/page file-name-base n)])
                   ,(number->string (add1 n)))))
       ,(cond [(= (add1 page-num) num-pages)
               `(li ([class "page-item disabled"])
                    (a ([class "page-link"] [href "#"]) 'rarr))]
              [else `(li ([class "page-item"])
                         (a ([class "page-link"]
                             [href ,(file/page file-name-base (add1 page-num))])
                            'rarr))])))

(define (render-index-entry post)
  ((get-index-entry-renderer) post))


;; ============================================================
;; Write Atom Feed

;; References:
;; - https://validator.w3.org/feed/docs/atom.html
;; - https://tools.ietf.org/html/rfc4287

(define reserved-tags '("all" "index" "draft")) ;; FIXME?

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
      (id ,(build-tag-uri ""))
      (updated ,updated)
      ,@(for/list ([post (in-list posts)]
                   [_ (in-range (site-max-feed-items))])
          (post->atom-feed-entry-xexpr post))))
  (string-append 
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
   (xexpr->string feed-x)))

;; post->atom-feed-entry-xexpr : String PostInfo -> XExpr
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
                     (xexpr->string
                      `(a ([href ,(send post get-full-link)])
                          (em "More" hellip)))]
                    [else ""]))))
