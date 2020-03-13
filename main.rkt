#lang racket/base
(require racket/match
         racket/class
         racket/path
         racket/file
         racket/string
         net/url
         (only-in racket/sequence in-slice)
         (only-in markdown xexpr->string)
         "config.rkt"
         "util.rkt"
         "private/post.rkt"
         "private/template.rkt")

;; ----------------------------------------

;; FIXME: option to ignore cache timestamps? (or maybe just for scribble?)
;; FIXME: option to avoid updating cache?
;; FIXME: option to only update cache, avoid writing to dest-dir?

;; go : -> Void
(define (go [dir (current-directory)])

  ;; Load site.rkt for configuration
  (parameterize ((current-directory (path->complete-path dir)))
    (dynamic-require (build-path (current-directory) "site.rkt") #f))

  ;; Find all post sources
  ;; FIXME: generalize to multiple dirs?
  (define post-src-paths (find-files post-src-path? (get-post-src-dir)))

  (define srcs (map path->postsrc post-src-paths))
  (check-duplicate-post-src srcs)

  ;; First, build to cache
  (with-delay-exceptions
    (for ([src (in-list srcs)])
      (delay-exception (build/cache-post src))))

  ;; Read metadata from cache, build index
  (define infos
    (for/list ([src (in-list srcs)])
      (read-post-info src)))

  (define index (build-index infos))

  ;; Delete existing dest-dir
  ;; (delete-directory/files (get-dest-dir))

  (let ([prev-h (postindex-prev index)]
        [next-h (postindex-next index)])
    (for ([info (in-list infos)] #:when (send info render?))
      (write-post info
                  (hash-ref (postindex-prev index) info #f)
                  (hash-ref (postindex-next index) info #f))))

  ;; Write main index
  ;; FIXME

  ;; Write tag pages
  ;; FIXME

  ;; Write feeds
  (write-atom-feed index #f)
  ;; FIXME

  infos)

;; check-duplicate-post-src : (Listof postsrc) -> Void
(define (check-duplicate-post-src srcs)
  (define seen (make-hash)) ;; Hash[String => postsrc]
  (with-delay-exceptions
    (for ([src (in-list srcs)])
      (delay-exception
       (cond [(hash-ref seen (postsrc-name src) #f)
              => (lambda (prev-src)
                   (j-error "duplicate post name\n  path: ~e\n  previous path: ~e"
                            (postsrc-path src) (postsrc-path prev-src)))]
             [else (hash-set! seen (postsrc-name src) src)])))))

;; ============================================================
;; Indexes

(struct postindex
  (posts  ;; (Listof PostInfo)  -- sorted most recent first FIXME !!!
   next   ;; Hash[PostInfo => PostInfo]
   prev   ;; Hash[PostInfo => PostInfo]
   ) #:prefab)

;; build-index : (Listof Postinfo) -> PostIndex
(define (build-index infos0)
  (define infos (filter (lambda (info) (send info index?)) infos0))
  (define sorted-infos (sort infos string>? #:key (lambda (info) (send info sortkey))))
  (define next (make-hasheq))
  (define prev (make-hasheq))
  (for ([info (in-list sorted-infos)]
        [next-info (in-list (if (pair? sorted-infos) (cdr sorted-infos) '()))])
    (hash-set! next info next-info)
    (hash-set! prev next-info info))
  (postindex sorted-infos next prev))

;; ============================================================
;; Write Post

;; write-post : PostInfo PostInfo/#f PostInfo/#f -> Void
;; Note: prev = older, next = newer
(define (write-post post prev-post next-post)
  (define env (hash 'post post 'prev-post prev-post 'next-post next-post))
  (make-directory* (send post get-out-dir))
  (with-output-to-file (build-path (send post get-out-dir) "index.html") #:exists 'replace
    (lambda () (write-string (render-post post prev-post next-post))))
  (parameterize ((current-directory (send post get-cachedir)))
    (for ([file (in-list (find-files file-exists?))]
          #:when (not (dont-copy-file? file)))
      (copy-file file (build-path (send post get-out-dir) file)))))

;; render-post : PostInfo PostInfo/#f PostInfo/#f -> String
(define (render-post post prev-post next-post)
  ((get-post-renderer) post prev-post next-post))

(define (dont-copy-file? path)
  (regexp-match? #rx"^_" (path->string (file-name-from-path path))))

;; ============================================================
;; Write Index

;; write-index : Index String/#f -> Void
(define (write-index index tag)
  (define posts (postindex-posts index))
  (define title "???") ;; FIXME
  (define feed-url
    (let ([feed-file (format "~a.atom.xml" (or tag "all"))])
      (build-enc-url #:local? #t (get-feeds-url) feed-file)))
  (define num-posts (length posts))
  (define num-pages (ceiling (/ num-posts (site-posts-per-page))))
  (for ([page-num (in-range num-pages)]
        [page-posts (in-slice (site-posts-per-page) posts)])
    (define page-title
      (cond [(zero? page-num) title]
            [else (format "~a (page ~a)" title (add1 page-num))]))
    (write-index-page page-posts page-title tag feed-url page-num num-pages)))

;; write-index-page : (Listof PostInfo) String String/#f URL Nat Nat -> Void
(define (write-index-page posts title tag feed-url page-num num-pages)
  (define-values (dest-dir file-name-base)
    (cond [tag (values (get-tags-dest-dir) (slug tag))]
          [else (values (get-dest-dir) "index")]))
  (define file (file/page file-name-base page-num))
  (define rendered-posts
    (for/list ([post (in-list posts)])
      (render-index-entry post)))
  (define footer
    (cond [(> num-pages 1)
           (xexpr->string
            `(footer ,(bootstrap-pagination file-name-base page-num num-pages)))]
          [else ""]))
  (define content (string-join (append rendered-posts (list footer)) "\n"))
  (render-index title tag content)
  (with-output-to-file (build-path dest-dir file)
    #:exists 'replace
    (lambda () (write-string (render-index title tag content)))))

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

(define (render-index title tag feed content)
  ((get-index-entry-renderer) title tag content))

#|
(define site%  ;; post, index
  (class object%
    (super-new)

    (define/public (get-tags)
      _)

    (define/public (get-header-html)
      _)
    ))
|#

#|
(define index-page% ;; index, post (main index)
  (class object%
    (init-field index
                posts
                content-html)
    (super-new)

    (define/public (get-posts) (postindex-posts index))
    (define/public (get-prev p) (hash-ref (postindex-prev index) p #f))
    (define/public (get-next p) (hash-ref (postindex-next index) p #f))

    (define/public (get-content-html) content-html)

    (define/public (get-header-html)
      ...)

    (define/public (get-rel-www) ...)
    ))
|#

;; ============================================================
;; Write Atom Feed

;; References:
;; - https://validator.w3.org/feed/docs/atom.html
;; - https://tools.ietf.org/html/rfc4287

(define reserved-tags '("all" "index" "draft")) ;; FIXME?

;; write-atom-feed : Index String/#f -> Void
(define (write-atom-feed index tag)
  (define file-name (format "~a.atom.xml" (or tag "all")))
  (make-directory* (get-feeds-dest-dir))
  (with-output-to-file (build-path (get-feeds-dest-dir) file-name)
    #:exists 'replace
    (lambda () (write-string (render-atom-feed index tag)))))

;; write-atom-feed : Index String/#f -> String
;; If tag is #f, feed name is "all" but tag index is main site.
(define (render-atom-feed index tag)
  (define posts (postindex-posts index))
  (define title "TITLE");; FIXME
  (define updated (match posts ['() "N/A"] [(cons post _) (send post get-date-8601)]))
  (define feed-x
    `(feed
      ([xmlns "http://www.w3.org/2005/Atom"]
       [xml:lang "en"])
      (title ([type "text"]) ,(format "~a: ~a" (get-site-title) title))
      (author (name ,(get-site-author)))
      (link ([rel "self"] [href ,(enc-url (get-atom-feed-url (or tag "all")))]))
      (link ([rel "alternate"]
             [href ,(cond [tag (enc-url (get-tag-url tag))]
                          [else (enc-url (get-base-url))])]))
      (id ,(build-tag-uri ""))
      (updated ,updated)
      ,@(for/list ([post (in-list posts)]
                   [_ (in-range (site-max-feed-items))])
          (post->atom-feed-entry-xexpr tag post))))
  (string-append 
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
   (xexpr->string feed-x)))

;; post->atom-feed-entry-xexpr : String PostInfo -> XExpr
(define (post->atom-feed-entry-xexpr tag post)
  `(entry
    (title ([type "text"]) ,(send post get-title))
    (link ([rel "alternate"] [href ,(send post get-full-enc-url)]))
    (id ,(build-tag-uri (send post get-rel-www)))
    (published ,(send post get-date-8601))
    (updated ,(send post get-date-8601))
    ;; (author (name ,(send post get-author)))
    (content ([type "html"])
             ,(send post get-blurb-html)
             ,(cond [(send post get-more?)
                     (xexpr->string
                      `(a ([href ,(send post get-full-enc-url)])
                          (em "More" hellip)))]
                    [else ""]))))
