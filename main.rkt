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
         "private/index.rkt"
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
  (define posts
    (for/list ([src (in-list srcs)])
      (read-post-info src)))

  (define site (new site% (posts posts)))
  (define index (build-index #f posts))

  ;; Delete existing dest-dir
  ;; (delete-directory/files (get-dest-dir))

  (for ([post (in-list posts)] #:when (send post render?))
    (write-post post (send index get-prev post) (send index get-next post) site))

  ;; Write main index
  (write-index index site)

  ;; Write tag pages
  ;; FIXME

  ;; Write feeds
  (write-atom-feed index)
  ;; FIXME

  posts)

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
