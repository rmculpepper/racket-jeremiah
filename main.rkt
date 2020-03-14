#lang racket/base
(require racket/class
         racket/file
         "config.rkt"
         "util.rkt"
         "private/data.rkt"
         "private/build.rkt"
         "private/render.rkt"
         "private/write.rkt")

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
  (define posts (map read-post srcs))
  (define index (build-index #f posts))
  (define site (new site% (index index)))

  (parameterize ((the-site site))

    ;; Delete existing dest-dir
    ;; (delete-directory/files (get-dest-dir))

    ;; Copy static resources
    (parameterize ((current-directory (get-static-src-dir)))
      (for ([file (in-list (find-files file-exists?))])
        (define src-file (build-path (get-static-src-dir) file))
        (define dest-file (build-path (get-dest-dir) file))
        (make-parent-directory* dest-file)
        (when (file-exists? dest-file) (delete-file dest-file))
        (copy-file src-file dest-file)))

    ;; Write posts
    (for ([post (in-list posts)] #:when (send post render?))
      (write-post post))

    ;; Write main index and feed
    (write-index index)
    (write-atom-feed index)

    ;; Write tag indexes and feeds
    (for ([tag (in-list (send site get-tags))])
      (define tag-index (build-index tag posts))
      (write-index tag-index)
      (write-atom-feed tag-index))

    posts))

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

;; read-post : PostSrc -> Post
(define (read-post src #:who [who 'read-post])
  (define-values (meta blurb more?) (read-post-info src #:who who))
  (new render-post% (src src) (meta meta) (blurb blurb) (more? more?)))

;; build-index : String/#f (Listof Post) -> Index
(define (build-index tag posts)
  (define sorted-posts
    (sort (filter (lambda (post) (send post index? tag)) posts)
          string>?
          #:key (lambda (post) (send post sortkey))))
  (new index% (tag tag) (posts sorted-posts)))


;; ----------------------------------------
(require racket/lazy-require)
(lazy-require ["private/preview.rkt" (preview)])
(provide preview)
