#lang racket/base
(require racket/stxparam
         racket/class
         racket/file
         "config.rkt"
         "data.rkt"
         "build.rkt"
         "render.rkt"
         "write.rkt")

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
  (define site (new render-site% (posts posts)))
  (define index (send site get-index))

  (parameterize ((the-site site))

    ;; Delete existing dest-dir
    ;; (delete-directory/files (get-dest-dir))

    ;; Copy static resources
    (for ([src-dir (in-list (reverse (pre-static-src-dirs)))])
      (copy-static-files src-dir (get-dest-dir)))
    (copy-static-files (get-static-src-dir) (get-dest-dir))

    ;; Write posts
    (for ([post (in-list posts)] #:when (send post render?))
      (write-post post))

    ;; Write main index and feed
    (write-index index)
    (write-atom-feed index)

    ;; Write tag indexes and feeds
    (for ([tag (in-list (send site get-tags))])
      (define tag-index (send site get-tag-index tag))
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
                   (error 'jeremiah
                          "duplicate post name\n  path: ~e\n  previous path: ~e"
                          (postsrc-path src) (postsrc-path prev-src)))]
             [else (hash-set! seen (postsrc-name src) src)])))))

;; copy-static-files : Path Path -> Void
(define (copy-static-files src-dir dest-dir)
  (when (directory-exists? src-dir)
    (parameterize ((current-directory src-dir))
      (for ([file (in-list (find-files file-exists?))]) ;; FIXME: copy links?
        (define src-file (build-path src-dir file))
        (define dest-file (build-path dest-dir file))
        (make-parent-directory* dest-file)
        (when (file-exists? dest-file) (delete-file dest-file))
        (copy-file src-file dest-file)))))

;; read-post : PostSrc -> Post
(define (read-post src #:who [who 'read-post])
  (define-values (meta blurb more?) (read-post-info src #:who who))
  (new render-post% (src src) (meta meta) (blurb blurb) (more? more?)))


;; ----------------------------------------

(define-syntax-parameter delay-exception (syntax-rules ()))

(define-syntax-rule (with-delay-exceptions . body)
  (call/call/delay-exception
   (lambda (c/de)
     (syntax-parameterize ((delay-exception
                            (syntax-rules ()
                              [(_ . inner)
                               (c/de (lambda () . inner))])))
       . body))))

(define (call/call/delay-exception proc)
  (define exns null) ;; (Listof Exn), mutated
  (define (call/delay-exception inner-proc)
    (with-handlers ([exn:fail? (lambda (e) (set! exns (cons e exns)))])
      (inner-proc)))
  (proc call/delay-exception)
  (when (pair? exns)
    (for ([e (in-list (reverse exns))])
      ((error-display-handler) (exn-message e) e))
    (error 'jeremiah "~s errors, exiting" (length exns))))

;; ----------------------------------------

(require racket/lazy-require)
(lazy-require ["private/preview.rkt" (preview)])
(provide preview)
