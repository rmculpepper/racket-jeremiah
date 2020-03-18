#lang racket/base
(require racket/stxparam
         racket/class
         racket/file
         "config.rkt"
         "data.rkt"
         "build.rkt"
         "render.rkt"
         "write.rkt")
(provide go)

;; go : -> Void
(define (go #:include-draft? [include-draft? #f]
            #:force-rebuild? [force-rebuild? #f])
  (log-jeremiah-info "The post source dir is ~e" (get-post-src-dir))
  (log-jeremiah-info "The post cache dir is ~e" (get-post-cache-dir))
  (log-jeremiah-info "The output dir is ~e" (get-dest-dir))

  ;; Find all post sources
  ;; FIXME: generalize to multiple dirs?
  (log-jeremiah-info "Finding post sources")
  (define post-src-paths (find-files post-src-path? (get-post-src-dir)))
  (for ([path (in-list post-src-paths)])
    (log-jeremiah-debug "found post source: ~e" path))
  (log-jeremiah-info "Found ~s post sources" (length post-src-paths))

  (define srcs (map path->postsrc post-src-paths))
  (check-duplicate-post-src srcs)

  ;; First, build to cache
  (log-jeremiah-info "Checking cache")
  (let ([builders
         (filter values
                 (with-delay-exceptions
                   (for/list ([src (in-list srcs)])
                     (delay-exception
                      (check-cache/get-builder src #:force-rebuild? force-rebuild?)))))])
    (log-jeremiah-info "Building ~s posts" (length builders))
    (with-delay-exceptions
      (for ([builder (in-list builders)])
        (delay-exception (builder))))
    (log-jeremiah-info "Finished building posts"))

  ;; Read metadata from cache, build index
  (define site
    (let ([posts (map read-post srcs)])
      (log-jeremiah-info "Read ~s built posts" (length posts))
      (new render-site% (posts posts) (include-draft? include-draft?))))
  (define index (send site get-index))
  (log-jeremiah-info "Main index has ~s posts" (length (send index get-posts)))

  (parameterize ((the-site site))

    ;; Delete existing dest-dir
    ;; (delete-directory/files (get-dest-dir))

    ;; Copy static resources
    (log-jeremiah-info "Copying static resources")
    (for ([src-dir (in-list (reverse (pre-static-src-dirs)))])
      (copy-static-files src-dir (get-dest-dir)))
    (copy-static-files (get-static-src-dir) (get-dest-dir))
    (log-jeremiah-info "Finished copying static resources")

    ;; Write posts
    (log-jeremiah-info "Writing posts")
    (for ([post (in-list (send index get-posts))])
      (write-post post))
    (log-jeremiah-info "Finished writing posts")

    ;; Write indexes and feeds
    (log-jeremiah-info "Writing indexes and feeds")
    (begin
      ;; Write main index and feed
      (log-jeremiah-debug "Writing main index and feed")
      (write-index index)
      (write-atom-feed index)
      ;; Write tag indexes and feeds
      (for ([tag (in-list (send site get-tags))])
        (log-jeremiah-debug "Writing index and feed for tag: ~e" tag)
        (define tag-index (send site get-tag-index tag))
        (write-index tag-index)
        (write-atom-feed tag-index)))

    ;; Done
    (log-jeremiah-info "Done")
    (void)))

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
  (begin0 (proc call/delay-exception)
    (when (pair? exns)
      (for ([e (in-list (reverse exns))])
        ((error-display-handler) (exn-message e) e))
      (error 'jeremiah "~s errors, exiting" (length exns)))))
