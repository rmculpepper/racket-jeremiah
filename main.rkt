#lang racket/base
(require racket/match
         racket/path
         racket/file
         "config.rkt"
         "util.rkt"
         "private/post.rkt")

;; ----

(define (post-src-path? p)
  (post-file-name? (path->string (file-name-from-path p))))

(struct postsrc
  (path ;; AbsPath
   name ;; String
   cachedir ;; AbsPath -- may not exist, initially
   ) #:prefab)

;; path->postsrc : Path -> postsrc
(define (path->postsrc p)
  (define name (path->string (file-name-from-path p)))
  (postsrc p name (build-path (get-post-cache-dir) name)))

(struct postinfo
  (src  ;; postsrc
   meta ;; ???
   ) #:prefab)

;; ----------------------------------------

;; FIXME: option to avoid updating cache?
;; FIXME: option to only update cache, avoid writing to dest-dir?

;; go : -> Void
(define (go)

  ;; Find all post sources
  ;; FIXME: generalize to multiple dirs?
  (define post-src-paths (find-files post-src-path? (get-post-src-dir)))

  (define srcs (map path->postsrc post-src-paths))
  (check-duplicate-post-src srcs)

  ;; First, build to cache
  (with-delay-exceptions
    (for ([src (in-list srcs)])
      (delay-exception (build/cache-post src))))

  #|
  ;; Read metadata from cache, build index
  (define infos
    (for/list ([src (in-list srcs)])
      (read-post-src-info src)))
  (define index (build-index infos))

  ;; Copy post files from cache to dest-dir
  ;;(delete-directory/files (get-dest-dir))
  (copy-directory/files (get-copy-src-dir) (get-dest-dir) #:preserve-links? #t)
  (for ([src (in-list srcs)])
    (copy-directory/files (postsrc-cachedir (postinfo-src info))
                          (postinfo->dest-dir info)))
  ;; write index...
  |#

  (void))

(define (build/cache-post src)
  (match-define (postsrc path name cachedir) src)
  ;; FIXME: unless cachedir exists & has timestamp > src path timestamp
  (make-directory* cachedir)
  (build-post path cachedir))

;; ----------------------------------------

(define (find-relative-files ok? dir)
  (parameterize ((current-directory dir))
    (find-files ok?)))

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
