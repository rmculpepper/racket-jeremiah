#lang racket/base
(require "util.rkt"
         "post.rkt")

;; ----

(define (post-src-path? p)
  (post-file-name? (path->string (file-name-from-path p))))

(struct postsrc
  (path ;; AbsPath
   name ;; String
   cachedir ;; AbsPath -- may not exist, initially
   meta ;; #f or ???
   ) #:prefab)

;; path->postsrc : Path -> postsrc
(define (path->postsrc p)
  (define name (path->string (file-name-from-path p)))
  (postsrc path name (build-dir (get-post-cache-dir) name)))

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

  (define srcs (map path->postsrc))
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

;; ----------------------------------------

(define (find-relative-files ok? dir)
  (parameterize ((current-directory dir))
    (find-files ok?)))
    
;; check-duplicate-post-src : (Listof postsrc) -> Void
(define (check-duplicate-post-src srcs)
  (define seen (make-hash)) ;; Hash[String => postsrc]
  (with-delay-exception
    (for ([src (in-list srcs)])
      (delay-exception
       (cond [(hash-ref seen (postsrc-name src))
              => (lambda (prev-src)
                   (j-error "duplicate post name\n  path: ~e\n  previous path: ~e"
                            (postsrc-path src) (postsrc-path prev-src)))]
             [else (hash-set! seen (postsrc-name src) src)])))))
