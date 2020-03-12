#lang racket/base
(require racket/match
         racket/class
         racket/path
         racket/file
         net/url
         "config.rkt"
         "util.rkt"
         "private/post.rkt")

;; ----------------------------------------

;; FIXME: option to ignore cache timestamps? (or maybe just for scribble?)
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

  ;; Read metadata from cache, build index
  (define infos
    (for/list ([src (in-list srcs)])
      (read-post-info src)))

  (define index (build-index infos))

  infos
  #|
  ;; Copy post files from cache to dest-dir
  ;;(delete-directory/files (get-dest-dir))
  (copy-directory/files (get-copy-src-dir) (get-dest-dir) #:preserve-links? #t)
  (for ([src (in-list srcs)])
    (copy-directory/files (postsrc-cachedir (postinfo-src info))
                          (postinfo->dest-dir info)))
  ;; write index...
  |#)

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
  (posts  ;; (Listof PostInfo)
   next   ;; Hash[PostInfo => PostInfo]
   prev   ;; Hash[PostInfo => PostInfo]
   ) #:prefab)

;; build-index : (Listof Postinfo) -> PostIndex
(define (build-index infos0)
  (define infos (filter (lambda (info) (send info index?)) infos0))
  (define sorted-infos (sort infos string<? #:key (lambda (info) (send info sortkey))))
  (define next (make-hasheq))
  (define prev (make-hasheq))
  (for ([info (in-list sorted-infos)]
        [next-info (in-list (if (pair? sorted-infos) (cdr sorted-infos) '()))])
    (hash-set! next info next-info)
    (hash-set! prev next-info info))
  (postindex sorted-infos next prev))
