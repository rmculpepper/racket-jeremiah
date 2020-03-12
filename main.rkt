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
  (src   ;; postsrc
   meta  ;; MetaHash
   blurb ;; XExprs
   more? ;; Boolean
   ) #:prefab)

(define (postinfo-index? info)
  (member (metadata-display (postinfo-meta info)) '("index")))
(define (postinfo-render? info)
  (member (metadata-display (postinfo-meta info)) '("index" "draft")))

(define (postinfo-sortkey info)
  (define meta (postinfo-meta info))
  (define date (or (metadata-date meta)
                   (error 'postinfo-sortkey "no date: ~e" info)))
  (string-append date (metadata-auxsort meta)))

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

  #|
  ;; Copy post files from cache to dest-dir
  ;;(delete-directory/files (get-dest-dir))
  (copy-directory/files (get-copy-src-dir) (get-dest-dir) #:preserve-links? #t)
  (for ([src (in-list srcs)])
    (copy-directory/files (postsrc-cachedir (postinfo-src info))
                          (postinfo->dest-dir info)))
  ;; write index...
  |#

  (void))

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

;; build/cache-post : postsrc -> Void
(define (build/cache-post src)
  (match-define (postsrc path name cachedir) src)
  (define src-timestamp (file-or-directory-modify-seconds path))
  (define cache-timestamp (get-cache-timestamp cachedir))
  (when #f
    (eprintf "cache ~s ~a src ~s\n"
             cache-timestamp
             (if (< cache-timestamp src-timestamp) '< '>)
             src-timestamp))
  ;; FIXME: what about scribble file that depends on modified lib?
  (unless (> cache-timestamp src-timestamp)
    (make-directory* cachedir)
    (build-post path cachedir)))

(define (get-cache-timestamp cachedir)
  (with-handlers ([exn:fail:filesystem? (lambda (e) -inf.0)])
    (with-input-from-file (build-path cachedir "_cache.rktd") read)))

;; read-post-info : postsrc -> postinfo
(define (read-post-info src)
  (match-define (postsrc path name cachedir) src)
  (define-values (_ts meta-h blurb more?) (read-post-cache cachedir))
  (postinfo src meta-h blurb more?))

;; ----------------------------------------

(struct postindex
  (posts  ;; (Listof postinfo)
   next   ;; Hash[postinfo => postinfo]
   prev   ;; Hash[postinfo => postinfo]
   ) #:prefab)

;; build-index : (Listof postinfo) -> postindex
(define (build-index infos0)
  (define infos (filter (lambda (info) (postinfo-index? info)) infos0))
  (define sorted-infos (sort infos string<? #:key postinfo-sortkey))
  (define next (make-hasheq))
  (define prev (make-hasheq))
  (for ([info (in-list sorted-infos)]
        [next-info (in-list (if (pair? sorted-infos) (cdr sorted-infos) '()))])
    (hash-set! next info next-info)
    (hash-set! prev next-info info))
  (postindex sorted-infos next prev))
