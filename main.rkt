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
;; Post Source

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

;; ============================================================
;; Post Info

;; read-post-info : postsrc -> PostInfo
(define (read-post-info src)
  (match-define (postsrc path name cachedir) src)
  (define-values (_ts meta blurb more?) (read-post-cache cachedir))
  (new postinfo% (src src) (meta meta) (blurb blurb) (more? more?)))

;; PostInfo = instance of postinfo%
(define postinfo%
  (class object%
    (init-field src meta blurb more?)
    (super-new)

    (define/public (get-src) src)
    (define/public (get-meta) meta)
    (define/public (get-blurb) blurb)
    (define/public (get-more?) more?)

    (define/public (get-title) (metadata-title meta))
    (define/public (get-author) (metadata-author meta))
    (define/public (get-date) (metadata-date meta))
    (define/public (get-tags) (metadata-tags meta))

    (define/public (get-rel-www) (post-meta->rel-www meta))
    (define/public (get-url) (combine-url/relative (get-base-url) (get-rel-www)))
    (define/public (get-enc-url) (url->string (get-url)))

    (define/public (index?)
      (member (metadata-display meta) '("index")))
    (define/public (render?)
      (member (metadata-display meta) '("index" "draft")))

    (define/public (sortkey) ;; -> String
      (define date (or (metadata-date meta)
                       (error 'postinfo-sortkey "no date: ~e" (about))))
      (string-append date (metadata-auxsort meta)))

    (define/public (about) (format "(postinfo ~e)" src))
    ))

;; post-meta->rel-www : Meta -> String
;; URL path (suffix) as string, not including base-url.
;; Should not have "/" at beginning or end. -- FIXME: enforce on pattern?
(define (post-meta->rel-www meta)
  (define title-slug (slug (metadata-title meta)))
  (define-values (pattern year month day)
    (match (metadata-date meta)
      [(pregexp #px"^(\\d{4})-(\\d{2})-(\\d{2})" (list _ year month day))
       (values (get-permalink-pattern) year month day)]
      [#f
       (define (nope . _) (error 'post-meta->rel-www "date component not available"))
       (values (get-draft-permalink-pattern) nope nope nope)]))
  (regexp-replaces pattern
                   `([#rx"{year}" ,year]
                     [#rx"{month}" ,month]
                     [#rx"{day}" ,day]
                     [#rx"{title}" ,title-slug]
                     #;[#rx"{filename}",filename])))


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
