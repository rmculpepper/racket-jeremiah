#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/path
         racket/date
         racket/hash
         racket/string
         net/url
         markdown
         "config.rkt"
         (prefix-in config: "config.rkt")
         "xexpr.rkt")
(provide (all-defined-out))

;; ============================================================
;; Site

(define site%
  (class object%
    (init-field posts
                [the-index% index%])
    (super-new)

    (define tags
      (let ([h (make-hash)])
        (for ([post (in-list posts)]
              #:when (send post index?)
              [tag (in-list (send post get-tags))]
              #:when (not (member tag reserved-tags)))
          (hash-set! h tag #t))
        (sort (hash-keys h) string-ci<?)))

    (define tag=>index ;; includes #f => main index
      (for/hash ([tag (in-list (cons #f tags))])
        (values tag (build-index tag posts))))

    (define/public (get-posts) posts)
    (define/public (get-tags) tags)
    (define/public (get-index) (hash-ref tag=>index #f)) ;; #f = main index
    (define/public (get-tag-index tag) (hash-ref tag=>index tag #f))

    (define/public (get-prev-post post) (send (get-index) get-prev post))
    (define/public (get-next-post post) (send (get-index) get-next post))

    ;; build-index : String/#f (Listof Post) -> Index
    (define/private (build-index tag posts)
      (define sorted-posts
        (sort (filter (lambda (post) (send post index? tag)) posts)
              string>?
              #:key (lambda (post) (send post sortkey))))
      (new the-index% (tag tag) (posts sorted-posts)))

    ;; ----------------------------------------
    ;; Utils for site.rkt and templates

    (define/public (link . paths) (apply build-link #:local? #t (get-base-url) paths))
    (define/public (full-link . paths) (apply build-link #:local? #f (get-base-url) paths))
    ))


;; ============================================================
;; Indexes and Index Pages

;; Index = instance of index%
(define index%
  (class object%
    (init-field tag     ;; String/#f
                posts)  ;; (Listof Post), sorted most recent first
    (super-new)

    (define prev-h (make-hasheq)) ;; Hasheq[Post => Post/#f]
    (define next-h (make-hasheq)) ;; Hasheq[Post => Post/#f]
    (for ([post (in-list posts)]
          [next-post (in-list (if (pair? posts) (cdr posts) '()))])
      (hash-set! next-h post next-post)
      (hash-set! prev-h next-post post))

    (define/public (get-title)
      (cond [tag (format "Posts tagged '~a'" tag)]
            [else (site-title)]))
    (define/public (get-tag) tag)
    (define/public (get-posts) posts)
    (define/public (get-prev p) (hash-ref prev-h p #f))
    (define/public (get-next p) (hash-ref next-h p #f))

    (define/public (get-updated-8601)
      (and (pair? posts) (send (car posts) get-date-8601)))

    ;; ----------------------------------------
    ;; Paths and URLs

    (define/public (get-feed-file-name)
      (format "~a.atom.xml" (or tag "all")))
    (define/public (get-feed-dest-file)
      (build-path (get-feeds-dest-dir) (get-feed-file-name)))
    (define/public (get-feed-url)
      (build-url (get-feeds-url) (get-feed-file-name)))
    (define/public (get-feed-link)
      (url->string (local-url (get-feed-url))))

    (define/public (get-tag-url) ;; no tag => base url (implicit /index.html)
      (if tag (config:get-tag-url tag) (get-base-url)))

    (define/public (get-tag-dest-file-name-base)
      (if tag (slug tag) "index"))
    (define/public (get-tag-dest-dir)
      (if tag (get-tags-dest-dir) (get-dest-dir)))
    ))

;; IndexPage = instance of index-page%
(define index-page%
  (class object%
    (init-field index           ;; Index
                posts           ;; (Listof Post)
                page-num        ;; Nat
                num-pages)      ;; Nat
    (super-new)

    (define/public (get-tag) (send index get-tag))

    (define/public (get-index) index)
    (define/public (get-posts) posts)
    (define/public (get-page-num) page-num)
    (define/public (get-num-pages) num-pages)
    (define/public (get-title)
      (cond [(zero? page-num) (send index get-title)]
            [else (format "~a (page ~a)" (send index get-title) (add1 page-num))]))

    ;; ----------------------------------------
    ;; Paths and URLs

    (define/public (get-dest-file-name [n page-num])
      (file/page (send index get-tag-dest-file-name-base) n))
    (define/public (get-dest-file)
      (build-path (send index get-tag-dest-dir) (get-dest-file-name)))

    (define/public (get-url [n page-num])
      (let ([tag (send index get-tag)])
        (cond [tag (build-url (get-tags-url) (get-dest-file-name n))]
              [else (build-url (get-base-url) (get-dest-file-name n))])))
    (define/public (get-link [n page-num])
      (build-link #:local? #t (get-url n)))
    (define/public (get-full-link [n page-num])
      (build-link #:local? #f (get-url n)))

    (define/public (get-page-url) (get-url))
    (define/public (get-page-link) (get-link))

    (define/public (get-feed-link)
      (send index get-feed-link))
    ))

(define (file/page file-name-base page-num)
  (cond [(zero? page-num) (format "~a.html" file-name-base)]
        [else (format "~a-~a.html" file-name-base page-num)]))


;; ============================================================
;; Posts

(struct postsrc
  (path ;; AbsPath
   name ;; String
   cachedir ;; AbsPath -- may not exist, initially
   ) #:prefab)

;; Post = instance of post%
(define post%
  (class object%
    (init-field src meta blurb more?)
    (super-new)

    (define/public (get-src) src)
    (define/public (get-meta) meta)
    (define/public (get-blurb-xexprs) blurb)
    (define/public (get-more?) more?)

    (define/public (get-source-type)
      (match (path->string (postsrc-path src))
        [(regexp "[.]scrbl$") 'scribble]
        [(regexp "[.]md[t]?$") 'markdown]
        [_ #f]))

    (define/public (get-title) (metadata-title meta))
    (define/public (get-title-xexpr) (metadata-title-xexpr meta))
    (define/public (get-authors)
      (define authors (metadata-authors meta))
      (if (pair? authors) authors (list (get-site-author))))
    (define/public (get-tags) (metadata-tags meta))

    (define/public (get-date) ;; short date: YYYY-MM-DD
      (match (metadata-date meta)
        [(pregexp #px"^(\\d{4}-\\d{2}-\\d{2})" (list _ d)) d]
        [#f (error 'get-date "date not available\n  post: ~a" (about))]))
    (define/public (get-year) ;; String(4) or #f
      (match (metadata-date meta)
        [(pregexp #px"^(\\d{4})" (list _ year)) year]
        [else #f]))
    (define/public (get-date-object)
      (cond [(metadata-date meta) => string->date/8601]
            [else (error 'get-date-obj "date not available\n  post: ~a" (about))]))
    (define/public (get-date-8601)
      (parameterize ((date-display-format 'iso-8601))
        (format "~aZ" (date->string (get-date-object) #t))))

    (define/public (index? [tag #f])
      (and (member (metadata-display meta) '("index"))
           (cond [tag (and (not (member tag reserved-tags))
                           (and (member tag (get-tags)) #t))]
                 [else #t])))
    (define/public (render?)
      (member (metadata-display meta) '("index" "draft")))

    (define/public (sortkey) ;; -> String
      (or (metadata-date meta)
          (error 'post%::sortkey "no date: ~e" (about))))

    (define/public (about) (format "(post ~e)" src))

    (define/public (get-body-xexprs)
      (with-input-from-file (build-path (postsrc-cachedir src) "_index.rktd")
        read))

    ;; ----------------------------------------
    ;; Paths and URLs

    (define/public (get-cachedir) (postsrc-cachedir src))

    (define/public (get-rel-www) ;; -> String
      ;; URL path as string, not including base-url
      ;; should not start or end with "/" -- FIXME: enforce on pattern?
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

    (define/public (get-page-url) (build-url (get-url) "index.html"))
    (define/public (get-page-link) (build-link #:local? #t (get-page-url)))

    (define/public (get-out-dir) (build-path (get-dest-dir) (get-rel-www)))
    (define/public (get-url) (build-url (get-base-url) (get-rel-www)))
    (define/public (get-full-link) (url->string (get-url)))
    (define/public (get-link) (url->string (local-url (get-url))))
    (define/public (get-feed-link) (get-atom-feed-link "all"))
    ))


;; ============================================================
;; Metadata

;; Metadata is a hasheq with the following possible keys:
;; - 'title : String
;; - 'title-xexpr : XExpr -- text or span element containing rendered title
;;       -- NOTE: currently disabled, because it looks weird (maybe just CSS issue?)
;; - 'date : String -- should have form "YYYY-MM-DD"
;; - 'authors : String (comma-separated)
;; - 'tags : String (comma-separated)
;; - 'display : "index" | "draft" | "norender"

;; Metadata is gathered from the following sources (earlier overrides later):
;; - header   -- * (except title-xexpr)
;; - content  -- authors, title, title-xexpr (only from Scribble)
;; - path     -- date, display

(define reserved-tags '("all" "index" "draft")) ;; FIXME?

(define (metadata-title h)
  (or (hash-ref h 'title #f)
      (error 'metadata-title "missing title: ~e" h)))
(define (metadata-title-xexpr h)
  (or ;;(hash-ref h 'title-xexpr #f) -- see note above
      (hash-ref h 'title #f)
      (error 'metadata-title "missing title: ~e" h)))
(define (metadata-date h)
  (match (hash-ref h 'date #f)
    [(pregexp "^\\d{4}-\\d{2}-\\d{2}" (list d)) d]
    [(? string? d) (error 'metadata-date "bad date: ~e" d)]
    [#f #f]))
(define (metadata-authors h)
  (string-split (hash-ref h 'authors "") #rx"[ *],[ ]*" #:trim? #t))
(define (metadata-tags h)
  (string-split (hash-ref h 'tags "") #rx"[ ]*,[ ]*" #:trim? #t))
(define (metadata-display h)
  (define v (hash-ref h 'display "index"))
  (cond [(member v '("index" "draft" "norender")) v]
        [else (error 'metadata-display "bad display: ~e" v)]))

;; merge-metadata : MetaHash ...+ -> MetaHash
;; Merges metadata; each key get value from *leftmost* hash that has a value for it.
(define (merge-metadata mh . mhs)
  (apply hash-union mh mhs #:combine (lambda (a b) a)))

;; path->metadata : Path -> MetaHash
(define (path->metadata path)
  (match (path->string (file-name-from-path path))
    [(regexp #rx"^(\\d{4}-\\d{2}-\\d{2})-(?:.+?)[.](?:md|mdt|scrbl|html)$"
             (list _ date-str))
     (hasheq 'date date-str)]
    [(regexp #rx"^draft-(?:.+?)[.](?:md|mdt|scrbl|html)$")
     (hasheq 'draft "yes")]
    [_ #hasheq()]))

;; string->date/8601 : String -> Date
(define (string->date/8601 s)
  (define (num s) (if s (string->number s) 0))
  (match s
    [(pregexp #px"^(\\d{4})-(\\d{2})-(\\d{2})(?:[ T](\\d{2}):(\\d{2})(?:[:](\\d{2}))?(Z)?)?$"
              (list _ year month day hour minute second tz))
     (define dsec
       (find-seconds (num second) (num minute) (num hour)
                     (num day) (num month) (num year)
                     (not (equal? tz "Z"))))
     (seconds->date dsec #f)]
    [_ (error 'string->date/8601 "bad date: ~e" s)]))
