#lang racket/base
(require (for-syntax racket/base racket/syntax)
         racket/match
         racket/class
         racket/list
         racket/promise
         racket/path
         racket/date
         racket/hash
         racket/string
         net/url
         markdown
         "util.rkt"
         "xexpr.rkt"
         (prefix-in config: "config.rkt"))
(provide (all-defined-out))

;; An IndexName (IName) is one of
;; - String -- a tag name
;; - 'main  -- the main index
;; - 'draft -- the special draft index

;; ============================================================
;; Config

(define-syntax (define-init-field+getter stx)
  (syntax-case stx ()
    [(_ (name ...))
     (with-syntax ([(tmp ...) (generate-temporaries #'(name ...))]
                   [((get-name config:name) ...)
                    (for/list ([name (in-list (syntax->list #'(name ...)))])
                      (list (format-id name "get-~a" name)
                            (format-id name "config:~a" name)))])
       #'(begin (define tmp (config:name)) ...
                (define/public (get-name) tmp) ...))]))

(define config%
  (class object%

    (define-init-field+getter
      (site-title
       site-author
       site-max-feed-items
       site-posts-per-page
       base-dir
       pre-static-src-dirs
       base-url
       tag-uri-entity
       tag-uri-prefix
       permalink-pattern
       draft-pattern
       page-renderer
       post-renderer
       index-entry-renderer
       extra-html))

    (super-new)

    ;; ----------------------------------------
    ;; Paths

    (define/private (-base-rel-path . relpaths)
      (apply build-path (get-base-dir) relpaths))

    (define/public (get-static-src-dir) (-base-rel-path "static"))
    (define/public (get-post-src-dir)   (-base-rel-path "posts"))
    (define/public (get-cache-dir)      (-base-rel-path "cache"))
    (define/public (get-post-cache-dir) (-base-rel-path "cache" "posts"))
    (define/public (get-dest-dir)       (-base-rel-path "build"))
    (define/public (get-feeds-dest-dir) (-base-rel-path "build" "feeds"))
    (define/public (get-tags-dest-dir)  (-base-rel-path "build" "tags"))

    ;; ----------------------------------------
    ;; URLs and Links

    (define/private (-base-rel-url . relpaths)
      (apply build-url (get-base-url) relpaths))

    (define/public (get-tags-url)  (-base-rel-url "tags"))
    (define/public (get-feeds-url) (-base-rel-url "feeds"))

    (define/public (get-link) (url->string (local-url (get-base-url))))
    (define/public (get-full-link) (url->string (get-base-url)))

    ;; ----------------------------------------
    ;; URLs and Paths for Indexes

    (define/public (get-index-main-url iname)
      (match iname
        [(? string? tag)
         (-base-rel-url "tags" (format "~a.html" (get-index-dest-file-name-base tag)))]
        ['main (get-base-url)] ;; implicit /index.html
        ['draft (-base-rel-url "tags" "draft.html")]))

    (define/public (get-index-page-url iname n)
      (define file-name (get-index-page-file-name iname n))
      (match iname
        [(? string? tag) (build-url (get-tags-url) file-name)]
        [(or 'main 'draft) (build-url (get-base-url) file-name)]))

    (define/public (get-index-page-dest-file iname n)
      (define file-name (get-index-page-file-name iname n))
      (build-path (get-index-dest-dir iname) file-name))

    (define/public (get-index-page-file-name iname n)
      (file/page (get-index-dest-file-name-base iname) n))

    (define/public (get-index-dest-file-name-base iname)
      (match iname
        [(? string? tag) (slug tag)]
        ['main "index"]
        ['draft "draft"]))

    (define/public (get-index-dest-dir iname)
      (match iname
        [(? string? tag) (get-tags-dest-dir)]
        [(or 'main 'draft) (get-dest-dir)]))

    (define/public (get-index-atom-feed-url iname)
      (-base-rel-url "feeds" (get-index-atom-feed-file-name iname)))

    (define/public (get-index-atom-feed-file-name iname)
      (format "~a.atom.xml"
              (match iname
                [(? string? tag) (slug tag)]
                ['main "all"]
                ['draft (error 'get-index-atom-feed-file-name "no atom feed for drafts")])))

    (define/public (get-index-atom-id iname)
      (define file-name (get-index-atom-feed-file-name iname))
      (build-tag-uri (format "feeds/~a" file-name)))

    (define/public (get-index-main-full-link iname)
      (url->string (get-index-main-url iname)))
    (define/public (get-index-main-link iname)
      (url->string (local-url (get-index-main-url iname))))
    (define/public (get-index-atom-feed-full-link iname)
      (url->string (get-index-atom-feed-url iname)))
    (define/public (get-index-atom-feed-link iname)
      (url->string (local-url (get-index-atom-feed-url iname))))

    ;; ----------------------------------------
    ;; Tag URI

    ;; build-tag-uri : String -> String
    (define/public (build-tag-uri suffix)
      (define entity (get-tag-uri-entity))
      (define prefix (get-tag-uri-prefix))
      (format "tag:~a:~a~a~a" entity prefix (if (equal? suffix "") "" ":") suffix))

    ;; ----------------------------------------
    ;; Post Sources

    ;; path->postsrc : Path -> postsrc
    (define/public (path->postsrc p)
      (define name (path->string (file-name-from-path p)))
      (postsrc p name (build-path (get-post-cache-dir) name)))
    ))

(define (file/page file-name-base page-num)
  (cond [(zero? page-num) (format "~a.html" file-name-base)]
        [else (format "~a-~a.html" file-name-base (add1 page-num))]))

(define has-config%
  (class object%
    (init-field config)
    (super-new)
    (define/public (get-config) config)))

;; ============================================================
;; Site

(define site%
  (class has-config%
    (inherit-field config)
    (init-field posts
                [the-index% index%])
    (super-new)

    (define iname=>index
      (let ([iname=>posts (make-hash)])
        (hash-set! iname=>posts 'main null)
        (hash-set! iname=>posts 'draft null)
        (for ([post (in-list posts)])
          (case (send post get-display)
            [("index")
             (hash-cons! iname=>posts 'main post)
             (for ([tag (in-list (send post get-tags))])
               (hash-cons! iname=>posts tag post))]
            [("draft") (hash-cons! iname=>posts 'draft post)]))
        (for ([(tag posts) (in-hash iname=>posts)] #:when (member tag reserved-tags))
          (log-jeremiah-error "reserved tag ~e in posts: ~e" tag
                              (for/list ([p (in-list posts)]) (send p get-source))))
        (for/hash ([(iname posts) (in-hash iname=>posts)]
                   #:when (not (member iname reserved-tags)))
          (values iname (build-index iname posts)))))

    (define tags (sort (filter string? (hash-keys iname=>index)) string-ci<?))

    (let ([h (make-hash)])
      (for ([t (in-list tags)])
        (hash-cons! h (string-foldcase t) t))
      (for ([(k ts) (in-hash h)] #:when (> (length ts) 1))
        (log-jeremiah-error "tag collision: ~s" ts)))

    (define/public (get-posts) (send (get-index) get-posts))
    (define/public (get-tags) tags)
    (define/public (get-index) (hash-ref iname=>index 'main))
    (define/public (get-tag-index tag) (hash-ref iname=>index tag #f))

    (define/public (get-prev-post post) (send (get-index) get-prev post))
    (define/public (get-next-post post) (send (get-index) get-next post))

    ;; build-index : IndexName (Listof Post) -> Index
    (define/private (build-index iname posts)
      (define sorted-posts
        (sort posts string>? #:key (lambda (post) (send post sortkey))))
      (new the-index% (config config) (iname iname) (posts sorted-posts)))

    ;; ----------------------------------------
    ;; Utils for site.rkt and templates

    (define/public (get-title) (send config get-site-title))
    (define/public (get-author) (send config get-site-author))
    (define/public (get-index-main-link tag) (send config get-index-main-link tag))

    (define/public (link . paths)
      (apply build-link #:local? #t (send config get-base-url) paths))
    (define/public (full-link . paths)
      (apply build-link #:local? #f (send config get-base-url) paths))
    ))

(define (hash-cons! h k v)
  (hash-update! h k (lambda (vs) (cons v vs)) null))


;; ============================================================
;; Indexes and Index Pages

;; Index = instance of index%
(define index%
  (class has-config%
    (inherit-field config)
    (init-field iname   ;; IndexName
                posts)  ;; (Listof Post), sorted most recent first
    (super-new)

    (define prev-h (make-hasheq)) ;; Hasheq[Post => Post/#f]
    (define next-h (make-hasheq)) ;; Hasheq[Post => Post/#f]
    (for ([post (in-list posts)]
          [next-post (in-list (if (pair? posts) (cdr posts) '()))])
      (hash-set! next-h post next-post)
      (hash-set! prev-h next-post post))

    (define/public (get-title)
      (match iname
        [(? string? tag) (format "Posts tagged '~a'" tag)]
        ['main (send config get-site-title)]
        ['draft "Draft posts"]))
    (define/public (get-tag) (and (string? iname) iname)) ;; FIXME?
    (define/public (get-posts) posts)
    (define/public (get-prev p) (hash-ref prev-h p #f))
    (define/public (get-next p) (hash-ref next-h p #f))

    (define/public (get-updated-8601)
      (and (pair? posts) (send (car posts) get-date-8601)))

    (define/public (get-atom-id)
      (send config get-index-atom-id iname))

    ;; ----------------------------------------
    ;; Paths and URLs

    (define/public (get-feed-file-name)
      (send config get-index-atom-feed-file-name iname))
    (define/public (get-feed-dest-file)
      (build-path (send config get-feeds-dest-dir) (get-feed-file-name)))
    (define/public (get-feed-url) (send config get-index-atom-feed-url iname))
    (define/public (get-feed-link) (url->string (local-url (get-feed-url))))

    (define/public (get-index-main-url) ;; no tag => base url (implicit /index.html)
      (send config get-index-main-url iname))
    (define/public (get-index-page-url [n 0])
      (send config get-index-page-url iname n))
    (define/public (get-index-page-dest-file n)
      (send config get-index-page-dest-file iname n))
    ))

;; IndexPage = instance of index-page%
(define index-page%
  (class has-config%
    (inherit-field config)
    (init-field index           ;; Index
                posts           ;; (Listof Post)
                page-num        ;; Nat
                num-pages)      ;; Nat
    (super-new (config (send index get-config)))

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

    (define/public (get-dest-file)
      (send index get-index-page-dest-file page-num))

    (define/public (get-url [n page-num])
      (send index get-index-page-url n))
    (define/public (get-link [n page-num])
      (build-link #:local? #t (get-url n)))
    (define/public (get-full-link [n page-num])
      (build-link #:local? #f (get-url n)))

    (define/public (get-page-url) (get-url))
    (define/public (get-page-link) (get-link))

    (define/public (get-feed-link)
      (send index get-feed-link))
    ))


;; ============================================================
;; Posts

(struct postsrc
  (path ;; AbsPath
   name ;; String
   cachedir ;; AbsPath -- may not exist, initially
   ) #:prefab)

;; Post = instance of post%
(define post%
  (class has-config%
    (inherit-field config)
    (init-field src meta blurb more?)
    (super-new)

    (define/public (get-src) src)
    (define/public (get-meta) meta)
    (define/public (get-blurb-xexprs) blurb)
    (define/public (get-more?) more?)

    (define/public (get-source) (postsrc-path src))
    (define/public (get-source-type)
      (match (path->string (postsrc-path src))
        [(regexp "[.]scrbl$") 'scribble]
        [(regexp "[.]md[t]?$") 'markdown]
        [_ #f]))

    (define/public (get-title) (metadata-title meta))
    (define/public (get-title-xexpr) (metadata-title-xexpr meta))
    (define/public (get-authors)
      (define authors (metadata-authors meta))
      (if (pair? authors) authors (list (send config get-site-author))))
    (define/public (get-tags) (metadata-tags meta))

    (define/public (get-title-slug) (metadata-slug meta))
    (define/public (get-atom-id)
      (or (metadata-atom-id meta) (send config build-tag-uri (get-rel-www))))

    (define/public (get-date) ;; short date: YYYY-MM-DD
      (match (metadata-date meta)
        [(pregexp #px"^(\\d{4}-\\d{2}-\\d{2})" (list _ d)) d]
        [#f (error 'get-date "date not available\n  post: ~a" (get-source))]))
    (define/public (get-year) ;; String(4) or #f
      (match (metadata-date meta)
        [(pregexp #px"^(\\d{4})" (list _ year)) year]
        [else #f]))
    (define/public (get-date-object)
      (cond [(metadata-date meta) => string->date/8601]
            [else (error 'get-date-obj "date not available\n  post: ~a" (get-source))]))
    (define/public (get-date-8601)
      (parameterize ((date-display-format 'iso-8601))
        (format "~aZ" (date->string (get-date-object) #t))))

    (define/public (get-display) (metadata-display meta))

    (define/public (sortkey) ;; -> String
      (or (metadata-date meta)
          (error 'post%::sortkey "no date: ~e" (get-source))))

    (define/public (get-body-xexprs)
      (with-input-from-file (build-path (postsrc-cachedir src) "_index.rktd")
        read))

    ;; ----------------------------------------
    ;; Paths and URLs

    (define/public (get-cachedir) (postsrc-cachedir src))

    (define/public (get-rel-www) (force rel-www-p))
    (define rel-www-p (delay (-get-rel-www)))

    (define/public (-get-rel-www) ;; -> String
      ;; URL path as string, not including base-url
      ;; should not start or end with "/" -- FIXME: enforce on pattern?
      (define title-slug (get-title-slug))
      (define-values (pattern year month day)
        (match (metadata-date meta)
          [(pregexp #px"^(\\d{4})-(\\d{2})-(\\d{2})" (list _ year month day))
           (values (send config get-permalink-pattern) year month day)]
          [#f
           (define (nope . _) (error 'post-meta->rel-www "date component not available"))
           (values (send config get-draft-pattern) nope nope nope)]))
      (regexp-replaces pattern
                       `([#rx"{year}" ,year]
                         [#rx"{month}" ,month]
                         [#rx"{day}" ,day]
                         [#rx"{title}" ,title-slug]
                         #;[#rx"{filename}",filename])))

    (define/public (get-page-url) (build-url (get-url) "index.html"))
    (define/public (get-page-link) (url->string (local-url (get-page-url))))

    (define/public (get-dest-dir) (build-path (send config get-dest-dir) (get-rel-www)))
    (define/public (get-url) (build-url (send config get-base-url) (get-rel-www)))
    (define/public (get-full-link) (url->string (get-url)))
    (define/public (get-link) (url->string (local-url (get-url))))
    (define/public (get-feed-link) (send config get-index-atom-feed-link 'main))
    ))


;; ============================================================
;; Metadata

;; Metadata is a hasheq with the following possible keys:
;; - 'title : String
;; - 'slug : String
;; - 'title-xexpr : XExpr -- text or span element containing rendered title
;;       -- NOTE: currently disabled, because it looks weird (maybe just CSS issue?)
;; - 'date : String -- should have form "YYYY-MM-DD"
;; - 'authors : String (comma-separated)
;; - 'tags : String (comma-separated)
;; - 'display : "index" | "draft" | "none"
;; - 'atomid : String, must be URI (or IRI?) -- use for porting

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
(define (metadata-slug h)
  (or (hash-ref h 'slug #f)
      (slug (metadata-title h))))
(define (metadata-date h)
  (match (hash-ref h 'date #f)
    [(pregexp "^\\d{4}-\\d{2}-\\d{2}" (list d)) d]
    [(? string? d) (error 'metadata-date "bad date: ~e" d)]
    [#f #f]))
(define (metadata-authors h)
  (string-split (hash-ref h 'authors "") #rx"[ ]*,[ ]*" #:trim? #t))
(define (metadata-tags h)
  (string-split (hash-ref h 'tags "") #rx"[ ]*,[ ]*" #:trim? #t))
(define (metadata-display h)
  (define v (hash-ref h 'display "index"))
  (cond [(member v '("index" "draft" "none")) v]
        [else (error 'metadata-display "bad display: ~e" v)]))
(define (metadata-atom-id h) ;; FIXME: validate?
  (hash-ref h 'atomid #f))

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
    [(pregexp #px"^(\\d{4})-(\\d{2})-(\\d{2})$" (list _ year month day))
     ;; A date w/o time is interpreted as noon UTC, so that the UTC
     ;; date and local date coincide (almost) everywhere.
     (define dsec (find-seconds 0 0 12 (num day) (num month) (num year) #f))
     (seconds->date dsec #f)]
    [(pregexp #px"^(\\d{4})-(\\d{2})-(\\d{2})(?:[ T](\\d{2}):(\\d{2})(?:[:](\\d{2}))?(Z)?)?$"
              (list _ year month day hour minute second tz))
     (define dsec
       (find-seconds (num second) (num minute) (num hour)
                     (num day) (num month) (num year)
                     (not (equal? tz "Z"))))
     (seconds->date dsec #f)]
    [_ (error 'string->date/8601 "bad date: ~e" s)]))
