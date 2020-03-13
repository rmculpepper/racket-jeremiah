#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/string
         racket/hash
         racket/sequence
         racket/port
         racket/path
         racket/file
         net/url
         markdown
         "../config.rkt"
         "template.rkt"
         "html.rkt"
         "xexpr-map.rkt"
         "xexpr2text.rkt")
(provide (all-defined-out))

;; TODO IDEAS
;; - allow path convention: "YYYY/MM-DD-auxtitle.ext" -> YYYY-MM-DD
;; - allow rendering to either <destpath>/index.html or <destpath>.html
;; - allow path convention: "post-..." with date set in header?

;; ============================================================
;; Post Source

(define (post-src-path? p)
  (post-file-name? (path->string (file-name-from-path p))))

(define (post-file-name? str)
  (or (dated-post-file-name? str)
      (draft-post-file-name? str)))

(define (dated-post-file-name? str)
  (regexp-match? #px"^\\d{4}-\\d{2}-\\d{2}-(?:.+?)[.](?:md|mdt|scrbl|html)$" str))

(define (draft-post-file-name? str)
  (regexp-match? #px"^draft-(?:.+?)[.](?:md|mdt|scrbl|html)$" str))

(struct postsrc
  (path ;; AbsPath
   name ;; String
   cachedir ;; AbsPath -- may not exist, initially
   ) #:prefab)

;; path->postsrc : Path -> postsrc
(define (path->postsrc p)
  (define name (path->string (file-name-from-path p)))
  (postsrc p name (build-path (get-post-cache-dir) name)))


;; ============================================================
;; Post Cache

;; Building a post writes the following to its cachedir:
;; - _cache.rktd - timestamp, meta, blurb-xexprs, more?
;; - _index.rktd - xexprs of body (need to delay, because of prev/next links?)
;; - _<other> - other non-public files starting with _
;; - aux files linked from body

;; build/cache-post : postsrc -> Void
(define (build/cache-post src)
  (match-define (postsrc path name cachedir) src)
  (define src-timestamp (file-or-directory-modify-seconds path))
  (define cache-timestamp (read-cache-timestamp cachedir))
  (when #f
    (eprintf "cache ~s ~a src ~s\n"
             cache-timestamp
             (if (< cache-timestamp src-timestamp) '< '>)
             src-timestamp))
  ;; FIXME: what about scribble file that depends on modified lib?
  (unless (> cache-timestamp src-timestamp)
    (make-directory* cachedir)
    (build-post path cachedir)))

;; read-post-info : postsrc -> PostInfo
;; Reads info about a built post from its cache.
(define (read-post-info src #:who [who 'read-post-info])
  (match-define (postsrc path name cachedir) src)
  (define-values (_ts meta blurb more?)
    (with-input-from-file (build-path cachedir "_cache.rktd")
      (lambda ()
        (define timestamp (read))
        (define meta-h (read))
        (define blurb (read))
        (define more? (read))
        (unless (exact-nonnegative-integer? timestamp)
          (error who "bad timestamp: ~e" timestamp))
        (unless (hash? meta-h)
          (error who "bad metadata: ~e" meta-h))
        (unless (list? blurb)
          (error who "bad blurb: ~e" blurb))
        (unless (boolean? more?)
          (error who "bad more?: ~e" more?))
        (values timestamp meta-h blurb more?))))
  (new postinfo% (src src) (meta meta) (blurb blurb) (more? more?)))

;; read-cache-timestamp : Path -> (U Nat -inf.0)
(define (read-cache-timestamp cachedir)
  (with-handlers ([exn:fail:filesystem? (lambda (e) -inf.0)])
    (with-input-from-file (build-path cachedir "_cache.rktd") read)))


;; ============================================================
;; Metadata

;; PostMetadata is a hasheq with the following possible keys:
;; - 'title : String
;; - 'date : String -- should have form "YYYY-MM-DD"
;; - 'auxsort : String -- extra stuff (eg time of day) to control sorting
;; - 'author : String
;; - 'tags : String (comma-separated)
;; - 'display : "index" | "draft" | "norender"

;; Metadata is gathered from the following sources (earlier overrides later):
;; - header   -- *
;; - content  -- author, title (only from Scribble)
;; - path     -- date, display

(define (check-metadata h)
  (void (metadata-title h))
  (void (metadata-date h))
  (void (metadata-display h)))

(define (metadata-title h)
  (or (hash-ref h 'title #f)
      (error 'metadata-title "missing title: ~e" h)))
(define (metadata-date h)
  (match (hash-ref h 'date #f)
    [(pregexp "^\\d{4}-\\d{2}-\\d{2}$" (list d)) d]
    [(? string? d) (error 'metadata-date "bad date: ~e" d)]
    [#f #f]))
(define (metadata-auxsort h)
  (hash-ref h 'auxsort ""))
(define (metadata-author h)
  (hash-ref h 'author ""))
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

;; ------------------------------------------------------------
;; Metadata Header

;; The header of a Scribble post consists of the prefix of the file
;; consisting of lines of the form
;;
;;   [;]*[ ]*([a-zA-Z]+)[ ]*[:][ ]*(.*)
;;
;; that is, comments followed by a property name (alpha characters
;; only), followed by a colon, followed by the value.

;; The header of a Markdown post consists of the prefix of the file
;; consisting of lines of the form
;;
;;   [ ]{2,}([a-zA-Z]+)[ ]*[:][ ]*(.*)
;;
;; that is, lines with 2 or more spaces of indentation, followed by a
;; property name, then a colon, then the value.

;; MetaHash = Hasheq[Symbol => String]
;; Keys are downcased and converted to symbols, but values not parsed.

;; read-metadata-header : (U 'spaces 'comment) InputPort -> MetaHash
;; Reads the header from the input port and returns a hash representing the
;; metadata. The input port is left open at the position after the header.
(define (read-metadata-header mode [in (current-input-port)])
  (define meta-rx
    (case mode
      [(spaces) #px"^[ ]{2,}([a-zA-Z]+)[ ]*:[ ]*(.*?)[ ]*$"]
      [(comment) #rx"^[ ]*[;]+[ ]*([a-zA-Z]+)[ ]*:[ ]*(.*?)[ ]*$"]))
  ;; parse-meta-line : String -> (U (List String String) #f)
  (define (parse-meta-line line)
    (match (regexp-match meta-rx line)
      [(list _ k v) (list (string->symbol (string-downcase k)) v)]
      [_ #f]))
  (define (in-parsed-meta-lines in)
    (stop-before (sequence-map parse-meta-line (in-lines in)) not))
  ;; The following code is a bit magic. It relies on the way reading from
  ;; (peeking-input-port in) interacts with reading from in directly.
  (for/fold ([h (hash)]) ([kv (in-parsed-meta-lines (peeking-input-port in))])
    (void (read-line in)) ;; consume the peeked line
    (match kv [(list k v) (hash-set h k v)])))


;; ============================================================
;; Building Posts

;; build-post : Path -> Void
;; PRE: path refers to file, is simplified, has at least one directory part
;; PRE: cachedir exists (might not be empty)
(define (build-post path cachedir)
  (define timestamp (current-seconds))
  (define path-meta-h (path->metadata path))
  (define-values (dir name d?) (split-path path)) ;; PRE => name, dir are paths, d? is #f
  (define-values (body header-meta-h body-meta-h)
    (match (path->string name)
      [(regexp "[.]scrbl$")
       (build-scribble-post path cachedir)]
      [(regexp "[.]html$")
       (read-html-post path)]
      [(regexp "[.]md$")
       (define footnote-prefix (string->symbol (path->prefix path)))
       (read-markdown-post path #f footnote-prefix)]
      [(regexp "[.]mdt$")
       (define footnote-prefix (string->symbol (path->prefix path)))
       (define text (render-template dir (path->string name) '()))
       (read-markdown-post path text footnote-prefix)]))
  (define meta-h (merge-metadata header-meta-h body-meta-h path-meta-h))
  ;; Split out the blurb (may be less than the entire body)
  (define-values (blurb more?) (above-the-fold body))
  (define body* (enhance-body body))
  (define blurb* (enhance-body blurb))
  ;; Write files to cachedir
  (with-output-to-file (build-path cachedir "_index.rktd") #:exists 'replace
    (lambda () (printf "~s\n" body*)))
  (with-output-to-file (build-path cachedir "_cache.rktd") #:exists 'replace
    (lambda () (printf "~s\n\n~s\n\n~s\n\n~s\n" timestamp meta-h blurb* more?)))
  (void))

;; path->prefix : Path -> String
(define (path->prefix path)
  ;; Returns the file part of the path without the extension.
  ;; Used for Markdown footnotes, Scribble image subdirs, etc.
  (path->string (path-replace-extension (file-name-from-path path) #"")))

;; above-the-fold? : XExprs -> (values XExprs Boolean)
(define (above-the-fold xs)
  (define-values (above below)
    (splitf-at xs (lambda (x) (not (more-xexpr? x)))))
  (values above (not (empty? below))))

(define (more-xexpr? x)
  (match x
    [`(!HTML-COMMENT () ,(pregexp "more")) #t]
    [_ #f]))

(define (enhance-body xs)
  ;; FIXME
  xs)

;; ------------------------------------------------------------
;; Scribble Posts

;; build-scribble-post : Path Path -> (values XExprs MetaHash MetaHash)
(define (build-scribble-post path cachedir)
  (define header-meta-h
    (with-input-from-file path
      (lambda () (read-metadata-header 'comment))))
  (run-scribble path cachedir)
  ;; Delete the usual Scribble aux files; refs to them are redirected.
  (for ([file (in-list (directory-list cachedir))])
    (when (usual-scribble-file? file)
      (delete-file (build-path cachedir file))))
  ;; Read as XExprs
  (define xexprs
    (with-input-from-file (build-path cachedir SCRIBBLE-OUTFILE)
      read-html-as-xexprs))
  (define body0 (extract-scribble-body xexprs))
  (define-values (body scribble-meta-h) (adjust-scribble-html body0))
  (values body header-meta-h scribble-meta-h))

(define SCRIBBLE-OUTFILE "__index.html")

;; run-scribble : Path Path -> Void
(define (run-scribble path outdir)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-command-line-arguments
                  (vector "--quiet"
                          "--html"
                          "--dest" (path->string outdir)
                          "--dest-name" SCRIBBLE-OUTFILE
                          "--redirect" "https://docs.racket-lang.org/local-redirect/"
                          "--redirect-main" "https://docs.racket-lang.org"
                          "++main-xref-in"
                          (path->string path))])
    (dynamic-require 'scribble/run #f)))

;; usual-scribble-files : (Listof String)
(define usual-scribble-files
  '("manual-fonts.css" "manual-racket.css" "manual-style.css" "manual-racket.js"
    "racket.css" "scribble-common.js" "scribble.css"))

;; usual-scribble-file? : Path -> Boolean
(define (usual-scribble-file? file)
  (member (path->string file) usual-scribble-files))

;; extract-scribble-body : XExprs -> XExprs
(define (extract-scribble-body xexprs)
  ;; Extract the part we care about -- the elements in the "main" div after
  ;; the "versionbox" div. (The `match` might be too fragile way to do this.)
  (match (cadr xexprs)
    [`(html
       ()
       (head . ,_)
       ,(list-no-order
         `(div ([class "maincolumn"])
               (div ([class "main"]) . ,xs))
         _ ...))
     xs]
    [_ (error 'extract-scribble-body "bad scribble output: ~e" xexprs)]))

;; adjust-scribble-html : XExprs -> (values XExprs MetaHash)
(define (adjust-scribble-html xs)
  (define meta-h #hash()) ;; mutated
  (define xs*
    (xexpr-map*
     (lambda (x _)
       (match x
         ;; Delete version
         [`(div ([class "versionbox"]) . ,_) '()]
         ;; Delete author box; record authors in meta-h
         [`(div ([class "SAuthorListBox"]) . ,_)
          (match x
            [`(div ([class "SAuthorListBox"])
               (span ([class "SAuthorList"]) . ,_)
               ,@(list `(p ([class "author"]) ,(? string? authors)) ...))
             (set! meta-h (hash-set meta-h 'author (string-join authors ", ")))]
            [_ (void)])
          '()]
         ;; Delete title; record title in meta-h
         [`(h2 . ,_)
          (match x
            [`(h2 () (a . ,_) ,@title-xs)
             (set! meta-h (hash-set meta-h 'title (xexpr->markdown `(span () ,@title-xs))))]
            [_ (void)])
          '()]
         ;; Convert blockquotes (???)
         [`(blockquote ([class "SCodeFlow"]) . ,xs)
          `[(div ([class "SCodeFlow"]) ,@xs)]]
         ;; Adjust headers:
         ;; Scribble @title is rendered as <h2>, @section as <h3>,
         ;; and @subsection as <h4>, and so on. Hoist the headings up
         ;; to be consistent with the Markdown format sources.
         ;; (that is, @section as <h2>, @subsection as <h3>, etc).
         [`(h3 . ,x) `[(h2 ,@x)]]
         [`(h4 . ,x) `[(h3 ,@x)]]
         [`(h5 . ,x) `[(h4 ,@x)]]
         [`(h6 . ,x) `[(h5 ,@x)]]
         ;; Turn <!--more--> into html comment
         [`(p () "<" "!" ndash " more " ndash ">") `[(!HTML-COMMENT () "more")]]
         [x (list x)]))
     xs))
  (values xs* meta-h))

;; ------------------------------------------------------------
;; Markdown Posts

;; read-markdown-post : Path (U String #f) String -> (values XExprs MetaHash MetaHash)
(define (read-markdown-post path maybe-text footnote-prefix)
  (define (handle-src in)
    (define meta-h (read-metadata-header 'spaces in))
    (values meta-h (port->string in)))
  (define-values (meta-h body-text)
    (cond [maybe-text (handle-src (open-input-string maybe-text))]
          [else (call-with-input-file* path handle-src)]))
  (define xs (parse-markdown body-text footnote-prefix))
  (values xs meta-h '#hasheq()))

;; ------------------------------------------------------------
;; HTML Posts

;; read-html-post : Path -> (values XExprs MetaHash MetaHash)
(define (read-html-post path)
  (define-values (meta-h body-xs)
    (call-with-input-file* path
      (lambda (in)
        (define meta-h (read-metadata-header 'spaces in))
        (define body-xs (cddr (read-html-as-xexprs in)))
        (values meta-h body-xs))))
  (values body-xs meta-h '#hasheq()))


;; ============================================================
;; Built Post Info

;; PostInfo = instance of postinfo%
(define postinfo%
  (class object%
    (init-field src meta blurb more?)
    (super-new)

    (define/public (get-src) src)
    (define/public (get-meta) meta)
    (define/public (get-blurb) blurb)
    (define/public (get-more?) more?)

    (define/public (get-cachedir) (postsrc-cachedir src))

    (define/public (get-title) (metadata-title meta))
    (define/public (get-author) (metadata-author meta))
    (define/public (get-date) (metadata-date meta))
    (define/public (get-tags) (metadata-tags meta))

    (define/public (get-rel-www) (post-meta->rel-www meta))
    (define/public (get-out-dir) (build-path (get-dest-dir) (get-rel-www)))
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

    (define/public (get-body-xexprs)
      (with-input-from-file (build-path (postsrc-cachedir src) "_index.rktd")
        read))

    ;; Rendering

    (define/public (get-title-html) (title->html (get-title)))
    (define/public (get-body-html) (xexprs->html (get-body-xexprs)))
    (define/public (get-date-html) (xexpr->html (get-date-xexpr)))
    (define/public (get-tags-html) (xexpr->html (get-tags-xexpr)))

    (define/public (get-date-xexpr)
      (let ([d (get-date)]) `(time ([datetime ,d] [pubdate "true"]) ,d)))
    (define/public (get-tags-xexpr)
      `(span
        ([class "tags"])
        ,@(add-between (for/list ([t (in-list (get-tags))])
                         (tag->xexpr t))
                       ", ")))

    ;; Rendering utils -- FIXME: split out?

    (define/public (xexpr->html x) (xexpr->string x))
    (define/public (xexprs->html xs) (string-join (map xexpr->string xs) "\n"))
    (define/public (title->html t)
      ;; `parse-markdown` returns (listof xexpr?). For simple "one-liner"
      ;; markdown that's usually a list with just a single 'p element. In
      ;; that case, discard the 'p and use its body element(s). If it
      ;; parsed to something more complicated, the visual result will
      ;; probably be unappealing, but at least handle that case here.
      (define xs (match (parse-markdown t)
                   [`((p () . ,xs)) xs]
                   [xs xs]))
      (string-join (map xexpr->string xs) ""))
    (define/public (tag->xexpr tag-s)
      `(a ([href ,(tag->enc-url tag-s)]) ,tag-s))
    (define/public (tag->enc-url tag-s)
      (url->string (get-tag-url tag-s)))
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
