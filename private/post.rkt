#lang racket/base
(require racket/match
         racket/list
         racket/string
         racket/hash
         racket/sequence
         racket/port
         racket/path
         racket/file
         markdown
         "template.rkt"
         "html.rkt"
         "xexpr-map.rkt"
         "xexpr2text.rkt")
(provide (all-defined-out))

(define (post-file-name? str)
  (or (dated-post-file-name? str)
      (draft-post-file-name? str)))

(define (dated-post-file-name? str)
  (regexp-match? #px"^\\d{4}-\\d{2}-\\d{2}-(?:.+?)[.](?:md|mdt|scrbl|html)$" str))

(define (draft-post-file-name? str)
  (regexp-match? #px"^draft-(?:.+?)[.](?:md|mdt|scrbl|html)$" str))


;; ============================================================
;; Metadata

;; PostMetadata is a hasheq with the following keys:
;; - 'title : String
;; - 'date : #f or "draft" or String of the form "YYYY-MM-DD"
;; - 'datesort : String -- extra stuff (eg time of day) to control sorting
;; - 'author : (Listof String)
;; - 'tags : (Listof String)

;; FIXME: add 'time ?

;; The following tags are special:
;; - DRAFT - implies NOINDEX
;; - NOINDEX - leave out of blog index/history, sitemap, etc
;; - NORENDER - don't process at all

;; A post's metadata is gathered from the following sources (later
;; overrides earlier):
;; - path of source file
;; - content (eg, @author, @title)
;; - header


;; TODO: validate/interpret metahash

(define (check-metadata meta-h)
  (error 'check-metadata "unimplemented"))

(define (tag-string->tags s)
  (match (regexp-split #px"," (string-trim s))
    ['("") '()]
    [ss    (map string-trim ss)]))


;; merge-metadata : MetaHash ...+ -> MetaHash
;; Merges metadata; each key get value from *leftmost* hash that has a value for it.
(define (merge-metadata mh . mhs)
  (apply hash-union mh mhs #:combine (lambda (a b) a)))

;; path->metadata : Path -> MetaHash
(define (path->metadata path)
  (match (path->string (file-name-from-path path))
    [(regexp #rx"^(\\d{4}-\\d{2}-\\d{2})-(.+?)[.](?:md|mdt|scrbl|html)$"
             (list _ date-str title-str))
     (hasheq 'date date-str 'title title-str)]
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

;; BUILD writes the following to the cachedir:
;; - _cache.rktd - timestamp, meta, blurb-xexprs
;; - _index.rktd - xexprs of body (need to delay, because of prev/next links?)
;; - _<other> - other non-public files starting with _
;; - aux files linked from body

;; build-post : Path -> Void
;; PRE: path refers to file, is simplified, has at least one directory part
;; PRE: cachedir exists (might not be empty)
(define (build-post path cachedir)
  (define timestamp (file-or-directory-modify-seconds path))
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
    (lambda () (printf "~s\n\n~s\n\n~s\n\n~s\n" timestamp meta-h more? blurb*)))
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
             (set! meta-h (hash-set meta-h "Title" (xexpr->markdown `(span () ,@title-xs))))]
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


;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#|

#|
POST Conventions
- path -> date convention
  - eg, path "YYYY-MM-DD-auxtitle.ext" -> YYYY-MM-DD
  - eg, path "YYYY/MM-DD-auxtitle.ext" -> YYYY-MM-DD
- draft posts
- in-source metadata: author, title
- render to { YYYY/MM/cleaned-title.html, YYYY/MM/cleaned-title/<aux-file> }
  - alternatively: YYYY/MM/DD/cleaned-title/{index.html, <aux-file>}
- processing
  - remove local scribble css, redirect links to global; also javascript
  - add other css/js links?
  - optional processing via enhance-body
|#

;; CACHE
;; src-path -> (post-rendering Timestamp MetaHash CacheDir/#f)
;;   -- keep cache dir only if aux files, eg images

;; Note: won't know final destination dir until metadata parsed!





(struct post
  (title      ;; String
   date       ;; String
   tags       ;; (Listof String)
   blurb      ;; String - HTML of the post summary
   body       ;; #f or String - HTML of full post, or #f if same as blurb
   dest-dir   ;; Path - full path of dir w/ aux files
   ) #:prefab)

;; DELETED
;; dest-path  ;; Path - full pathname of local HTML file
;; uri-path   ;; String - path portion of URI, with leading /
;; src-path   ;; Path - full pathname of source file
;; modified   ;; Nat - time blurb/body were updated

;; ============================================================
;; Reading Posts

;; read-post : Path -> (U #f Post)
;; PRE: path refers to file, is simplified, has a at least one directory part
(define (read-post path)
  (define-values (dir name d?) (split-path path)) ;; PRE => name, dir are paths, d? is #f
  (define-values (body header-meta-h body-meta-h)
    (match (path->string name)
      [(regexp "[.]scrbl$")
       (read-scribble-post path)]
      [(regexp "[.]html$")
       (read-html-post path)]
      [(regexp "[.]md$")
       (define footnote-prefix (string->symbol (path->prefix path)))
       (read-markdown-post path #f footnote-prefix)]
      [(regexp "[.]mdt$")
       (define footnote-prefix (string->symbol (path->prefix path)))
       (define text (render-template dir (path->string name) '()))
       (read-markdown-post path text footnote-prefix)]))
  (define meta-h (merge-metadata header-meta-h body-meta-h (path->metadata path)))
  (finish-read-post path body meta-h))

;; finish-read-post : Path (Listof XExpr) MetaHash -> (U Post #f)
(define (finish-read-post path body meta-h)
  (define name (file-name-from-path path))
  (match-define (list title date-str tags) (check-metadata path meta-h))
  (let/ec return
    (when (member "DRAFT" tags)
      ;; Skipping due to DRAFT tag
      (return #f))
    ;; Split out the blurb (may be less than the entire body)
    (define-values (blurb more?) (above-the-fold body))
    (post title date-str tags
          (xexprs->string (enhance-body blurb))
          (and more? (xexprs->string (enhance-body body)))
          dir)))

;; path->prefix : Path -> String
(define (path->prefix path)
  ;; Returns the file part of the path without the extension.
  ;; Used for Markdown footnotes, Scribble image subdirs, etc.
  (path->string (path-replace-extension (file-name-from-path path) #"")))

;; above-the-fold? : XExprs -> (values XExprs Boolean)
(define (above-the-fold xs)
  (define-values (above below)
    (splitf-at (lambda (x) (not (more-xexpr? x))) xs))
  (values above (not (empty? below))))

(define (more-xexpr? x)
  (match x
    [`(!HTML-COMMENT () ,(pregexp "more")) #t]
    [_ #f]))

;; xexprs->string : XExprs -> String
(define (xexprs->string xs)
  (string-join (map xexpr->string xs) "\n"))

(define (enhance-body xs)
  ;; FIXME
  xs)

;; ------------------------------------------------------------
;; Scribble Posts

;; read-scribble-post : Path String String -> (values XExprs MetaHash MetaHash)
(define (read-scribble-post path)
  (define img-dest (build-path (www/img-path) "posts" (post-path->prefix path)))
  (define header-meta-h
    (call-with-input-file* path (lambda (in) (read-metadata-header 'comment in))))
  (define-values (xs scribble-meta-h)
    (read-scribble-file path
                        #:img-local-path img-dest
                        #:img-uri-prefix (canonical-uri (abs->rel/www img-dest))))
  (values xs header-meta-h scribble-meta-h))

;; ------------------------------------------------------------
;; Markdown Posts

;; read-markdown-post : Path (U String #f) String -> (values XExprs MetaHash MetaHash)
(define (read-markdown-post path maybe-text footnote-prefix)
  (define (handle-src in)
    (define meta-h (read-meta-data 'spaces in))
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
        (define meta-h (read-meta-data 'spaces in))
        (define body-xs (cddr (read-html-as-xexprs in)))
        (values meta-h body-xs))))
  (values body-xs meta-h '#hasheq()))
|#
