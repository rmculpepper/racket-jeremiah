#lang racket/base
(require racket/match
         racket/list
         racket/string
         racket/hash
         racket/sequence
         racket/port
         racket/path
         "template.rkt"
         "read-scribble.rkt")

;; CACHE
;; src-path -> (post-rendering Timestamp MetaHash CacheDir/#f)
;;   -- keep cache dir only if aux files, eg images

;; Note: won't know final destination dir until metadata parsed!

(define (post-file-name? str)
  (or (dated-post-file-name? str)
      (draft-post-file-name? str)))

(define (dated-post-file-name? str)
  (regexp-match? #px"^\\d{4}-\\d{2}-\\d{2}-(?:.+?)[.](?:md|mdt|scrbl|html)$" str))

(define (draft-post-file-name? str)
  (regexp-match? #px"^draft-(?:.+?)[.](?:md|mdt|scrbl|html)$" str))


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
(define (read-metadata-header mode in)
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
