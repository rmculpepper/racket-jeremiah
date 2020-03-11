#lang racket/base
(require racket/contract/base
         racket/file
         racket/path
         racket/format
         racket/match
         racket/string
         racket/pretty
         threading
         (only-in xml xexpr?)
         "html.rkt"
         "xexpr2text.rkt"
         "xexpr-map.rkt")
(provide read-scribble-file)

;; read-scribble-file : Path -> (values XExprs MetaHash)
(define (read-scribble-file path)
  (define dir (path->string (make-temporary-file "frog~a" 'directory)))
  (run-scribble path dir)
  ;; Delete the usual Scribble aux files; refs to them are redirected.
  (for ([file (in-list (find-files file-exists? dir))]
        #:when (usual-scribble-file? dir file))
    (delete-file file))
  ;; Debug
  (when #f
    (eprintf "rendering ~a produced ~s\n"
             (file-name-from-path path)
             (map path->string (map file-name-from-path (find-files file-exists? dir)))))
  (define xexprs (with-input-from-file (build-path dir OUTFILE) read-html-as-xexprs))
  ;; Extract the part we care about -- the elements in the "main" div
  ;; after the "versionbox" div.  (The `match` might be too fragile
  ;; way to do this.)
  (match (cadr xexprs)
    [`(html
       ()
       (head . ,_)
       ,(list-no-order
         `(div ([class "maincolumn"])
               (div ([class "main"])
                    . ,xs))
         _ ...))
     (adjust-scribble-html xs)]
    [x (error 'read-scribble-file "bad scribble post: ~e" x)]))

(define OUTFILE "__index.html")

;; run-scribble : Path Path -> Void
(define (run-scribble path out-dir)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-command-line-arguments
                  (vector "--quiet"
                          "--html"
                          "--dest" out-dir
                          "--dest-name" OUTFILE
                          "--redirect" "https://docs.racket-lang.org/local-redirect/"
                          "--redirect-main" "https://docs.racket-lang.org"
                          "++main-xref-in"
                          (path->string path))])
    (dynamic-require 'scribble/run #f)))

;; usual-scribble-files : (Listof String)
(define usual-scribble-files
  '("manual-fonts.css" "manual-racket.css" "manual-style.css" "manual-racket.js"
    "racket.css" "scribble-common.js" "scribble.css"))

;; usual-scribble-file? : Path Path -> Boolean
(define (usual-scribble-file? dir file)
  (define-values (file-dir file-name _) (split-path file))
  (and (equal? file-dir dir)
       (member (path->string file-name) usual-scribble-files)))

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


;; ============================================================

(module+ test
  (require rackunit)

  (let ([path (make-temporary-file)]
        [s #<<EOF
#lang scribble/manual
@title{The Post's Title}
@section{Section 1}
Here is some text.

<!-- more -->

Below the fold.
EOF
])
    (with-output-to-file path #:exists 'replace (λ () (display s)))
    (check-equal?
     (read-scribble-file path)
     '((h1 () (a ((name "(part._.The_.Post_s_.Title)"))) "The Post" rsquo "s Title")
      (h1 () "1" (tt () nbsp) (a ((name "(part._.Section_1)"))) "Section 1")
      (p () "Here is some text.")
      (!HTML-COMMENT () "more")
      (p () "Below the fold.")))
    (delete-file path))
  ;; regression test for https://github.com/greghendershott/frog/issues/75
  (let ([path (make-temporary-file)]
        [s #<<EOF
#lang scribble/manual
@hyperlink["https://aur.archlinux.org/packages/?SeB=m&K=bluephoenix47" "Aur"]
EOF
])
    (with-output-to-file path #:exists 'replace (λ () (display s)))
    (check-equal?
     (read-scribble-file path)
     '((p ()
        (a ((href "https://aur.archlinux.org/packages/?SeB=m&K=bluephoenix47")) "Aur"))))
    (delete-file path)))
