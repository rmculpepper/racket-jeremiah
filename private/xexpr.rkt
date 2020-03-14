#lang racket/base
(require racket/match
         racket/list
         racket/format
         racket/function
         racket/string
         xml
         (only-in html read-html-as-xml))
(provide read-html-as-xexprs
         xexpr->html
         xexprs->html
         xexpr-map
         xexpr-map*
         xexprs->description)

;; ============================================================
;; HTML

;; read-html-as-xexprs : -> (Listof XExpr)
(define (read-html-as-xexprs)
  (define x (element #f #f 'root '() (read-html-as-xml)))
  (cddr (decode-ampersands-in-attributes (xml->xexpr x))))

;; decode-ampersands-in-attributes : XExpr -> XExpr
(define (decode-ampersands-in-attributes x)
  (match x
    [`(,tag ([,ks ,vs] ...) . ,els)
     `(,tag
       ,(for/list ([k ks]
                   [v vs])
          (list k (regexp-replace* "&amp;" v "\\&")))
       ,@(map decode-ampersands-in-attributes els))]
    [v v]))

(define (xexpr->html x) (xexpr->string x))
(define (xexprs->html xs) (string-join (map xexpr->string xs) "\n"))


;; ============================================================
;; XExpr traversal

;; Does depth-first traversal of the xexpr `x`, calling `f` for each
;; sub-xexpr of `x` (including `x` itself).
;;
;; The xexprs passed to `f` always have an explicit attributes list --
;; similar to `(parameterize ([xexpr-drop-empty-attributes
;; #f]))`. Normalizing them like this simplifies matching.
;;
;; Note that `f` must return a `(listof xexpr?)`, not an
;; `xexpr?`. This permits `f` to replace one xexpr with several that
;; get spliced in where the original one was (without needing to nest
;; them in some artificial parent element like a `span` or `div`). For
;; example: Can replace '(em "foo" "bar") with "foo" "bar". Of course,
;; this means if `f` wants to return the xexpr unchanged, it must be
;; nested in list: `(list original)` not `original`.  To delete an
;; xexpr completely, `f` can return the empty list '().
;;
;; `f` is also passed a `(listof xexpr?)` that is the parent xexprs,
;; in "reverse" order (parent, grandparent, and so on). This allows
;; transforming elements that are only descendants of specific other
;; elements. Note that these are the full xexprs; if you only care
;; about the tag symbols, you can run the list through (map first
;; parents) to get something like `(td tr tbody table)` or whatever.
;; To match on both the xexpr and the parents, you may find `match*`
;; handy.

;; xexpr-map : (XExpr (Listof XExpr) -> (Listof XExpr)) XExpr -> XExpr
(define (xexpr-map f x)
  (match (xexpr-map* f (list x))
    [(list e) e]
    [r (error 'xexpr-map "got non-singleton result list: ~e" r)]))

;; xexpr-map* : (XExpr (Listof XExpr) -> (Listof XExpr)) (Listof XExpr) -> (Listof XExpr)
(define (xexpr-map* f xs)
  (define ((inner ps) x)
    (match x
      ;; xexpr with explicit attributes (even if just '())
      [`(,(? symbol? tag) ([,(? symbol? ks) ,(? string? vs)] ...) . ,es)
       (f `(,tag ,(map list ks vs) ,@(append* (map (inner (cons x ps)) es))) ps)]
      ;; xexpr with no attributes: transform to empty list
      [`(,(? symbol? tag) . ,es) ((inner ps) `(,tag () ,@es))]
      [x (f x ps)]))
  (append* (map (inner '()) xs)))


;; ============================================================
;; XExpr to Text

;; xexprs->description : (Listof XExpr) [Nat] -> String
(define (xexprs->description xs [max-len 255])
  (define s (kill-newlines (string-join (map (curryr xexpr->markdown " ") xs) "")))
  (define len (string-length s))
  (define sub (substring s 0 (min max-len len)))
  (define esc (escape-double-quotes sub))
  (cond [(< len (string-length sub)) esc]
        [else (~a esc "...")]))

(define (substring* s from upto)
  (substring s from (min upto (string-length s))))

;; xexpr->markdown : XExpr [String] -> String
;; Not full markdown, just a "lite" variant for human readers only.
(define (xexpr->markdown x [block-suffix ""])
  (define (heading? s)
    (memq s '(h1 h2 h3 h4 h5 h6 h7 h8 h9)))
  (define (block? s)
    (memq s '(p div li)))
  (define (->s es) ;convert entities to string
    (apply ~a (map (curryr xexpr->markdown block-suffix) es)))
  (define (normalize x) ;; ensure xexpr has explicit attributes
    (match x
      [`(,(? symbol? tag) ([,(? symbol? ks) ,(? string? vs)] ...) . ,es) x]
      [`(,(? symbol? tag)                                         . ,es) `(,tag () ,@es)]
      [_                                                                 x]))
  (match (normalize x)
    [`(em            ,_ . ,es) (~a "_" (->s es) "_")]
    [`(strong        ,_ . ,es) (~a "**" (->s es) "**")]
    [`(code          ,_ . ,es) (~a "`" (->s es) "`")]
    [`(,(? heading?) ,_ . ,es) (~a (->s es) ": ")]
    [`(,(? block?)   ,_ . ,es) (~a (->s es) block-suffix)]
    [`(,(? symbol?)  ,_ . ,es) (~a (->s es))]
    [(? string? s)             s]
    ['ndash                    "-"]
    ['mdash                    "--"]
    ['amp                      "&"]
    [(or 'lsquo 'rsquo)        "'"]
    [(or 'ldquo 'rdquo 'quot)  "\""]
    [(? valid-char? i)         (string (integer->char i))]
    [_                         ""])) ;; ignore others

;; escape-double-quotes : String -> String
(define (escape-double-quotes s)
  (regexp-replace* #rx"\"" s "\\&quot;")) ;need to escape `&` in replace str

;; kill-newlines : String -> String
(define (kill-newlines s)
  (string-trim #:left? #t #:right? #t #:repeat? #t
               (regexp-replace* "\n+" s " ")))
