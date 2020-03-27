#lang racket/base
(require racket/contract/base
         "private/util.rkt"
         (only-in xml xml-attribute-encode xexpr/c)
         (only-in "private/xexpr.rkt" xexpr->html xexprs->html))
(provide (all-from-out "private/util.rkt")
         (contract-out
          [$h (->* [] [] #:rest (listof xexpr/c) string?)]
          [$hs (-> (listof xexpr/c) string?)]
          [$a (-> string? string?)])
         $try
         $cancel)

;; ============================================================
;; Helpers (FIXME: move elsewhere)

;; In general, all methods except those ending in -html and -link
;; return data that must be explicitly converted to HTML to escape
;; markup characters (eg, "<", "\"", etc).

;; $h : XExpr ... -> String  -- note, XExpr includes String!
;; Use to convert XExprs (including simple Strings!) to HTML.
(define ($h . xs) (xexprs->html xs))

;; $hs : (Listof XExpr) -> String
(define ($hs xs) (xexprs->html xs))

;; $a : String -> String
;; Use for element attributes, which have different escaping rules.
(define ($a s) (xml-attribute-encode s))

;; ($try e ...) => (list e ...) unless some e calls ($cancel), then null
(define-syntax-rule ($try e ...)
  (with-handlers ([cancel? (lambda (x) null)])
    (list e ...)))
(define ($cancel) (raise 'cancel))
(define (cancel? v) (eq? v 'cancel))
