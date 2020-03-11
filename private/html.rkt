#lang racket/base
(require racket/match
         xml
         (only-in html read-html-as-xml))
(provide read-html-as-xexprs)

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

;; ============================================================
(module+ test
  (require rackunit
           racket/port)
  (define input
    "<p>Hi</p>
<p><a href='/path/to/thing?a=1&b=2'>link</a></p>
<p><a href='/path/to/thing?a=1&amp;b=2'>link</a></p>")
  (check-equal? (with-input-from-string input read-html-as-xexprs)
                '((p () "Hi") "\n"
                  (p () (a ((href "/path/to/thing?a=1&b=2")) "link")) "\n"
                  (p () (a ((href "/path/to/thing?a=1&b=2")) "link")))))
