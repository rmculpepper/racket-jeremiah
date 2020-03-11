#lang racket/base
(require racket/contract/base
         racket/format
         racket/function
         racket/match
         racket/string
         xml)
(provide xexprs->description
         xexpr->markdown)

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

;; ============================================================

(module+ test
  (require rackunit)

  ;; ----
  (check-equal?
   (xexprs->description '((h1 ([class "foo"]) "A heading")
                          (p "A " (em "paragraph") " of some stuff.")
                          (p "A " (em "paragraph") " of some stuff."))
                        50)
   "A heading: A _paragraph_ of some stuff. A _paragra...")
  (check-equal?
   (xexprs->description '((h1 ([class "foo"]) "A heading")
                          (img ([src "blah"]))
                          (p "A " (em "paragraph") " of \"some\" stuff."))
                        50)
   "A heading: A _paragraph_ of &quot;some&quot; stuff....")

  ;; ----
  (check-equal? (xexpr->markdown '(em "italic"))
                "_italic_")
  (check-equal? (xexpr->markdown '(em ([class "foo"]) "italic"))
                "_italic_")
  (check-equal? (xexpr->markdown '(strong "bold"))
                "**bold**")
  (check-equal? (xexpr->markdown '(strong ([class "foo"]) "bold"))
                "**bold**")
  (check-equal? (xexpr->markdown '(em "italic " (strong "bold") " italic"))
                "_italic **bold** italic_")
  (check-equal? (xexpr->markdown '(p "I am some " (em "italic") " text"))
                "I am some _italic_ text")
  (check-equal? (xexpr->markdown '(p ([class "foo"])
                                     "I am some " (em "italic") " text"))
                "I am some _italic_ text")
  (check-equal? (xexpr->markdown '(p "M" 'amp "Ms" 'mdash "gotta love 'em"))
                "M&Ms--gotta love 'em")
  (check-equal? (xexpr->markdown '(div (p "Hi.") (p "Hi.")) "\n")
                "Hi.\nHi.\n\n")
  (check-equal? (xexpr->markdown '(p "Hi" #x20 "there"))
                "Hi there")
  (check-equal? (xexpr->markdown `(p () "A " ,(char->integer #\λ) " char"))
                "A λ char")

  ;; ----
  (check-equal? (escape-double-quotes "A \"double quote\" in the string.")
                "A &quot;double quote&quot; in the string.")

  ;; ----
  (check-equal? (kill-newlines "\nHi.\n")
                "Hi.")
  (check-equal? (kill-newlines "\nHi\nthere.\n")
                "Hi there.")
  (check-equal? (kill-newlines "\nPara\n1.\n\nPara\n2.\n")
                "Para 1. Para 2."))
