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
         (only-in xml xml-attribute-encode)
         "config.rkt"
         "data.rkt"
         "xexpr.rkt")
(provide (all-defined-out))

;; ============================================================
;; Site

(define render-site%
  (class site%
    (super-new)

    ;; Rendering utilities for use by templates, etc

    (define/public (to-html x) (xexpr->html x))
    (define/public (to-attr s) (xml-attribute-encode s))

    (define/public (tags->links-html tags)
      (string-join (map xexpr->html (add-between (tags->links-xexprs tags) ", ")) ""))
    (define/public (tags->links-xexprs tags)
      (map (lambda (t) (tag->link-xexpr t)) tags))

    (define/public (tag->link-html tag)
      (xexpr->html (tag->link-xexpr tag)))
    (define/public (tag->link-xexpr tag)
      `(a ([href ,(get-tag-link tag)]) ,tag))

    (define/public (date->html d)
      (xexpr->html (date->xexpr d)))
    (define/public (date->xexpr d)
      (if d `(time ([datetime ,d]) ,d) `(span)))

    (define/public (get-extra-html location page)
      ((or (extra-html) void) location page))
    ))

;; ============================================================
;; Pages

(define page<%>
  (interface ()
    get-page-type   ;; -> (U 'post 'index ...)

    get-url         ;; may point to a directory with an index.html file
    get-link

    get-page-url    ;; points specificially to the page's HTML file
    get-page-link

    get-title      ;; -> String
    get-keywords   ;; -> String (note, not (Listof String)!)
    get-feed-link  ;; -> String
    get-prev-link  ;; -> String/#f
    get-next-link  ;; -> String/#F
    ))


;; ============================================================
;; Render index page

(define render-index-page%
  (class* index-page% (page<%>)
    (inherit-field index page-num num-pages)
    (inherit get-link get-posts)
    (super-new)

    (define/public (get-page-type) 'index)
    (define/public (is-page-type? type) (eq? (get-page-type) type))

    (define/public (render-page-html)
      (define content-html (render-content-html))
      ((page-renderer) this content-html))

    (define/public (render-content-html)
      (define rendered-posts
        (for/list ([post (in-list (get-posts))])
          (send post render-index-entry-html)))
      (string-join rendered-posts "\n"))

    (define/public (get-keywords) (or (send index get-tag) ""))
    (define/public (get-prev-link)
      (cond [(zero? page-num) #f] [else (get-link (sub1 page-num))]))
    (define/public (get-next-link)
      (cond [(= page-num (sub1 num-pages)) #f] [else (get-link (add1 page-num))]))
    ))


;; ============================================================
;; Render post

(define render-post%
  (class* post% (page<%>)
    (inherit-field)
    (inherit get-date
             get-authors
             get-tags
             get-link
             get-full-link
             get-title-xexpr
             get-blurb-xexprs
             get-body-xexprs)
    (super-new)

    (define/public (get-page-type) 'post)
    (define/public (is-page-type? type) (eq? (get-page-type) type))

    (define/public (render-page-html)
      (define content-html (render-content-html))
      ((page-renderer) this content-html))

    (define/public (render-content-html)
      ((post-renderer) this))

    (define/public (render-index-entry-html)
      ((index-entry-renderer) this))

    (define/public (get-title-html) (xexpr->html (get-title-xexpr)))
    (define/public (get-blurb-html) (xexprs->html (get-blurb-xexprs)))
    (define/public (get-body-html) (xexprs->html (get-body-xexprs)))

    (define/public (get-keywords) (string-join (get-tags) ","))
    (define/public (get-next-link)
      (cond [(get-next-post) => (lambda (p) (send p get-link))] [else #f]))
    (define/public (get-prev-link)
      (cond [(get-prev-post) => (lambda (p) (send p get-link))] [else #f]))

    (define/public (get-prev-post)
      (let ([index (send (the-site) get-index)]) (send index get-prev this)))
    (define/public (get-next-post)
      (let ([index (send (the-site) get-index)]) (send index get-next this)))
    ))
