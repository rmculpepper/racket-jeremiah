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

    (define/public (tags->links-html tags)
      (xexprs->html (tags->links-xexprs tags)))
    (define/public (tags->links-xexprs tags)
      (add-between (map (lambda (t) (tag->link-xexpr t)) tags) ", "))

    (define/public (tag->link-html tag)
      (xexpr->html (tag->link-xexpr tag)))
    (define/public (tag->link-xexpr tag)
      `(a ([href ,(get-tag-link tag)]) ,tag))

    (define/public (date->html d)
      (xexpr->html (date->xexpr d)))
    (define/public (date->xexpr d)
      (if d `(time ([datetime ,d]) ,d) `(span)))
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

    get-header-html
    get-header-xexprs

    get-feed-link
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

    (define/public (get-header-html)
      (xexprs->html (get-header-xexprs)))
    (define/public (get-header-xexprs)
      (define tag (send index get-tag))
      `((title ,(send index get-title))
        ;; (meta ([name "description"] [content ""])) ;; FIXME
        ,@(if tag (list `(meta ([name "keywords"] [content ,tag]))) '())
        (link ([rel "alternate"] [type "application/atom+xml"] [title "Atom Feed"]
               [href ,(send index get-feed-link)]))
        ,@(cond [(zero? page-num) null]
                [else (list `(link ([rel "prev"] [href ,(get-link (sub1 page-num))])))])
        ,@(cond [(= page-num (sub1 num-pages)) null]
                [else (list `(link ([rel "next"] [href ,(get-link (add1 page-num))])))])
        ))
    ))


;; ============================================================
;; Render post

(define render-post%
  (class* post% (page<%>)
    (inherit-field)
    (inherit get-title
             get-date
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
    (define/public (get-header-html) (xexprs->html (get-header-xexprs)))

    (define/public (get-header-xexprs)
      `((title ,(get-title))
        (meta ([name "description"] [content ""])) ;; FIXME
        ;;(meta ([name "author"] [content ,(get-authors)])) ;; FIXME
        (meta ([name "keywords"] [content ,(string-join (get-tags) ",")]))
        (link ([rel "canonical"] [href ,(get-full-link)]))
        (link ([rel "alternate"] [type "application/atom+xml"] [title "Atom Feed"]
               [href ,(get-atom-feed-link "all")]))
        ,@(cond [(send (send (the-site) get-index) get-prev this)
                 => (lambda (prev)
                      (list `(link ([rel "prev"] [href ,(send prev get-link)]))))]
                [else null])
        ,@(cond [(send (send (the-site) get-index) get-next this)
                 => (lambda (next)
                      (list `(link ([rel "next"] [href ,(send next get-link)]))))]
                [else null])))
    ))
