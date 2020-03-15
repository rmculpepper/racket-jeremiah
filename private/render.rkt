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
         "../config.rkt"
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

    get-pagination-html
    get-pagination-xexprs
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
      ((or (page-renderer) default-page-renderer) this content-html))

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

    (define/public (get-pagination-html)
      (xexprs->html (get-pagination-xexprs)))
    (define/public (get-pagination-xexprs)
      (define file-name-base (send index get-tag-dest-file-name-base))
      (list `(footer ,(bootstrap-pagination file-name-base page-num num-pages))))
    ))

(define (bootstrap-pagination file-name-base page-num num-pages)
  `(ul ([class "pagination"])
       ,(cond [(zero? page-num)
               `(li ([class "page-item disabled"])
                    (a ([class "page-link"] [href "#"]) 'larr))]
              [else
               `(li ([class "page-item"])
                    (a ([class "page-link"]
                        [href ,(file/page file-name-base (sub1 page-num))])
                       'larr))])
       ,@(for/list ([n (in-range num-pages)])
           `(li ([class ,(cond [(= n page-num) "page-item active"] [else "page-item"])])
                (a ([class "page-link"]
                    [href ,(file/page file-name-base n)])
                   ,(number->string (add1 n)))))
       ,(cond [(= (add1 page-num) num-pages)
               `(li ([class "page-item disabled"])
                    (a ([class "page-link"] [href "#"]) 'rarr))]
              [else `(li ([class "page-item"])
                         (a ([class "page-link"]
                             [href ,(file/page file-name-base (add1 page-num))])
                            'rarr))])))


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
      ((or (page-renderer) default-page-renderer) this content-html))

    (define/public (render-content-html)
      ((or (post-renderer) default-post-renderer) this))

    (define/public (render-index-entry-html)
      ((or (index-entry-renderer) default-index-entry-renderer) this))

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

    (define/public (get-pagination-html)
      (xexprs->html (get-pagination-xexprs)))
    (define/public (get-pagination-xexprs)
      (bootstrap-prev/next-page (send (the-site) get-prev-post this)
                                (send (the-site) get-next-post this)))
    ))

(define (bootstrap-prev/next-page prev-page next-page)
  ;; Bootstrap 4 prev/next pagination
  (list
   `(div ([class "row justify-content-center"])
         (nav ([aria-label "Page Navigation"])
              (ul ([class "pagination"])
                  ,@(cond [prev-page
                           (list
                            `(li ([class "page-item"])
                                 (a ([class "page-link"] [aria-label "Previous"]
                                     [href ,(send prev-page get-link)])
                                    (span ([aria-hidden "true"])
                                          larr ,(send prev-page get-title-xexpr)))))]
                          [else null])
                  ,@(cond [next-page
                           (list
                            `(li ([class "page-item"])
                                 (a ([class "page-link"] [aria-label "Next"]
                                     [href ,(send next-page get-link)])
                                    (span ([aria-hidden "true"])
                                          rarr ,(send next-page get-title-xexpr)))))]
                          [else null]))))))


;; ============================================================

(module default-renderers racket/base
  (require web-server/templates
           racket/class
           racket/list
           racket/string
           net/url-structs
           "../config.rkt"
           "xexpr.rkt")
  (provide (all-defined-out))

  (define (default-index-entry-renderer post)
    (define site (the-site))
    (include-template "../template/index-entry.html"))
  (define (default-post-renderer post)
    (define site (the-site))
    (include-template "../template/post.html"))
  (define (default-page-renderer page content-html)
    (define site (the-site))
    (include-template "../template/page.html")))

(require (submod "." default-renderers))
